;;; debase-genclass.el --- DBus<->EIEIO interface           -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: lisp, unix
;; URL: https://github.com/ieure/debase
;; Version: 0.5
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lol

;;; Code:

(require 'eieio)
(require 'dbus)
(require 'gv)
(require 'dom)

(defconst debase--prefix "debase-"
  "Default prefix for Debase-generated symbols.")

 ;; Helpers

(defun debase--interface-properties (interface-def)
  "Return properties for D-Bus interface INTERFACE-DEF."
  (debase--assert interface-def 'interface)
  (thread-first (lambda (child) (eq 'property (dom-tag child)))
    (cl-remove-if-not (dom-non-text-children interface-def))))

(defun debase--interface-methods (interface-def)
  "Return methods for D-Bus interface INTERFACE-DEF."
  (debase--assert interface-def 'interface)
  (thread-first (lambda (child) (eq 'method (dom-tag child)))
    (cl-remove-if-not (dom-non-text-children interface-def))))


(cl-defun debase--interface-name->name (interface-name)
  "Return the EIEIO class name for D-Bus interface INTERFACE-DEF."
  (thread-last interface-name
    (assoc 'name)
    cdr
    (replace-regexp-in-string "^org\\.freedesktop\\." "")
    (replace-regexp-in-string "\\." "-")))



 ;; Base class

(defclass debase-gen* (debase-object)
  ((class-generator :initarg :class-generator)
   (mangle :initarg :mangle :initform #'identity :type functionp))
  :abstract t)

 ;; Methods

(defun debase-gen--method-def->arglist (method-def)
  "Return the CL argument list for METHOD-DEF."
  (debase--assert method-def 'method)
  (cl-loop for child in (dom-non-text-children method-def)
           with i = 0
           when (eq 'arg (dom-tag child))
           when (string= "in" (cdr (assoc 'direction (dom-attributes child))))
           collect (intern (or (cdr (assoc 'name (dom-attributes child)))
                               (format "arg%d" i)))
           do (incf i)))

(defclass debase-gen-method (debase-gen*)
  ())

(cl-defmethod initialize-instance :after ((this debase-gen-method) &rest igore)
  (with-slots (xml) this (debase--assert xml 'method)))

(cl-defmethod debase-gen-code ((this debase-gen-method))
  "Return the EIEIO method definition for THIS."
  (with-slots (class-generator xml mangle) this
    (with-slots (class-name) class-generator
      (let ((method-name (cdr (assoc 'name (dom-attributes xml))))
            (args (debase-gen--method-def->arglist xml)))
        `(cl-defmethod ,(intern (funcall mangle method-name)) ((obj ,class-name) ,@args)
           ,(format "Return the results of calling D-Bus interface \"%s\" method \"%s\" on a `DEBASE-OBJECT' OBJ."
                    (oref class-generator interface) method-name)
           (dbus-call-method this ,method-name ,@args))))))

 ;; Properties

(defun debase--property-readable? (property-def)
  "Is the property specified in PROPERTY-DEF readable?"
  (debase--assert property-def 'property)
  (let ((access (cdr (assoc 'access (dom-attributes property-def)))))
    (or (string= access "read")
        (string= access "readwrite"))))

(defun debase--property-writeable? (property-def)
  "Is the property specified in PROPERTY-DEF writeable?"
  (debase--assert property-def 'property)
  (let ((access (cdr (assoc 'access (dom-attributes property-def)))))
    (or (string= access "write")
        (string= access "readwrite"))))

(defun debase--type->lisp (type)
  ;; https://dbus.freedesktop.org/doc/dbus-specification.html
  (pcase type
    ("b" 'boolean)
    ("s" 'string)                       ; string
    ("o" 'string)                       ; object path
    ("g" 'string)                       ; type signature
    ((or "y" "n" "q" "i" "u" "x" "t") 'string)
    ("d" 'float)
    (_ t)))

(defun debase--type->hint (type)
  ;; https://dbus.freedesktop.org/doc/dbus-specification.html
  (pcase type
    ("a" '(:array))
    ("a{sv}" `(:array :signature "{sv}"))))

(defclass debase-gen-property (debase-gen*) nil)

(cl-defmethod initialize-instance :after ((this debase-gen-property) &rest igore)
  (with-slots (xml) this (debase--assert xml 'property)))

(cl-defmethod debase-gen-slotdef ((this debase-gen-property))
  "Return slot definition for property PROPERTY-DEF."
  (with-slots (xml mangle) this
    (let ((property-name (cdr (assoc 'name (dom-attributes xml)))))
      ;; Ignore the prefix for the property name's slot.
      `(,(intern (funcall mangle property-name))
        :type ,(debase--type->lisp (cdr (assoc 'type (dom-attributes xml))))
        ;; But use it for the accessor.
        :accessor ,(intern (funcall mangle property-name))))))

(cl-defmethod debase-gen-code ((this debase-gen-property))
  "Return the EIEIO method definition for THIS."
  (with-slots (xml mangle class-generator) this
    (with-slots (class-name) class-generator
    (let* ((helpers)
           (property-name (cdr (assoc 'name (dom-attributes xml))))
           (accessor (intern (funcall mangle property-name))))
      (when (debase--property-writeable? xml)
        ;; Clear the setter, if there is one, otherwise `gv-setter' complains.
        (put accessor 'gv-expander nil)
        (thread-first
            `(gv-define-setter ,accessor (val obj)
               (backquote (with-slots (bus service path interface) ,',obj
                            (dbus-set-property bus service path interface ,property-name ,',val)
                            (oset ,',obj ,accessor ,',val))))
          (push helpers)))

      (thread-first
          `(cl-defmethod ,accessor ((this ,class-name))
             ,(if (debase--property-readable? xml)
                  `(with-slots (bus service path interface) this
                     (dbus-get-property bus service path interface
                                        ,property-name))
                `(error "Property `%s' isn't readable" ,property-name)))
        (push helpers))
      helpers))))


 ;; Classes

(defclass debase-gen-class (debase-object)
  ((class-name :initarg :class-name
               :type symbol
               :documentation "Name of the class to generate.")
   (method-prefix :initarg :method-prefix
                  :type '(or symbol string)
                  :documentation "Prefix to apply to methods.")
   (slot-prefix :initarg :slot-prefix
                :type '(or symbol string)
                :documtentation "Prefix to apply to slots."))
  :documentation "A class which generate other classes.")

(cl-defmethod initialize-instance :after ((this debase-class-generator) &rest args)
  (unless (and (slot-boundp this 'class-name) (slot-value this 'class-name))
    (set-slot-value this 'class-name (debase--interface->name interface-def))))

(cl-defmethod debase-gen-code ((this debase-gen-class))
  "Return definition of an EIEIO class to interface with D-Bus.

The is a list of expressions for an EIEIO class, its generic
methods, and property accessors."
  (let* ((interface-name (oref this interface))
         (interface-def (car (debase-object--interfaces this interface-name)))
         (properties (mapcar (lambda (xml) (debase-gen-property :class-generator this :xml xml))
                             (debase--interface-properties interface-def)))
         (methods (mapcar (lambda (xml) (debase-gen-method :class-generator this :xml xml))
                          (debase--interface-methods interface-def))))

    (with-slots (class-name) this
      `(prog1
           (defclass ,class-name (debase-object)
             ,@(mapcar #'debase-gen-slotdef properties)
             :documentation ,(format "Debase interface class for D-Bus interface \"%s\"" interface-name))

         ,@(cl-loop for p in properties append (debase-gen-code p))
         ,@(mapcar #'debase-gen-code methods)))))

 ;; Name mangling

(defun debase-basic-mangler (dbus-name)
  "Mangles DBUS-NAME into something Lispier.

ex.  FooBARQuux -> foo-bar-quux."
  (let ((case-fold-search))
    (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" dbus-name))))

(cl-defun debase-prefix-mangler (prefix dbus-name)
  (concat prefix dbus-name))

(cl-defun debase--name-mangle (dbus-name &key (prefix "debase-") )
  "Mangle DBUS-NAME into something Lispier.

   ex. FooBARQuux -> foo-bar-quux."

  (thread-last dbus-name
    debase-basic-mangler
    (debase-prefix-mangler prefix)))


 ;; Interface handling

(defun debase-interface-names (xml)
  "Return a list of supported D-Bus interface names in XML."
  (debase--assert xml 'node)
  (cl-loop for child in (dom-non-text-children xml)
           when (eq 'interface (dom-tag child))
           collect (debase-interface-name child)))''

(defun debase--object-interfaces (xml &optional interfaces)
  "Return D-Bus interface definitions INTERFACES from XML.

If INTERFACES is nil, returns all interfaces except those in
`debase--ignore-interfaces'.

If INTERFACES is :all, returns all interfaces, even those in
`debase--ignore-interfaces'.

If INTERFACES is a list of strings, return interfaces matching them.
"
  (debase--assert xml 'node)
  (cl-loop for interface in
           (let ((object-interfaces (debase-interface-names xml)))
             (cond
              ((eq interfaces :all) object-interfaces)
              ((consp interfaces) interfaces)
              (t (cl-loop for interface in object-interfaces
                 unless (member interface debase--ignore-interfaces)
                 collect interface))))

           collect (debase--interface xml interface)))

(provide 'debase-genclass)
;;; debase.el ends here
