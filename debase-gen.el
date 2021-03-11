;;; debase-gen.el --- Debase code generation           -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: lisp, unix
;; URL: https://github.com/ieure/debase
;; Version: 0.7
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

;; DEBASE-GEN creates EIEIO classes for D-Bus interfaces.

;;; Code:

(require 'debase)
(require 'eieio)
(require 'gv)
(require 'dom)

 ;; Base class

(defclass debase-gen (debase-object)
  ((class-generator
    :initarg :class-generator
    :documentation "Reference to the class generator associated with this generator.")
   (mangle
    :initarg :mangle :initform #'debase-gen-mangle :type functionp
    :documentation "A function to produce Lisp names from D-Bus string identifiers."))
  :abstract t
  :documentation "Base class for Debase code generation.

Code generation produces an EIEIO class which extends `DEBASE-OBJECT'
and represents a D-Bus interface.
")


 ;; Method generation

(defclass debase-gen-method (debase-gen)
  ()
  :documentation "Class to generate D-Bus methods.")

(cl-defmethod debase-gen-method-->arglist ((this debase-gen-method))
  "Return the CL argument list for `debase-gen-method' THIS."
  (with-slots (xml mangle) this
    (cl-loop for child in (dom-non-text-children xml)
           with i = 0
           when (eq 'arg (dom-tag child))
           when (string= "in" (cdr (assoc 'direction (dom-attributes child))))
           collect (intern (or (funcall mangle (cdr (assoc 'name (dom-attributes child))))
                               (format "arg%d" i)))
           do (incf i))))

(cl-defmethod initialize-instance :after ((this debase-gen-method) &rest ignore)
  "Initialize `DEBASE-GEN-METHOD' instance THIS."
  (with-slots (xml) this (debase--assert xml 'method)))

(cl-defmethod debase-gen-code ((this debase-gen-method))
  "Return generated EIEIO method definition for THIS."
  (with-slots (class-generator xml mangle) this
    (with-slots (class-name) class-generator
      (let ((method-name (funcall mangle (cdr (assoc 'name (dom-attributes xml)))))
            (args (debase-gen-method-->arglist this)))
        `(cl-defmethod ,(intern method-name) ((obj ,class-name) ,@args)
           ,(format "Return the results of calling D-Bus interface \"%s\" method \"%s\" on a `DEBASE-OBJECT' OBJ."
                    (oref class-generator interface) method-name)
           (dbus-call-method this ,method-name ,@args))))))

 ;; Properties: slot definitions

(defclass debase-gen-slotdef (debase-gen)
  ()
  :documentation "Class to generate slot definitions for D-Bus properties.")

(cl-defmethod initialize-instance :after ((this debase-gen-slotdef) &rest igore)
  (with-slots (xml) this (debase--assert xml 'property)))

(cl-defmethod debase-gen-code ((this debase-gen-slotdef))
  "Return slot definition for property PROPERTY-DEF."
  (with-slots (xml mangle) this
    (let ((property-name (cdr (assoc 'name (dom-attributes xml)))))
      ;; Ignore the prefix for the property name's slot.
      `(,(intern (funcall mangle property-name))
        :type ,(debase--type->lisp (cdr (assoc 'type (dom-attributes xml))))
        ;; But use it for the accessor.
        :accessor ,(intern (funcall mangle property-name))))))

 ;; Properties: accessors

(defclass debase-gen-accessors (debase-gen)
  ()
  :documentation "Class to generate D-Bus properties.")

(cl-defmethod initialize-instance :after ((this debase-gen-accessors) &rest igore)
  (with-slots (xml) this (debase--assert xml 'property)))

(cl-defmethod debase-gen-accessors--access ((this debase-gen-accessors))
  "Return the access specification of D-Bus property THIS."
  (with-slots (xml) this (cdr (assoc 'access (dom-attributes xml)))))

(cl-defmethod debase-gen-accessors--readable? ((this debase-gen-accessors))
  "Returns non-NIL if property THIS is readable."
  (member (debase-gen-accessors--access this) '("read" "readwrite")))

(cl-defmethod debase-gen-accessors--writeable? ((this debase-gen-accessors))
  "Returns non-NIL if property THIS is writable."
  (member (debase-gen-accessors--access this) '("write" "readwrite")))

(cl-defmethod debase-gen-code ((this debase-gen-accessors))
  "Return the EIEIO method definition for THIS."
  (with-slots (xml mangle class-generator) this
    (with-slots (class-name) class-generator
    (let* ((helpers)
           (property-name (cdr (assoc 'name (dom-attributes xml))))
           (accessor (intern (funcall mangle property-name))))
      (when (debase-gen-accessors--writeable? this)
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
             ,(if (debase-gen-accessors--readable? this)
                  `(with-slots (bus service path interface) this
                     (dbus-get-property bus service path interface
                                        ,property-name))
                `(error "Property `%s' isn't readable" ,property-name)))
        (push helpers))
      helpers))))


 ;; Classes

(defun debase-gen-class--properties (interface-def)
  "Return properties for D-Bus interface INTERFACE-DEF."
  (debase--assert interface-def 'interface)
  (thread-first (lambda (child) (eq 'property (dom-tag child)))
    (cl-remove-if-not (dom-non-text-children interface-def))))

(defun debase-gen-class--methods (interface-def)
  "Return methods for D-Bus interface INTERFACE-DEF."
  (debase--assert interface-def 'interface)
  (thread-first (lambda (child) (eq 'method (dom-tag child)))
    (cl-remove-if-not (dom-non-text-children interface-def))))

(defclass debase-gen-class (debase-gen)
  ((class-name :initarg :class-name
               :type symbol
               :documentation "Name of the class to generate.")
   (slotdef-generator
    :initform #'debase-gen-slotdef
    :documentation "Constructor for class to generate slot definitions for properties.")
   (accessors-generator
    :initform #'debase-gen-accessors
    :documentation "Constructor for class to generate accessors for properties.")
   (property-mangle
    :initarg :property-mangle
    :documentation "A function to mangle property names.  Passed
    to SLOTDEF-GENERATOR and ACCESSORS-GENERATPR.  Defaults to
    the value of the MANGLE slot.")

   (method-generator
    :initform #'debase-gen-method
    :documentation "Constructor for class to generate methods.")
   (method-mangle
    :initarg :method-mangle
    :documentation "A function to mangle method names.  Defaults to the value of the MANGLE slot."))

  :documentation "A class which generates other classes.")

(cl-defmethod initialize-instance :after ((this debase-gen-class) &rest args)
  (unless (and (slot-boundp this 'class-name) (slot-value this 'class-name))
    (error "Must specify :CLASS-NAME"))
  (unless (and (slot-boundp this 'interface) (slot-value this 'interface))
    (error "Must specify :INTERFACE"))

  (with-slots (interface mangle) this
    (debase-object-assert-interface this interface)
    (unless (slot-boundp this 'property-mangle)
      (oset this property-mangle mangle))
    (unless (slot-boundp this 'method-mangle)
      (oset this method-mangle mangle))))

(cl-defmethod debase-gen-code ((this debase-gen-class))
  "Return definition of an EIEIO class to interface with D-Bus.

The return value is an expression for an EIEIO class, its generic
methods, and property accessors."
  (with-slots (interface property-mangle slotdef-generator accessors-generator
                         method-generator method-mangle) this
    (let* ((interface-def (car (debase-object--interfaces this interface)))
           (slotdef-generators
            (mapcar (lambda (xml) (funcall slotdef-generator :class-generator this
                                           :mangle property-mangle :xml xml))
                    (debase-gen-class--properties interface-def)))

           (accessors-generators
            (mapcar (lambda (xml) (funcall accessors-generator :class-generator this
                                           :mangle property-mangle :xml xml))
                    (debase-gen-class--properties interface-def)))

           (method-generators
            (mapcar (lambda (xml) (funcall method-generator :class-generator this
                                           :mangle method-mangle :xml xml))
                    (debase-gen-class--methods interface-def))))
      (with-slots (class-name) this
        `(prog1
             (defclass ,class-name (debase-object)
               ,(mapcar #'debase-gen-code slotdef-generators)
               :documentation ,(format "Debase interface class for D-Bus interface \"%s\"" interface))

           ;; Each generator returns a list of accessors, because
           ;; properties may have readers and writers.  Append all the
           ;; results together.
           ,@(apply #'append (mapcar #'debase-gen-code accessors-generators))
           ,@(mapcar #'debase-gen-code method-generators))))))


 ;; Name mangling

(defun debase-gen-mangle (dbus-name)
  "Mangles DBUS-NAME into something Lispier.

ex.  FooBARQuux -> foo-bar-quux."
  (let ((case-fold-search))
    (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" dbus-name))))

(cl-defun debase-gen-mangle-prefix (prefix dbus-name)
  "Mangle DBUS-NAME by adding PREFIX to it."
  (concat prefix dbus-name))

(provide 'debase-gen)
;;; debase-gen.el ends here
