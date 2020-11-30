;;; debase.el --- DBus<->EIEIO interface           -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020  Ian Eure

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

(require 'dbus)
(require 'gv)
(require 'dom)

(defconst debase--prefix "debase-"
  "Default prefix for Debase-generated symbols.")

(defvar debase--ignore-interfaces
  '("org.freedesktop.DBus.Properties"
    "org.freedesktop.DBus.Introspectable"
    "org.freedesktop.DBus.Peer")
  "Interfaces to ignore.")

(defun debase--assert (xml? expected-type)
  "Assert that D-Bus XML? is of type EXPECTED-TYPE."
  (let ((actual-type (car xml?)))
    (assert (eq expected-type actual-type) "Expected type `%s', but got `%s'" expected-type actual-type)))

 ;; Name mangling

(defun debase-basic-mangler (dbus-name)
  "Mangles DBUS-NAME into something Lispier.

ex. FooBARQuux -> foo-bar-quux."
  (let ((case-fold-search))
    (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" dbus-name))))

(cl-defun debase-prefix-mangler (prefix dbus-name)
  (concat prefix dbus-name))

(defun debase--name-mangle (dbus-name &optional options)
  "Mangle DBUS-NAME into something Lispier.

   ex. FooBARQuux -> foo-bar-quux."

  (let ((prefix (cond ((memq :prefix options) (plist-get options :prefix))
                      (t debase--prefix))))
    (thread-last dbus-name
      debase-basic-mangler
      (debase-prefix-mangler prefix))))


 ;; Interface handling

(defun debase-interface-names (xml)
  "Return a list of supported D-Bus interface names in XML."
  (debase--assert xml 'node)
  (cl-loop for child in (dom-non-text-children xml)
           when (eq 'interface (dom-tag child))
           collect (debase-interface-name child)))''

(defun debase-interface-name (interface-def)
  "Return the name of the interface in INTERFACE-DEF XML."
  (debase--assert xml 'interface)
  (cdr (assoc 'name (dom-attributes interface-def))))

(defun debase--interface (xml interface-name)
  "Return definition of interface INTERFACE-NAME from introspected XML."
  (debase--assert xml 'node)
  (cl-loop for child in (dom-non-text-children xml)
           when (and (eq 'interface (dom-tag child))
                     (string= interface-name (cdr (assoc 'name (dom-attributes child)))))
           return child))

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

(defun debase--interface->name (interface-def &optional options)
  "Return the EIEIO class name for D-Bus interface INTERFACE-DEF."
  (debase--assert interface-def 'interface)
  (debase--name-mangle
   (thread-last (dom-attributes interface-def)
     (assoc 'name)
     cdr
     (replace-regexp-in-string "^org\\.freedesktop\\." "")
     (replace-regexp-in-string "\\." "-"))
   options))

 ;; Method handling

(defun debase--interface-method->arglist (method-def)
  "Return the CL argument list for METHOD-DEF."
  (debase--assert method-def 'method)
  (cl-loop for child in (dom-non-text-children method-def)
           with i = 0
           when (eq 'arg (dom-tag child))
           when (string= "in" (cdr (assoc 'direction (dom-attributes child))))
           collect (intern (or (cdr (assoc 'name (dom-attributes child)))
                               (format "arg%d" i)))
           do (incf i)))

(defun debase--interface-method->defmethod (options class-name interface-name method-def)
  "Return the EIEIO method definition for method METHOD-DEF.

   The method will be dispatched on EIEIO class CLASS-NAME."
  (debase--assert method-def 'method)
  (let ((method-name (cdr (assoc 'name (dom-attributes method-def))))
        (args (debase--interface-method->arglist method-def)))
    `(cl-defmethod ,(intern (debase--name-mangle method-name options)) ((obj ,class-name) ,@args)
       ,(format "Return the results of calling D-Bus method \"%s\" on an object supporting interface \"%s\""
                method-name interface-name)
       (with-slots (bus service path) obj
         (dbus-call-method bus service path ,interface-name
                           ,method-name
                           ,@args)))))

(defun debase--interface->methods (options class-name interface-def)
  "Return EIEIO methods for INTERFACE-DEF, bound to CLASS-NAME."
  (debase--assert interface-def 'interface)
  (mapcar (apply-partially #'debase--interface-method->defmethod options class-name
                           (cdr (assoc 'name (dom-attributes interface-def))))
          (debase--interface-methods interface-def)))

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

(defun debase--property->slotdef (property-def &optional options)
  "Return slot definition for property PROPERTY-DEF."
  (debase--assert property-def 'property)
  (let ((property-name (cdr (assoc 'name (dom-attributes property-def)))))
    ;; Ignore the prefix for the property name's slot.
    `(,(intern (debase--name-mangle property-name))
      :type t                           ; lol ugh FIXME
      ;; But use it for the accessor.
      :accessor ,(intern (debase--name-mangle (concat "prop-" property-name) options)))))

(defun debase--property->dbus-accessor (class-name interface accessor-symbol property-name)
  "Return a default (D-Bus) property accessor.

Creates a generic method template named ACCESSOR-SYMBOL, which
returns the value of PROPERTY-NAME, and binds it to the
CLASS-NAME class."
  `(cl-defmethod ,accessor-symbol ((this ,class-name))
     (with-slots (bus service path) this
       (dbus-get-property bus service path ,interface
                          ,property-name))))

(defun debase--property->error-accessor (class-name interface accessor-symbol property-name)
  "Return an error property accessor.

This is used for write-only D-Bus propertues.

Creates a generic method template named ACCESSOR-SYMBOL, which
attempts to access PROPERTY-NAME, but returns an error; and binds
it to the CLASS-NAME class."
  `(cl-defmethod ,accessor-symbol ((this ,class-name))
     (error "Property `%s' isn't readable" ,property-name)))

(defun debase--property->slot (class-name interface property-def &optional options)
  "Return slot def & helpers for D-Bus PROPERTY-DEF in CLASS-NAME.

   Returns a list of (SLOT-DEF [HELPER...])"

  (debase--assert property-def 'property)

  ;; There's always a slot definition.
  (let* ((slot-and-helpers (list (debase--property->slotdef property-def options)))
         (slotname (caar slot-and-helpers))
         (accessor (plist-get (cdar (last slot-and-helpers)) :accessor))
         (property-name (cdr (assoc 'name (dom-attributes property-def)))))

    ;; There's always a reader, but it might be one that just errors.
    (thread-first
        (funcall (if (debase--property-readable? property-def)
                     'debase--property->dbus-accessor
                   'debase--property->error-accessor)
                 class-name
                 interface
                 accessor
                 property-name)
      (push slot-and-helpers))

    ;; There's sometimes a writer.
    (when (debase--property-writeable? property-def)
      ;; Clear the setter, if there is one, otherwise `gv-setter' complains.
      (put accessor 'gv-expander nil)
      (thread-first
          `(gv-define-setter ,accessor (val obj)
             (backquote (with-slots (bus service path interface) ,',obj
                          (dbus-set-property bus service path interface ,property-name ,',val)
                          (oset ,',obj ,slotname ,',val))))
        (push slot-and-helpers)))

    (reverse slot-and-helpers)))

 ;; Base classes

(defclass debase--dbus ()
  ((interface :type string
              :documentation "D-Bus interface this class implements."))
  :abstract t
  :documentation "Base class for D-Bus interface classes.")

(defclass debase--dbus-object ()
  ((bus :initarg :bus
        :type symbol
        :documentation "Bus the D-Bus service is on.")
   (service :initarg :service
            :type string
            :documentation "D-Bus service.")
   (path :initarg :path
         :type string
         :documentation "Path to D-Bus object.")
   (interfaces :type list
               :documentation "Interfaces this object implements."))
  :abstract t
  :documentation "Base class for D-Bus objects.")


 ;; Class creation

(defun debase--create-interface (interface-def &rest options)
  "Define an EIEIO class and methods for D-Bus interface INTERFACE-DEF.

OPTIONS is an alist supporting the following keywords:

  :prefix - Generated symbols will be prefixed by this string, instead of `debase-'.
  :name - The generated class will be named this."
  (let* ((interface-name (debase-interface-name interface-def))
         (class-name (pcase (or (plist-get options :name)
                                (debase--interface->name interface-def options))
                       ((and s (pred symbolp)) s)
                       ((and s (pred stringp)) (intern s))))
         (properties (debase--interface-properties interface-def))
         (methods (debase--interface-methods interface-def))
         (slots-and-helpers (mapcar (lambda (prop-def) (debase--property->slot class-name interface-name prop-def options)) properties)))
    `(prog1
         (defclass ,class-name
           (debase--dbus)             ; Inherit from this base

           ((interface :initform ,interface-name)
            ,@(mapcar #'car slots-and-helpers))

           :documentation ,(format "Debase class corresponding to D-Bus interface \"%s\"" interface-name))
       ;; TODO:
       ;; - Maybe(?) override constructor with one that fetches property values.
       ;; - Override constructor with one that subscribes to property updates.

       ;; Interface methods
       ,@(debase--interface->methods options class-name interface-def)

       ;; Slot helpers -- getters and setf support.
       ,@(apply #'append (mapcar #'cdr slots-and-helpers)))))

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

(defun debase-define-composite-class* (class-name bus service path &rest options)
  "Create a class representing D-Bus SERVICE, on BUS, at PATH."
  (let* ((interfaces (debase--object-interfaces (dbus-introspect-xml bus service path)
                                                (plist-get options :interfaces)))
         (interface-names (mapcar 'debase-interface-name interfaces))
         (classes (mapcar (lambda (interface-def)
                            (apply #'debase--create-interface interface-def (plist-put options :name nil)))
                          interfaces))
         (child-class-names (cons 'debase--dbus-object (mapcar #'cadadr classes)))
         (class-name (or class-name (gensym (concat (or (plist-get options :prefix) debase--prefix) "composite--" (mapconcat #'symbol-name classes "&"))))))

    `(progn
       ,@(car (mapcar #'cdr classes))
       (defclass ,class-name ,child-class-names
             ((interfaces :initform ,interface-names))
             :documentation ,(concat
                              "Debase composite class, representing an object with the following interfaces:\n\n - "
                              (string-join interface-names "\n - "))))))

(defun debase-define-composite-class (class-name bus service path &rest options)
  (eval (apply #'debase-define-class* class-name bus service path options)))

(defun debase-define-simple-class* (class-name bus service path interface &rest options)
  "Create a class representing D-Bus SERVICE, on BUS, at PATH."
  (apply #'debase--create-interface
         (debase--interface (dbus-introspect-xml bus service path) interface)
         options))

(defun debase-make-instance (bus service path &rest options)
  "Create an instance of D-Bus SERVICE, on BUS, at PATH."
  (make-instance
   (apply 'debase-make-class (plist-get options :name) bus service path options)
   :bus bus
   :service service
   :path path))

(provide 'debase)
;;; debase.el ends here
