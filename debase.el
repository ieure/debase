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

(require 'eieio)
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

(cl-defun debase--interface->name (interface-def &key prefix)
  "Return the EIEIO class name for D-Bus interface INTERFACE-DEF."
  (debase--assert interface-def 'interface)
  (debase--name-mangle
   (thread-last (dom-attributes interface-def)
     (assoc 'name)
     cdr
     (replace-regexp-in-string "^org\\.freedesktop\\." "")
     (replace-regexp-in-string "\\." "-"))
   :prefix prefix))

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

(cl-defun debase--interface-method->defmethod (method-def &key class-name interface-name prefix)
  "Return the EIEIO method definition for method METHOD-DEF.

   The method will be dispatched on EIEIO class CLASS-NAME."
  (debase--assert method-def 'method)
  (let ((method-name (cdr (assoc 'name (dom-attributes method-def))))
        (args (debase--interface-method->arglist method-def)))
    `(cl-defmethod ,(intern (debase--name-mangle method-name :prefix prefix)) ((obj ,class-name) ,@args)
       ,(format "Return the results of calling D-Bus method \"%s\" on an object supporting interface \"%s\""
                method-name interface-name)
       (with-slots (bus service path) obj
         (dbus-call-method bus service path ,interface-name
                           ,method-name
                           ,@args)))))

(cl-defun debase--interface->methods (interface-def &key class-name prefix)
  "Return EIEIO methods for INTERFACE-DEF, bound to CLASS-NAME."
  (debase--assert interface-def 'interface)
  (mapcar (lambda (method-def)
            (debase--interface-method->defmethod
             method-def
             :class-name class-name
             :interface-name (debase-interface-name interface-def)
             :prefix prefix))
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

(cl-defun debase--property->slotdef (property-def &key prefix)
  "Return slot definition for property PROPERTY-DEF."
  (debase--assert property-def 'property)
  (let ((property-name (cdr (assoc 'name (dom-attributes property-def)))))
    ;; Ignore the prefix for the property name's slot.
    `(,(intern (debase--name-mangle property-name))
      :type ,(debase--type->lisp (cdr (assoc 'type (dom-attributes property-def))))
      ;; But use it for the accessor.
      :accessor ,(intern (debase--name-mangle (concat "prop-" property-name) :prefix prefix)))))

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

(cl-defun debase--property->slot (property-def class-name interface &key prefix)
  "Return slot def & helpers for D-Bus PROPERTY-DEF in CLASS-NAME.

   Returns a list of (SLOT-DEF [HELPER...])"

  (debase--assert property-def 'property)

  ;; There's always a slot definition.
  (let* ((slot-and-helpers (list (debase--property->slotdef property-def :prefix prefix)))
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

(cl-defun debase--interface->class&methods (interface-def &key class-name prefix)
  "Define an EIEIO class and methods for D-Bus interface INTERFACE-DEF.

  :prefix - Generated symbols will be prefixed by this string, instead of `debase-'.
  :class-name - The generated class will be named this."
  (let* ((interface-name (debase-interface-name interface-def))
         (class-name (pcase (or class-name
                                (debase--interface->name interface-def :prefix prefix))
                       ((and s (pred symbolp)) s)
                       ((and s (pred stringp)) (intern s))))
         (properties (debase--interface-properties interface-def))
         (methods (debase--interface-methods interface-def))
         (slots-and-helpers (mapcar (lambda (prop-def) (debase--property->slot prop-def class-name interface-name :prefix prefix)) properties)))
    `(prog1
         (defclass ,class-name
           (debase--dbus)             ; Inherit from this base

           ((interface :initform ,interface-name)
            ,@(mapcar #'car slots-and-helpers))

           :abstract t
           :documentation ,(format "Debase interface class for D-Bus interface \"%s\"" interface-name))
       ;; TODO:
       ;; - Maybe(?) override constructor with one that fetches property values.
       ;; - Override constructor with one that subscribes to property updates.

       ;; Interface methods
       ,@(debase--interface->methods interface-def :class-name class-name :prefix prefix)

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

(cl-defun debase-define-class* (class-name &key spec node-xml (prefix "debase-") interfaces)
  "Return expression for CLASS-NAME, to interface with D-Bus.

The expression includes one or more EIEIO classes, generic methods,
and property accessors.

The D-Bus node specification may be supplied with keyword argument
:NODE-XML, which is a value returned from `dbus-introspect-xml'.  This
value may be saved in a source file.

Alternately, keyword argument :SPEC may be supplied, consisting of:

    (BUS SERVICE PATH)

When :SPEC is provided, `dbus-introspect-xml' will be called with the
supplied values.  This requires an active D-Bus session.

Keyword :INTERFACES is provided to `dbus--object-interfaces'; see its
documentation for more detail.

At least two classes will be created:

 - One or more abstract classes, corresponding to a single D-Bus
   interface, whose names are generated from the D-Bus interface
   names and :PREFIX.
 - A concrete class which offers no functionality on its own, but
   inherits from `debase--dbus-object' and all the abstract classes.
   This class will always be named CLASS-NAME.

With keyword argument :PREFIX, a string, apply this prefix to all
methods, property accessor functions, and complex classes.  Since
CLASS-NAME is explicitly specified, it will never have a prefix
applied.
"

  (unless (or spec node-xml)
    (error "Must supply only one of SPEC or NODE-XML"))
  (when node-xml
    (debase--assert node-xml 'node))

  (let* ((node-xml (or node-xml (apply #'dbus-introspect-xml spec)))
         (interfaces (debase--object-interfaces node-xml interfaces))
         (interface-names (mapcar 'debase-interface-name interfaces))
         (classes (mapcar (lambda (interface-def)
                            (debase--interface->class&methods interface-def :prefix prefix))
                          interfaces))
         (child-class-names (cons 'debase--dbus-object (mapcar #'cadadr classes)))
         (class-name (or class-name (gensym (concat prefix "composite--" (mapconcat #'symbol-name classes "&")))))
         (slots ))

    `(progn
       ,@(cl-loop for classdef in classes append (cdr classdef))
       (defclass ,class-name ,child-class-names
         ((interfaces :initform ,interface-names)
          ,@(when spec
              (cl-destructuring-bind (bus service path) spec
                `((bus :initform ,bus)
                  (service :initform ,service)
                  (path :initform ,path)))))

         :documentation ,(concat
                          "Debase composite class, representing an object with the following interfaces:\n\n - "
                          (string-join interface-names "\n - "))))))

(cl-defmacro debase-define-class (class-name &key spec node-xml (prefix "debase-") interfaces)
  "Define CLASS-NAME, to interface with D-Bus.

The D-Bus node specification may be supplied with keyword argument
:NODE-XML, which is a value returned from `dbus-introspect-xml'.  This
value may be saved in a source file.

Alternately, keyword argument :SPEC may be supplied, consisting of:

    (BUS SERVICE PATH)

When :SPEC is provided, `dbus-introspect-xml' will be called with the
supplied values.  This requires an active D-Bus session.

Keyword :INTERFACES is provided to `dbus--object-interfaces'; see its
documentation for more detail.

At least two classes will be created:

 - One or more abstract classes, corresponding to a single D-Bus
   interface, whose names are generated from the D-Bus interface
   names and :PREFIX.
 - A concrete class which offers no functionality on its own, but
   inherits from `debase--dbus-object' and all the abstract classes.
   This class will always be named CLASS-NAME.

With keyword argument :PREFIX, a string, apply this prefix to all
methods, property accessor functions, and complex classes.  Since
CLASS-NAME is explicitly specified, it will never have a prefix
applied.
"
  (declare (indent defun))
  `(eval ,(debase-define-class* class-name :spec spec :node-xml (eval node-xml) :prefix prefix :interfaces interfaces)))

(cl-defun debase-make-instance (bus service path &key class-name prefix interfaces)
  "Create an instance of D-Bus SERVICE, on BUS, at PATH."
  (make-instance
   (debase-define-class (or class-name (gensym "debase"))
                        :spec `(,bus ,service ,path)
                        :prefix prefix
                        :interfaces interfaces)
   :bus bus
   :service service
   :path path))

(provide 'debase)
;;; debase.el ends here
