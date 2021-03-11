;;; debase.el --- DBus convenience           -*- lexical-binding: t; -*-

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

;; lol

;;; Code:

(require 'dbus)

(defvar debase--ignore-interfaces
  '("org.freedesktop.DBus.Properties"
    "org.freedesktop.DBus.Introspectable"
    "org.freedesktop.DBus.Peer")
  "Interfaces to ignore.")

 ;; Helper functions

(defun debase--assert (xml? expected-type)
  "Assert that D-Bus XML? is of type EXPECTED-TYPE."
  (let ((actual-type (car xml?)))
    (assert (eq expected-type actual-type) "Expected type `%s', but got `%s'" expected-type actual-type)))

(defun debase-interface-name (interface-def)
     "Return the name of the interface in INTERFACE-DEF XML."
     (debase--assert interface-def 'interface)
     (cdr (assoc 'name (dom-attributes interface-def))))

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

 ;; Binding helpers

(cl-defmacro debase-flet-partial (bindings &rest body)
  "Like FLET, but binds ARGS partially applied to FUNC around BODY.

\(fn ((FUNC ARGS) ...) BODY)"
  (declare (indent 1))
  `(cl-flet ,(cl-loop for (func . args) in bindings
                      collect `(,func (apply-partially #',func ,@args)))
     ,@body))

(cl-defmacro debase-bind* ((bus service path &optional interface) &rest forms)
  "Bind D-Bus functions around FORMS, targeting BUS SERVICE PATH INTERFACE

Inside FORMS, calls to DBUS-INTROSPECT-XML, DBUS-CALL-METHOD,
DBUS-GET-PROPERTY, AND DBUS-SET-PROPERTY take their bus, service,
path, from this function's arguments.

\(fn (BUS SERVICE PATH &OPTIONAL :INTERFACE INTERFACE) &REST BODY)"
  (declare (indent 2))
  (let ((oargs (list bus service path))
        (iargs `(,bus ,service ,path ,(when interface interface))))
    `(debase-flet-partial ((dbus-introspect-xml ,@oargs)
                           (dbus-get-property ,@iargs)
                           (dbus-set-property ,@iargs)
                           (dbus-call-method ,@iargs)
                           (dbus-register-signal ,@iargs))
         ,@forms)))

(cl-defmacro debase-bind (debase-object &rest forms)
  "Bind FORMS so D-Bus methods implicitly target DEBASE-OBJECT.

Inside FORMS, calls to DBUS-INTROSPECT-XML, DBUS-CALL-METHOD,
DBUS-GET-PROPERTY, AND DBUS-SET-PROPERTY take their bus, service,
path, and interface arguments from DBUS-OBJECT, and don't require them
to be set.

\(fn (BUS SERVICE PATH &OPTIONAL :INTERFACE INTERFACE) &REST BODY)"
  (declare (indent 1))
  `(with-slots (bus service path interface) ,debase-object
     (debase-bind* (bus service path interface)
         ,@forms)))


 ;; Objects

(defclass debase-object ()
  ((bus :initarg :bus
        :type symbol
        :documentation "Bus the D-Bus service is on.")
   (service :initarg :service
            :type string
            :documentation "D-Bus service.")
   (path :initarg :path
         :type string
         :documentation "Path to D-Bus object.")
   (interface :initarg :interface
              :type string
              :accessor debase-object--interfaces
              :documentation "Interface this object binds to, if any.")
   (xml :initarg :xml
        :type cons
        :accessor debase-object--xml
        :documentation "XML representation of the D-Bus object."))
  :documentation "Base class for D-Bus objects.")

(cl-defmethod initialize-instance :after ((this debase-object) &rest ignore)
  "Initialize `DEBASE-OBJECT' instance THIS, ignoring args IGNORE."
  (with-slots (service) this
    (unless (slot-boundp this 'interface)
      (ignore-errors (oset this interface service)))
    (unless (slot-boundp this 'path)
      (ignore-errors
        (oset this path (concat "/" (replace-regexp-in-string
                                     "\\." "/" (oref this interface))))))))

(cl-defmethod debase-object-target ((this debase-object))
  "Return the target of `DEBASE-OBJECT' THIS.

Target is a list (BUS SERVICE PATH &OPTIONAL INTERFACE)."
  (with-slots (bus service path interface) this
    (list bus service path interface)))

(cl-defmethod debase-object--xml ((this debase-object))
  "Return XML representation of D-Bus object THIS."
  (unless (slot-boundp this 'xml)
    (oset this xml (debase-bind this (dbus-introspect-xml))))
  (oref this xml))

(cl-defmethod debase-object-assert-interface ((this debase-object) interface)
  "Assert that `DEBASE-OBJECT' THIS supports INTERFACE."
  (cl-assert (thread-last (debase-object--interfaces this :all)
               (mapcar #'debase-interface-name)
               (member interface))
             nil "Object `%s' doesn't implement interface `%s'" (type-of this) interface))

(cl-defmethod debase-object--interfaces ((this debase-object) &optional interfaces)
  "Return D-Bus interface definitions INTERFACES from XML.

If INTERFACES is nil, returns all interfaces except those in
`debase--ignore-interfaces'.

If INTERFACES is :all, returns all interfaces, even those in
`debase--ignore-interfaces'.

If INTERFACES is a list of strings, return interfaces matching them."
  (let ((xml (debase-object--xml this)))
    (debase--assert xml 'node)
    (cl-loop for child in (dom-non-text-children xml)
             when (eq 'interface (dom-tag child))
             when (cond
                   ((eq interfaces :all) t)
                   ((consp interfaces) (member (debase-interface-name child) interfaces))
                   (t (not (member (debase-interface-name child) debase--ignore-interfaces))))
             collect child)))

(cl-defmethod debase-object-call-method ((this debase-object) method &rest args)
  "Call METHOD with ARGS on interface THIS."
  (debase-bind this
    (if args (apply #'dbus-call-method method args)
      (funcall #'dbus-call-method method))))

(cl-defmethod debase-object-get ((this debase-object) property)
  "Get value of PROPERTY on interface THIS."
  (debase-bind this
    (dbus-get-property property)))

(cl-defmethod debase-object-set ((this debase-object) property value)
  "Set value of PROPERTY to VALUE on interface THIS."
  (debase-bind this
    (dbus-set-property property value)))

(cl-defmethod debase-object-register ((this debase-object) signal handler &rest args)
  "When SIGNAL fires on THIS, invoke HANDLER wtih ARGS. "
  (debase-bind this
    (if args (apply #'dbus-register-signal signal handler args)
      (funcall #'dbus-register-signal signal handler))))

(provide 'debase)
;;; debase.el ends here
