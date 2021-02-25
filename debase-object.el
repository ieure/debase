;;; debase-object.el --- D-Bus Objects               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: comm, hardware

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

;;

;;; Code:

(defclass debase-object* ()
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
   (xml :type cons
        :accessor debase-object--xml
        :documentation "XML representation of the D-Bus object."))
  :documentation "Base class for D-Bus objects.")

(cl-defmethod initialize-instance :after ((this debase-object*) &rest ignore)
  (with-slots (service) this
    (unless (slot-boundp this 'path)
      (oset this path (concat "/" (replace-regexp-in-string "\\." "/" service))))
    (unless (slot-boundp this 'interface)
      (oset this interface service))))

(cl-defmethod debase-object--xml ((this debase-object*))
  "Return XML representation of D-Bus object THIS."
  (debase-bind this
    (unless (slot-boundp this 'xml)
      (oset this xml (dbus-introspect-xml)))
    (oref this xml)))

(cl-defmethod debase-object--interfaces ((this debase-object*) &optional interfaces)
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

(cl-defmethod debase-object-call-method ((this debase-object*) method &rest args)
  "Call METHOD with ARGS on interface THIS."
  (debase-bind this
    (apply #'dbus-call-method method args)))

(cl-defmethod debase-object-get ((this debase-object*) property)
  "Get value of PROPERTY on interface THIS."
  (debase-bind this
    (dbus-get-property property)))

(cl-defmethod debase-object-set ((this debase-object*) property value)
  "Set value of PROPERTY to VALUE on interface THIS."
  (debase-bind this
    (dbus-set-property property value)))

(cl-defmethod debase-object-register ((this debase-object*) signal handler &rest args)
  "When SIGNAL fires on THIS, invoke HANDLER wtih ARGS. "
  (debase-bind this
    (apply #'dbus-register-signal signal handler args)))

(provide 'debase-object)
;;; debase-object.el ends here
