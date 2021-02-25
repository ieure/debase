;;; debase-bind.el --- Binding helpers               -*- lexical-binding: t; -*-

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

(cl-defmacro debase-flet-partial (bindings &rest body)
  "Like FLET, but binds ARGS partially applied to FUNC around BODY.

\(fn ((FUNC ARGS) ...) BODY)"
  (declare (indent 2))
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
                           (dbus-call-method ,@iargs))
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

(provide 'debase-bind)
;;; debase-bind.el ends here
