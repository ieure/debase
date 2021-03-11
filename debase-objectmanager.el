;;; debase-objectmanager.el --- D-Bus ObjectManager  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ian Eure

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

;; DEBASE-OBJECTMANAGER implements the
;; org.freedesktop.DBus.ObjectManager D-Bus interface.

;;; Code:

(require 'debase)

(defconst debase-objectmanager--interface "org.freedesktop.DBus.ObjectManager"
  "The D-Bus interface `DEBASE-OBJECTMANAGER' uses.")

(defclass debase-objectmanager (debase-object)
  ((managed-objects
    :type cons
    :documentation "List of objects this object manages.")
   (-objectmanager-on-change
    :initform '()
    :documentation "List of hook functions to call when MANAGED-OBJECTS changes.")
   (-objectmanager-signals
    :type cons
    :documentation "D-Bus signals this object has registered."))
  :documentation "A class representing the D-Bus ObjectManager interface.")

(defmethod debase-objectmanager-onchange ((this debase-objectmanager) f)
  "When the state of objects manged by THIS changes, call function F."
  (with-slots (-objectmanager-on-change) this
    (add-to-list '-objectmanager-on-change f)))

(defmethod debase-objectmanager--changed ((this debase-objectmanager) &rest ignore)
  "Refresh objects managed by THIS.

Calls hook functions in -OBJECTMANAGER-ON-CHANGE."
  (with-slots (managed-objects -objectmanager-on-change) this
    (setf managed-objects
          (debase-object-call-method
           (clone this :interface debase-objectmanager--interface) "GetManagedObjects"))
    (dolist (f -objectmanager-on-change)
      (funcall f))))

(defmethod initialize-instance :after ((this debase-objectmanager) &rest ignore)
  "Initialize instance THIS by populating managed objects."
  (unless (slot-boundp this 'interface)
    (error "Must target `%s' interface!" debase-objectmanager--interface))
  (debase-object-assert-interface this debase-objectmanager--interface)
  (debase-objectmanager--changed this))

(defmethod debase-objectmanager-start ((this debase-objectmanager))
  "Begin listening for updates to managed objects on THIS."
  (with-slots (-objectmanager-signals) this
    (let ((om (clone this :interface debase-objectmanager--interface)))
      (setf -objectmanager-signals
            (cl-loop for signal in '("InterfacesAdded" "InterfacesRemoved")
                     collect (debase-object-register
                              om signal
                              (apply-partially #'debase-objectmanager--changed this)))))))

(defmethod debase-objectmanager-started? ((this debase-objectmanager))
  "Returns non-NIL if ObjectManager THIS is listening for changes."
  (and (slot-boundp this '-added-signal) (slot-boundp this '-removed-signal)
       (with-slots (-added-signal -removed-signal) this
         -added-signal -removed-signal t)))

(defmethod debase-objectmanager-stop ((this debase-objectmanager))
  "Stop listening for updates to managed objects on THIS."
  (with-slots (-objectmanager-signals) this
    (mapc #'dbus-unregister-object -objectmanager-signals)
    (setf -objectmanager-signals nil)))

(provide 'debase-objectmanager)
;;; debase-objectmanager.el ends here
