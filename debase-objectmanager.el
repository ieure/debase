;;; debase-objectmanager.el --- D-Bus ObjectManager  -*- lexical-binding: t; -*-

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


(defclass debase-objectmanager (debase-object)
  ((managed-objects :type cons)
   (-added-signal)
   (-removed-signal))
  :documentation "A class representing the D-Bus ObjectManager interface.")

(defmethod debase-objectmanager--refresh ((this debase-objectmanager) &rest ignore)
  "Refresh objects managed by THIS."
  (with-slots (managed-objects) this
    (setf managed-objects (debase-object-call-method this "GetManagedObjects"))))

(defmethod initialize-instance :after ((this debase-objectmanager) &rest ignore)
  "Initialize instance THIS by populating managed objects."
  (debase-objectmanager--refresh this))

(defmethod debase-objectmanager-start ((this debase-objectmanager))
  "Begin listening for updates to managed objects on THIS."
  (with-slots (-added-signal -removed-signal) this
    (setf -added-signal (debase-object-register-signal this "InterfacesAdded" 'debase-objectmanager--refresh this)
          -removed-signal (debase-object-register-signal this "InterfacesRemoved" 'debase-objectmanager--refresh this))))

(defmethod debase-objectmanager-stop ((this debase-objectmanager))
  "Stop listening for updates to managed objects on THIS."
  (with-slots (-added-signal -removed-signal) this
    (dolist (signal (list -added-signal -removed-signal))
      (dbus-unregister-object signal))))

(provide 'debase-objectmanager)
;;; debase-objectmanager.el ends here
