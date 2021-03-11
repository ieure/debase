;;; debase-gen--test.el --- Tests for DEBASE-GEN     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: extensions

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

;; Tests.

;;; Code:

(require 'ert)
(require 'debase-gen)

(defconst debase-gen--test--xml '(node nil "\n  "
                                       (interface
                                        ((name . "org.freedesktop.DBus.Properties"))
                                        "\n    "
                                        (method
                                         ((name . "Get"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "interface_name")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "property_name")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "v")
                                           (name . "value")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "GetAll"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "interface_name")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "a{sv}")
                                           (name . "properties")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "Set"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "interface_name")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "property_name")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "v")
                                           (name . "value")
                                           (direction . "in")))
                                         "\n    ")
                                        "\n    "
                                        (signal
                                         ((name . "PropertiesChanged"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "interface_name")))
                                         "\n      "
                                         (arg
                                          ((type . "a{sv}")
                                           (name . "changed_properties")))
                                         "\n      "
                                         (arg
                                          ((type . "as")
                                           (name . "invalidated_properties")))
                                         "\n    ")
                                        "\n  ")
                                       "\n  "
                                       (interface
                                        ((name . "org.freedesktop.DBus.Introspectable"))
                                        "\n    "
                                        (method
                                         ((name . "Introspect"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "xml_data")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n  ")
                                       "\n  "
                                       (interface
                                        ((name . "org.freedesktop.DBus.Peer"))
                                        "\n    "
                                        (method
                                         ((name . "Ping")))
                                        "\n    "
                                        (method
                                         ((name . "GetMachineId"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "machine_uuid")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n  ")
                                       "\n  "
                                       (interface
                                        ((name . "org.freedesktop.UDisks2.Manager"))
                                        "\n    "
                                        (method
                                         ((name . "CanFormat"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "type")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "(bs)")
                                           (name . "available")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "CanResize"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "type")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "(bts)")
                                           (name . "available")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "CanCheck"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "type")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "(bs)")
                                           (name . "available")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "CanRepair"))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "type")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "(bs)")
                                           (name . "available")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "LoopSetup"))
                                         "\n      "
                                         (arg
                                          ((type . "h")
                                           (name . "fd")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "a{sv}")
                                           (name . "options")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "o")
                                           (name . "resulting_device")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "MDRaidCreate"))
                                         "\n      "
                                         (arg
                                          ((type . "ao")
                                           (name . "blocks")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "level")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "s")
                                           (name . "name")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "t")
                                           (name . "chunk")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "a{sv}")
                                           (name . "options")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "o")
                                           (name . "resulting_array")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "EnableModules"))
                                         "\n      "
                                         (arg
                                          ((type . "b")
                                           (name . "enable")
                                           (direction . "in")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "GetBlockDevices"))
                                         "\n      "
                                         (arg
                                          ((type . "a{sv}")
                                           (name . "options")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "ao")
                                           (name . "block_objects")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (method
                                         ((name . "ResolveDevice"))
                                         "\n      "
                                         (arg
                                          ((type . "a{sv}")
                                           (name . "devspec")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "a{sv}")
                                           (name . "options")
                                           (direction . "in")))
                                         "\n      "
                                         (arg
                                          ((type . "ao")
                                           (name . "devices")
                                           (direction . "out")))
                                         "\n    ")
                                        "\n    "
                                        (property
                                         ((type . "s")
                                          (name . "Version")
                                          (access . "read")))
                                        "\n    "
                                        (property
                                         ((type . "as")
                                          (name . "SupportedFilesystems")
                                          (access . "read")))
                                        "\n  ")
                                       "\n"))

(ert-deftest debase-gen-method--test ()
  (let* ((cgen (debase-gen-class :class-name 'foo :interface "org.freedesktop.UDisks2.Manager"))
         (method '(method
                   ((name . "Get"))
                   "\n      "
                   (arg
                    ((type . "s")
                     (name . "interface_name")
                     (direction . "in")))
                   "\n      "
                   (arg
                    ((type . "s")
                     (name . "property_name")
                     (direction . "in")))
                   "\n      "
                   (arg
                    ((type . "v")
                     (name . "value")
                     (direction . "out")))
                   "\n    "))
         (gen (debase-gen-method :class-generator cgen :xml method)))

    (should (equal (debase-gen-method-->arglist gen)
                   '(interface_name property_name)))

    (should (equal (debase-gen-code gen)
                   '(cl-defmethod Get ((obj foo) interface_name property_name)
                      "Return the results of calling D-Bus interface \"org.freedesktop.UDisks2.Manager\" method \"Get\" on a `DEBASE-OBJECT' OBJ."
                      (dbus-call-method this "Get" interface_name property_name))))))

(ert-deftest debase-test--property-readable? ()
  (should (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "read")))))
  (should (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "readwrite")))))
  (should (null (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "write")))))))

(ert-deftest debase-test--property-writeable? ()
  (should (null (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "read"))))))
  (should (eq t (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "write"))))))
  (should (eq t (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "readwrite")))))))

(provide 'debase-gen--test)
;;; debase-gen--test.el ends here
