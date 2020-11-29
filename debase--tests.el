;;; debase--tests.el --- Tests for debase            -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: internal

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

(require 'debase)
(require 'ert)

(ert-deftest debase-test--property-readable? ()
  (should (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "read")))))
  (should (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "readwrite")))))
  (should (null (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "write")))))))


(ert-deftest debase-test--property-writeable? ()
  (should (null (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "read"))))))
  (should (eq t (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "write"))))))
  (should (eq t (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "readwrite")))))))

(ert-deftest debase-test--name-mangle ()
  (should (string= "debase-foo-bar" (debase--name-mangle "FooBar")))
  (should (string= "foo-bar" (debase--name-mangle  "FooBar" '(:prefix nil))))
  (should (string= "msvfoo-bar" (debase--name-mangle "FooBar" '(:prefix "msv"))))
  (should (string= "msv-foo-bar" (debase--name-mangle "FooBar" '(:prefix "msv-")))))

(ert-deftest debase-test--interface-names ()
  (should (equal '("org.freedesktop.DBus.Properties"
                   "org.freedesktop.DBus.Introspectable"
                   "org.freedesktop.DBus.Peer"
                   "org.freedesktop.UDisks2.Manager")
                 (debase-interface-names '(node nil "\n  "
                                                (interface ((name . "org.freedesktop.DBus.Properties")))
                                                (interface ((name . "org.freedesktop.DBus.Introspectable")))
                                                (interface ((name . "org.freedesktop.DBus.Peer")))
                                                (interface ((name . "org.freedesktop.UDisks2.Manager"))))))))

(ert-deftest debase-test--interface-name ()
  (should (string= "org.freedesktop.UDisks2.Manager" (debase-interface-name '(interface ((name . "org.freedesktop.UDisks2.Manager"))))))
  (should (string= "org.freedesktop.DBus.Peer" (debase-interface-name '(interface ((name . "org.freedesktop.DBus.Peer")))))))

(ert-deftest debase-test--interface ()
  (let ((xml '(node nil (interface
                         ((name . "org.freedesktop.DBus.Introspectable"))
                         (method ((name . "Introspect"))
                                 (arg ((type . "s")
                                       (name . "xml_data")
                                       (direction . "out")))))
                    (interface
                     ((name . "org.freedesktop.DBus.Introspectable"))
                     (method ((name . "Introspect"))
                             (arg ((type . "s")
                                   (name . "xml_data")
                                   (direction . "out"))))))))
    (should (equal 'interface (car (debase--interface xml "org.freedesktop.DBus.Introspectable"))))))

(ert-deftest debase-test--interface->name ()
  (should (string= "debase-network-manager"
                   (debase--interface->name '(interface
                                               ((name . "org.freedesktop.NetworkManager"))))))

  (should (string= "nm-network-manager"
                   (debase--interface->name '(interface
                                               ((name . "org.freedesktop.NetworkManager"))) '(:prefix "nm-")))))

(ert-deftest debase-test--property->slotdef ()
  (should (equal
           '(debase-global-dns-configuration :type t :accessor debase-prop-global-dns-configuration)
           (debase--property->slotdef '(property
                                        ((type . "a{sv}")
                                         (name . "GlobalDnsConfiguration")
                                         (access . "readwrite")))))))

(ert-deftest debase-test---interface-method->arglist ()
  (should (equal '(arg0) (debase--interface-method->arglist '(method ((name . "Reload")) "\n      " (arg ((type . "u") (direction . "in"))) "\n    "))))

  (should (equal '(flags) (debase--interface-method->arglist '(method ((name . "Reload")) "\n      " (arg ((type . "u") (name . "flags") (direction . "in"))) "\n    ")))))

(provide 'debase--tests)
;;; debase--tests.el ends here
