;;; debase--tests.el --- Tests for debase            -*- lexical-binding: t; -*-

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

;; Test code.

;;; Code:

(require 'ert)
(require 'debase)

(ert-deftest debase--test--assert-success ()
  (should (not (debase--assert '(interface) 'interface)))
  (should (not (debase--assert '(node) 'node)))
  (should (not (debase--assert '(property) 'property))))

(ert-deftest debase--test--assert-failure () :expected-result :failed
  (should (not (debase--assert '(interface) 'property)))
  (should (not (debase--assert '(node) 'property)))
  (should (not (debase--assert '(property) 'method))))

(ert-deftest debase--test--flet-partial ()
  (debase-flet-partial ((+ 1)
                        (message "Hello %s"))
    (should (= 6 (+ 5)))
    (should (string= "Hello world" (message "world")))))

(provide 'debase--tests)
;;; debase--tests.el ends here
