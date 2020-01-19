;;; test-ert-example-package.el --- ERT tests for example-package.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords:

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

;; Foo bar.

;;; Code:

;;;; Requirements

(require 'ert)

;;;; Variables

;;;; Tests

(ert-deftest test-ert-example-package-1 ()
  "A test."
  (should (eq 'foo 'foo)))

;;;; Functions


;;;; Footer

(provide 'test-ert-example-package)

;;; test-ert-example-package.el ends here
