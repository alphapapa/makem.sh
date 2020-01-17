;;; test-buttercup-example-package.el --- Buttercup tests for example-package.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords:
;; Package-Requires: ((buttercup))

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

(require 'buttercup)

;;;; Variables

;;;; Tests

(describe "Buttercup tests"
  (it "Should pass this test"
    (expect 'foo :to-equal 'foo)))

;;;; Functions


;;;; Footer

(provide 'test-buttercup-example-package)

;;; test-buttercup-example-package.el ends here
