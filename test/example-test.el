;;; example-test.el --- Test example  -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test example

;;; Code:

;; (let ((chip8-root (expand-file-name "..")))
;;   (when (not (member chip8-root load-path))
;;     (add-to-list 'load-path chip8-root)))

(require 'chip8)

(ert-deftest shall-pass ()
  (should (equal 2 (+ 1 1))))

(ert-deftest shall-call-library ()
  (should (equal 2 (chip8--example))))

(ert-deftest shall-encode-bytes-in-binary-encoded-decimal ()
  (should (equal '(1 5 6) (chip8--to-bdc 156)))
  (should (equal '(0 1 5) (chip8--to-bdc 15)))
  (should (equal '(0 0 1) (chip8--to-bdc 1))))

(provide 'example-test)

;; Local Variables:
;; coding: utf-8
;; End:
;;; example-test.el ends here
