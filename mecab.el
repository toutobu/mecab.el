;;; mecab.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Junpei Tajima
;;
;; Author: Junpei Tajima <http://github/p-baleine>
;; Maintainer: Junpei Tajima <p-baleine@gmail.com>
;; Created: July 26, 2020
;; Modified: July 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/toutobu/mecabel
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

;;

(require 'mecabel-impl)

(defun mecab-test ()
  (print (mecabel-impl-test)))

(provide 'mecab)
;;; mecab.el ends here
