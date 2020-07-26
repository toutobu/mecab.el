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

(defun mecab-create-tagger (arg)
  (mecabel-impl-create-tagger arg (string-bytes arg))) ;; size

(defun mecab-parse-to-node (tagger sentence)
  (mecabel-impl-parse-to-node tagger sentence (string-bytes sentence))) ;;size

(defun mecab-next-node (node)
  (mecabel-impl-next-node node))

(defun mecab-get-node-value (node key)
  (mecabel-impl-get-node-value node key (string-bytes key))) ;; size

(provide 'mecab)
;;; mecab.el ends here
