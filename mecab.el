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

(defgroup mecab ()
  "Customize group for mecab.el."
  :prefix "mecab-")

(defun mecab-create-tagger (arg)
  "Create a new Tagger based on `ARG'."
  (mecabel-impl-create-tagger arg (string-bytes arg)))

(defun mecab-parse-to-node (tagger sentence)
  "Parse `SENTENCE' with `TAGGER' and return Node object."
  (mecabel-impl-parse-to-node tagger sentence (string-bytes sentence)))

(defun mecab-next-node (node)
  "Return next Node of `NODE'."
  (mecabel-impl-next-node node))

(defun mecab-get-node-value (node key)
  "Retrieve the value stored in `NODE' whose key is `KEY'.
`KEY' must be string."
  (mecabel-impl-get-node-value node key (string-bytes key)))

(defmacro mecab-node-ref (node key)
  "Retrieve the value stored in `NODE' whose key is `KEY'."
  (let ((key-str (with-output-to-string (princ key))))
    `(mecabel-impl-get-node-value ,node ,key-str (string-bytes ,key-str))))

(provide 'mecab)
;;; mecab.el ends here
