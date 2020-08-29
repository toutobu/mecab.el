;;; mecab-enhanced.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Junpei Tajima
;;
;; Author: Junpei Tajima <http://github/p-baleine>
;; Maintainer: Junpei Tajima <p-baleine@gmail.com>
;; Created: July 26, 2020
;; Modified: July 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/p-baleine/mecab-enhanced
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'eieio)
(require 'mecab)
(require 'generator)
(require 's)

(defgroup mecab-enhanced ()
  "Customize group for mecab-enhanced.el."
  :prefix "mecab-enhanced-"
  :group 'mecab)

(defclass mecab-enhanced-node ()
  ((surface
    :initarg :surface
    :type string
    :documentation "Surface(表層形).")
   (pos
    :initarg :pos
    :type string
    :documentation "Part of speech(品詞).")
   (sub-pos1
    :initarg :sub-pos1
    :type string
    :documentation "Sub pos 1(品詞細分類 1).")
   (sub-pos2
    :initarg :sub-pos2
    :type string
    :documentation "Sub pos 2(品詞細分類 2).")
   (sub-pos3
    :initarg :sub-pos3
    :type string
    :documentation "Sub pos 3(品詞細分類 3).")
   (conjugation-type
    :initarg :conjugation-type
    :type string
    :documentation "Conjugation type(活用型).")
   (conjugation-form
    :initarg :conjugation-form
    :type string
    :documentation "Conjugation form(活用形).")
   (original-form
    :initarg :original-form
    :type string
    :documentation "Original form(原形).")
   (ruby
    :initarg :ruby
    :documentation "Ruby(読み).")
   (pronounciation
    :initarg :pronounciation
    :documentation "Pronounciation(発音).")
   (offsets
    :initarg :offsets
    :type cons
    :documentation "Offset of this node.")))

(iter-defun mecab-enhanced-parse-to-iter
  (sentence &optional tagger preserve-newline-offset)
  "Return an iterator that iterates on parsed result of `SENTENCE'.
It yields `MECAB-ENHANCED-NODE' objects.
If `TAGGER' is specified, `SENTENCE' will be parsed by `TAGGER'.
If `PRESERVE-NEWLINE-OFFSET' is specified, offsets for newlines are preserved.
"
  (let* ((tagger (or tagger (mecab-create-tagger "")))
         (node (mecab-parse-to-node tagger sentence))
         (offset 0)
         (newline-offsets
          (and preserve-newline-offset
               (mecab-enhanced--gather-newline-offsets sentence))))
    (while node
      (when preserve-newline-offset
        (let (current-newline-offset)
          (while (setq current-newline-offset
                       (--find (eq offset (car it)) newline-offsets))
            (setq offset (cdr current-newline-offset)))))
      (let* ((surface (mecab-node-ref node surface))
             (feature (mecab-node-ref node feature))
             (feature-lst (s-split "," feature))
             (offset-end  (+ (length surface) offset)))
        (iter-yield
         (make-instance 'mecab-enhanced-node
                        :surface surface
                        :pos (nth 0 feature-lst)
                        :sub-pos1 (nth 1 feature-lst)
                        :sub-pos2 (nth 2 feature-lst)
                        :sub-pos3 (nth 3 feature-lst)
                        :conjugation-type (nth 4 feature-lst)
                        :conjugation-form (nth 5 feature-lst)
                        :original-form (nth 6 feature-lst)
                        :ruby (nth 7 feature-lst)
                        :pronounciation (nth 8 feature-lst)
                        :offsets (cons offset offset-end)))
        (setq node (mecab-next-node node))
        (setq offset offset-end)))))

;; utils

(cl-defun mecab-enhanced--gather-newline-offsets (sentence)
  "Return list of offsets of newlines in `SENTENCE'."
  (let ((newline-offsets '())
        (op 0))
    (with-temp-buffer
      (insert sentence)
      (setq op (goto-char (point-min)))
      (while (re-search-forward "[\s\n]" nil t)
        (let* ((s (1- (match-beginning 0)))
               (e (1- (match-end 0)))
               (offset (cons s e)))
          (setq newline-offsets (append newline-offsets (list offset)))
          (setq op (goto-char (match-end 0))))))
    newline-offsets))

(provide 'mecab-enhanced)
;;; mecab-enhanced.el ends here
