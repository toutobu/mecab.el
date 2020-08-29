;;; mecab-enhanced.el-test.el -- Tests for mecab-enhanced.el

;;; Code:

(require 'mecab-enhanced)

(ert-deftest mecab-enhanced-test--mecab-enhanced-parse-to-iter ()
  (let* ((sentence "なんだって円錐の立方積なんぞを計算し出したのだ")
         (iter (mecab-enhanced-parse-to-iter sentence))
         (x (iter-next iter)))
    (should (string= (oref x surface) ""))
    (should (string= (oref x pos) "BOS/EOS"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "なん"))
    (should (string= (oref x pos) "名詞"))
    (should (string= (oref x sub-pos1) "代名詞"))
    (should (string= (oref x sub-pos2) "一般"))
    (should (string= (oref x sub-pos3) "*"))
    (should (string= (oref x conjugation-type) "*"))
    (should (string= (oref x conjugation-form) "*"))
    (should (string= (oref x original-form) "なん"))
    (should (string= (oref x ruby) "ナン"))
    (should (string= (oref x pronounciation) "ナン"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "だって"))
    (should (string= (oref x pos) "助詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "円錐"))
    (should (string= (oref x pos) "名詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "の"))
    (should (string= (oref x pos) "助詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "立方"))
    (should (string= (oref x pos) "名詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "積"))
    (should (string= (oref x pos) "名詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "なんぞ"))
    (should (string= (oref x pos) "助詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "を"))
    (should (string= (oref x pos) "助詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "計算"))
    (should (string= (oref x pos) "名詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "し"))
    (should (string= (oref x pos) "動詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "出し"))
    (should (string= (oref x pos) "動詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "た"))
    (should (string= (oref x pos) "助動詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "の"))
    (should (string= (oref x pos) "名詞"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "だ"))
    (should (string= (oref x pos) "助動詞"))))

(ert-deftest mecab-enhanced-test--mecab-enhanced-parse-to-iter--offsets ()
  (let* ((sentence "「して見ると、巡査が虎で、我々三人が酔人だね」と、岡田が冷かした。
「Silentium !」と石原が叫んだ。  もう無縁坂の方角へ曲る角に近くなったからである。

角を曲れば")
         (iter (mecab-enhanced-parse-to-iter sentence nil t))
         (x (iter-next iter)))
    (should (string= (oref x surface) ""))
    (should (string= (oref x pos) "BOS/EOS"))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "「"))
    (should (string= (oref x pos) "記号"))
    (should (equal (oref x offsets) '(0 . 1)))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "し"))
    (should (string= (oref x pos) "動詞"))
    (should (equal (oref x offsets) '(1 . 2)))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "て"))
    (should (string= (oref x pos) "助詞"))
    (should (equal (oref x offsets) '(2 . 3)))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "見る"))
    (should (string= (oref x pos) "動詞"))
    (should (equal (oref x offsets) '(3 . 5)))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "と"))
    (should (string= (oref x pos) "助詞"))
    (should (equal (oref x offsets) '(5 . 6)))

    (cl-loop for i from 0 to 19 do (setq x (iter-next iter)))

    (should (string= (oref x surface) "冷かし"))
    (should (equal (oref x offsets) '(28 . 31)))
    (setq x (iter-next iter))
    (should (string= (oref x surface) "た"))
    (should (equal (oref x offsets) '(31 . 32)))
    (setq x (iter-next iter))
    (should (string= (oref x surface) "。"))
    (should (equal (oref x offsets) '(32 . 33)))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "「"))
    (should (equal (oref x offsets) '(34 . 35)))

    (setq x (iter-next iter))
    (should (string= (oref x surface) "Silentium"))
    (should (equal (oref x offsets) '(35 . 44)))

    (cl-loop for i from 0 to 23 do (setq x (iter-next iter)))

    (should (string= (oref x surface) "角"))
    (should (equal (oref x offsets) '(83 . 84)))))

;;; mecab-enhanced.el-test.el ends here
