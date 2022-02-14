(require 'ert)
(require 'assess)
(require 'nu-mode)


(ert-deftest nu-mode-highlight-comment ()
  (should (assess-face-at=
           "# foo bar"
           'nu-mode
           '("# foo bar")
           '(font-lock-comment-face nil))))

(ert-deftest nu-mode-highlight-keywords ()
  (should (assess-face-at=
           "def hello"
           'nu-mode
           "def"
           'font-lock-keyword-face)))
