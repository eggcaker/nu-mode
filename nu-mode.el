;;; nu-mode.el --- A major mode for the Nu programming language -*- lexical-binding: t -*-
;;
;; Filename: nu-mode.el
;; Description: A major mode for the Nu programming language
;; Author: eggcaker
;; Maintainer: eggcaker <eggcaker@gmail.com>
;; Version: 0.1.0
;; Keywords: nushell scripts
;; Compatibility: GNU Emacs 27.2
;; Package-Requires: ((emacs "27.2") (epc "0.1.1") (let-alist "1.0.1") (commenter "0.5.1"))
;;
;; This package provide a major-mode future (syntax highlight and
;; indentation)
;;
;;; Commentary:

;;; Code:

(require 'prog-mode)
(require 'cl)

(defvar nu-mode-hook nil)
(defvar nu-basic-offiset)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nu\\'" . nu-mode))


(defconst nu-types '("int" "string" "float" "duration"))
(defconst nu-keywords '("let" "for" "in" "if" "else" "elseif"))
(defconst nu-constants '("PATH"))

(setq nu-keywords-regexp (regexp-opt nu-keywords 'words))
(setq nu-type-regexp (regexp-opt nu-types 'words))
(setq git-regexp (regexp-opt '("git" "git checkout" "git rebase" "git pull" "git commit" "git add" "git push")))
(setq nu-core-regexp (regexp-opt '("alias" "debug" "def" "def-env" "describe" "do" "du" "echo" "error make" "export" "export def" "export def-env" "export env" "extern" "for" "help" "hide" "history" "if" "ignore" "let" "metadata" "module" "register" "source" "tutor" "use" "view-source")))
(setq nu-filters-regexp (regexp-opt '("all?" "any?" "append" "collect" "columns" "compact" "default" "drop" "drop column" "drop nth" "each" "each group" "each window" "empty?" "every" "find" "first" "flatten" "get" "headers" "keep" "keep until" "keep while" "last" "length" "lines" "merge" "move" "par-each" "par-each group" "prepend" "range" "reject" "rename" "reverse" "roll" "roll down" "roll left" "roll right" "roll up" "rotate" "select" "shuffle" "skip" "skip until" "skip while" "sort-by" "uniq" "update" "update cells" "where" "wrap" "zip")))
(setq nu-platform-regexp (regexp-opt '("ansi" "ansi gradient" "ansi strip" "clear" "input" "keybindings" "keybindings default" "keybindings list" "keybindings listen" "kill" "sleep" "term size")))
(setq nu-system-regexp (regexp-opt '("benchmark" "exec" "ps" "run_external" "sys" "which")))
(setq nu-strings-regexp (regexp-opt '("build-string" "char" "decode" "detect columns" "format" "nu-highlight" "parse" "size" "split" "split chars" "split column" "split row" "str" "str camel-case" "str capitalize" "str collect" "str contains" "str downcase" "str ends-with" "str find-replace" "str index-of" "str kebab-case" "str length" "str lpad" "str pascal-case" "str reverse" "str rpad" "str screaming-snake-case" "str snake-case" "str starts-with")))
(setq nu-generators-regexp (regexp-opt '("cal" "seq" "seq date")))
(setq nu-filesystem-regexp (regexp-opt '("cd" "cp" "load-env" "ls" "mkdir" "mv" "open" "rm" "save" "touch")))
(setq nu-deprecated-regexp (regexp-opt '("dataframe" "insert" "nth" "pivot" "str to-datetime" "str to-decimal" "str to-int" "unalias")))
(setq nu-date-regexp (regexp-opt '("date" "date format" "date humanize" "date list-timezone" "date now" "date to-table" "date to-timezone")))
(setq nu-dataframe-regexp (regexp-opt '("dfr" "dfr aggregate" "dfr all-false" "dfr all-true" "dfr append" "dfr arg-max" "dfr arg-min" "dfr arg-sort" "dfr arg-true" "dfr arg-unique" "dfr column" "dfr concatenate" "dfr contains" "dfr count-null" "dfr count-unique" "dfr cumulative" "dfr describe" "dfr drop" "dfr drop-nulls" "dfr dtypes" "dfr filter-with" "dfr first" "dfr get" "dfr get-day" "dfr get-hour" "dfr get-minute" "dfr get-month" "dfr get-nanosecond" "dfr get-ordinal" "dfr get-second" "dfr get-week" "dfr get-weekday" "dfr get-year" "dfr group-by" "dfr is-duplicated" "dfr is-in" "dfr is-not-null" "dfr is-null" "dfr is-unique" "dfr join" "dfr last" "dfr melt" "dfr not" "dfr open" "dfr pivot" "dfr rename" "dfr rename-col" "dfr replace" "dfr replace-all" "dfr rolling" "dfr sample" "dfr set" "dfr set-with-idx" "dfr shape" "dfr shift" "dfr slice" "dfr sort" "dfr str-lengths" "dfr str-slice" "dfr strftime" "dfr take" "dfr to-csv" "dfr to-df" "dfr to-dummies" "dfr to-lowercase" "dfr to-nu" "dfr to-parquet" "dfr to-uppercase" "dfr unique" "dfr value-counts" "dfr with-column")))
(setq nu-shells-regexp (regexp-opt '("enter" "exit" "shells")))
(setq nu-env-regexp (regexp-opt '("env" "let-env" "with-env")))
(setq nu-network-regexp (regexp-opt '("fetch" "url" "url host" "url path" "url query" "url scheme")))
(setq nu-conversions-regexp (regexp-opt '("fmt" "into" "into binary" "into bool" "into datetime" "into filesize" "into int" "into string")))
(setq nu-formats-regexp (regexp-opt '("from" "from csv" "from eml" "from ics" "from ini" "from json" "from ods" "from ssv" "from toml" "from tsv" "from url" "from vcf" "from xlsx" "from xml" "from yaml" "from yml" "to" "to csv" "to html" "to json" "to md" "to toml" "to tsv" "to url" "to xml" "to yaml")))
(setq nu-viewers-regexp (regexp-opt '("grid" "table")))
(setq nu-default-regexp (regexp-opt '("group-by" "hash md5" "hash sha256" "into decimal" "path" "path basename" "path dirname" "path exists" "path expand" "path join" "path parse" "path relative-to" "path split" "path type" "reduce" "split-by" "str substring" "str trim" "str upcase" "transpose" "version")))
(setq nu-hash-regexp (regexp-opt '("hash" "hash base64")))
(setq nu-math-regexp (regexp-opt '("math" "math abs" "math avg" "math ceil" "math eval" "math floor" "math max" "math median" "math min" "math mode" "math product" "math round" "math sqrt" "math stddev" "math sum" "math variance")))
(setq nu-random-regexp (regexp-opt '("random" "random bool" "random chars" "random decimal" "random dice" "random integer" "random uuid")))


(setq nu-font-lock-keywords
      `(
        (,nu-type-regexp . font-lock-type-face)
        (,nu-constant-regexp . font-lock-constant-face)

        (,git-regexp  . font-lock-builtin-face)

        (,nu-core-regexp . font-lock-builtin-face)
        (,nu-filters-regexp . font-lock-builtin-face)
        (,nu-platform-regexp . font-lock-builtin-face)
        (,nu-system-regexp . font-lock-builtin-face)
        (,nu-strings-regexp . font-lock-builtin-face)
        (,nu-generators-regexp . font-lock-builtin-face)
        (,nu-filesystem-regexp . font-lock-builtin-face)
        (,nu-deprecated-regexp . font-lock-builtin-face)
        (,nu-date-regexp . font-lock-builtin-face)
        (,nu-dataframe-regexp .  font-lock-builtin-face)
        (,nu-shells-regexp . font-lock-builtin-face)
        (,nu-env-regexp . font-lock-builtin-face)
        (,nu-network-regexp . font-lock-builtin-face)
        (,nu-conversions-regexp .  font-lock-builtin-face)
        (,nu-formats-regexp . font-lock-builtin-face)
        (,nu-viewers-regexp . font-lock-builtin-face)
        (,nu-default-regexp . font-lock-builtin-face)
        (,nu-hash-regexp . font-lock-builtin-face)
        (,nu-math-regexp . font-lock-builtin-face)
        (,nu-random-regexp . font-lock-builtin-face)


        (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_ ?' "\"" ))))
         (1  font-lock-function-name-face))
        (,nu-keywords-regexp . font-lock-keyword-face)

        ("$[a-zA-Z-0-9_\"]*" . font-lock-variable-name-face)
        ("\\([^ \n]*\\) *=" 1 font-lock-variable-name-face)

        ))

(defconst nu-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; # starts comments
    (modify-syntax-entry ?# "<" syntax-table)
    ;; endline ends comments
    (modify-syntax-entry ?\n ">" syntax-table)
    ;; underscores and dashes don't break words
    (modify-syntax-entry ?_ "w" syntax-table)
    (modify-syntax-entry ?- "w" syntax-table)
    ;; backticks are like quotes in shell
    (modify-syntax-entry ?` "\"" syntax-table)
    syntax-table)

(defvar nu-indent-offset 4 "My indentation offset.")

(defun nu-indent-line ()
  "Indent bodies of rules by the previous indent, or by `tab-width'."
  (interactive)
  (and abbrev-mode (= (char-syntax (preceding-char)) ?w)
       (expand-abbrev))
  (if (> (current-column) (current-indentation))
      ;; Don't indent when hitting tab in the middle of a line.
      'noindent
    (skip-chars-forward " \t")
    (indent-to
     (if (= (line-number-at-pos) (prog-first-column))
         (prog-first-column)
       (save-excursion
         (forward-line -1)
         (skip-chars-forward " \t")
         (let* ((previous-indentation (current-column))
                (previous-line-is-empty (and (bolp) (eolp)))
                (previous-line-contents (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                (previous-line-is-rule (string-match "^[^ \t#:][^#:]*:\\([^=].*\\|$\\)" previous-line-contents)))
           (cond (previous-line-is-empty (prog-first-column))
                 (previous-line-is-rule (+ (prog-first-column) tab-width))
                 (t previous-indentation))))))))

;;;###autoload
(define-derived-mode nu-mode prog-mode "Nu"
  "Major mode for editing standard Nufiles."

  :syntax-table nu-mode-syntax-table

  ;; Font lock.
  (setq font-lock-defaults '(nu-font-lock-keywords))

  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]*")

  ;; Tabs
  (setq-local tab-width 2)
  (setq-local tab-stop-list (number-sequence 0 120 2))

  (when (boundp 'evil-shift-width)
    (setq-local evil-shift-width 4))

  ;; Indentation
  (setq-local indent-line-function 'nu-indent-line))


;;; nu-mode.el ends here
;;;
(provide 'nu-mode)
