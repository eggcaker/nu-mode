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



(defcustom nu-command-builtins '(
                                 ("core" . '("alias" "debug" "def" "def-env" "describe" "do" "du" "echo" "error make" "export" "export def" "export def-env" "export env" "extern" "for" "help" "hide" "history" "if" "ignore" "let" "metadata" "module" "register" "source" "tutor" "use" "view-source"))
                                 ("filters" . '("all?" "any?" "append" "collect" "columns" "compact" "default" "drop" "drop column" "drop nth" "each" "each group" "each window" "empty?" "every" "find" "first" "flatten" "get" "headers" "keep" "keep until" "keep while" "last" "length" "lines" "merge" "move" "par-each" "par-each group" "prepend" "query" "query json" "query xml" "range" "reject" "rename" "reverse" "rotate" "select" "shuffle" "skip" "skip until" "skip while" "sort-by" "uniq" "update" "update cells" "where" "wrap" "zip"))
                                 ("platform" . '("ansi" "ansi gradient" "ansi strip" "clear" "input" "keybindings" "keybindings default" "keybindings list" "keybindings listen" "kill" "sleep" "term size"))
                                 ("system" . '("benchmark" "exec" "ps" "run_external" "sys" "which"))
                                 ("strings" . '("build-string" "char" "decode" "detect columns" "format" "nu-highlight" "parse" "size" "split" "split chars" "split column" "split row" "str" "str camel-case" "str capitalize" "str collect" "str contains" "str downcase" "str ends-with" "str find-replace" "str index-of" "str kebab-case" "str length" "str lpad" "str pascal-case" "str reverse" "str rpad" "str screaming-snake-case" "str snake-case" "str starts-with"))
                                 ("generators" . '("cal" "seq" "seq date"))
                                 ("filesystem" . '("cd" "cp" "load-env" "ls" "mkdir" "mv" "open" "rm" "save" "touch"))
                                 ("deprecated" . '("dataframe" "insert" "nth" "pivot" "str to-decimal" "str to-int"))
                                 ("date" . '("date" "date format" "date humanize" "date list-timezone" "date now" "date to-table" "date to-timezone"))
                                 ("dataframe" . '("dfr" "dfr aggregate" "dfr all-false" "dfr all-true" "dfr append" "dfr arg-max" "dfr arg-min" "dfr arg-sort" "dfr arg-true" "dfr arg-unique" "dfr column" "dfr concatenate" "dfr contains" "dfr count-null" "dfr count-unique" "dfr cumulative" "dfr describe" "dfr drop" "dfr drop-nulls" "dfr dtypes" "dfr filter-with" "dfr first" "dfr get" "dfr get-day" "dfr get-hour" "dfr get-minute" "dfr get-month" "dfr get-nanosecond" "dfr get-ordinal" "dfr get-second" "dfr get-week" "dfr get-weekday" "dfr get-year" "dfr group-by" "dfr is-duplicated" "dfr is-in" "dfr is-not-null" "dfr is-null" "dfr is-unique" "dfr join" "dfr last" "dfr melt" "dfr not" "dfr open" "dfr pivot" "dfr rename" "dfr rename-col" "dfr replace" "dfr replace-all" "dfr rolling" "dfr sample" "dfr set" "dfr set-with-idx" "dfr shape" "dfr shift" "dfr slice" "dfr sort" "dfr str-lengths" "dfr str-slice" "dfr strftime" "dfr take" "dfr to-csv" "dfr to-df" "dfr to-dummies" "dfr to-lowercase" "dfr to-nu" "dfr to-parquet" "dfr to-uppercase" "dfr unique" "dfr value-counts" "dfr with-column"))
                                 ("shells" . '("enter" "exit" "g" "n" "p" "shells"))
                                 ("env" . '("env" "let-env" "with-env"))
                                 ("network" . '("fetch" "query web" "url" "url host" "url path" "url query" "url scheme"))
                                 ("conversions" . '("fmt" "into" "into binary" "into bool" "into datetime" "into filesize" "into int" "into string"))
                                 ("formats" . '("from" "from csv" "from eml" "from ics" "from ini" "from json" "from ods" "from ssv" "from toml" "from tsv" "from url" "from vcf" "from xlsx" "from xml" "from yaml" "from yml" "to" "to csv" "to html" "to json" "to md" "to toml" "to tsv" "to url" "to xml" "to yaml"))
                                 ("default" . '("git checkout" "git push" "group-by" "hash md5" "hash sha256" "into decimal" "path" "path basename" "path dirname" "path exists" "path expand" "path join" "path parse" "path relative-to" "path split" "path type" "reduce" "split-by" "str substring" "str trim" "str upcase" "transpose" "version"))
                                 ("viewers" . '("grid" "table"))
                                 ("hash" . '("hash" "hash base64"))
                                 ("math" . '("math" "math abs" "math avg" "math ceil" "math eval" "math floor" "math max" "math median" "math min" "math mode" "math product" "math round" "math sqrt" "math stddev" "math sum" "math variance"))
                                 ("random" . '("random" "random bool" "random chars" "random decimal" "random dice" "random integer" "random uuid"))

   ))


(defconst nu-keywords
  '("\\(let\\|for\\|def\\|if\\|else\\|export\\)"))

(defconst nu-highlights
  '(;; Variable interpolation looks like "{{varname}}"
    ("$[a-zA-Z-0-9 \"]*" . font-lock-variable-name-face)
    ;; Variable assignment looks like "varname :="
    ("\\([^ \n]*\\) *=" 1 font-lock-variable-name-face)
    ;; Highlight variable interpolation in shell scripts like "${varname}"
    ("\\${\\([^}\n]*\\)}" 1 font-lock-variable-name-face)
    ;; Highlight rules like "rulename:"
    ("^\\(@?\\)\\([^ @:\n]+\\).*:\\([^=\n]\\|$\\)"
     (1 font-lock-negation-char-face)
     (2 font-lock-function-name-face))
    ;; TODO highlight arguments to rules. I would have done it, but it was hard so I gave up for now.
    ))

(defconst nu-font-lock-keywords
  (append nu-keywords
          nu-highlights))

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
    syntax-table))

(defun nu-untab-region (N)
  "Untab a selected region using `indent-rigidly'.
Argument N number of untabs to perform"
  (interactive "p")
  (let ((begin (if (use-region-p)
                 (region-beginning)
                 (line-beginning-position)))
        (end (if (use-region-p)
                 (region-end)
                 (line-end-position))))
    (indent-rigidly begin end (* N -4))))

;; from https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
;; (which is licensed GPL 2 or later)
(defvar nu-indent-offset 4 "My indentation offset.")
(defun nu-backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively #'backward-delete-char-untabify)
    (let ((movement (% (current-column) nu-indent-offset))
          (p (point)))
      (when (= movement 0) (setq movement nu-indent-offset))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively #'backward-delete-char))))))

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
  (setq-local tab-width 4)
  (setq-local tab-stop-list (number-sequence 0 120 4))

  (when (boundp 'evil-shift-width)
    (setq-local evil-shift-width 4))

  ;; Indentation
  (setq-local indent-line-function 'nu-indent-line)
  (local-set-key (kbd "DEL") #'nu-backspace-whitespace-to-tab-stop)
  (local-set-key (kbd "<backtab>") #'nu-untab-region))


;;; nu-mode.el ends here
;;;
(provide 'nu-mode)
