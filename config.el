;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Set window movement keybindings to default (shift + arrow)

(windmove-default-keybindings)

;; sourcepawn-mode

(define-derived-mode sourcepawn-mode c-mode
  "SourcePawn"
  "SourcePawn editing mode."

  (font-lock-add-keywords nil '(("\\<\\([A-Za-z]+\\|_\\):" . font-lock-builtin-face)))
  (setq-local indent-line-function 'c-indent-line)
  (setq-local tab-width 2)
)

(add-to-list 'auto-mode-alist '(".sp\\'" . sourcepawn-mode))
(add-to-list 'auto-mode-alist '(".inc\\'" . sourcepawn-mode))

;; typopunct

(load! "typopunct")
(map! :desc "Typopunct" :n "SPC t t" #'typopunct-mode)

;; markdown-mode

(defun markdown-mode-config ()
  (set-fill-column 80)
  (turn-on-auto-fill)
  (typopunct-change-language 'english t)
  (typopunct-mode 1)
  (setq-local comment-use-syntax nil)

  ;; In markdown-mode, I'm more interested in matching Liquid comments than
  ;; HTML comments, since I use Jekyll more than straight HTML.

  (defconst markdown-regex-comment-start
    "{%\\(?:\s*\\|\n\\)comment\\(?:\s*\\|\n\\)%}"
    "Regular expression matches Liquid comment opening.")

  (defconst markdown-regex-comment-end
    "{%\\(?:\s*\\|\n\\)endcomment\\(?:\s*\\|\n\\)%}"
    "Regular expression matches Liquid comment closing.")

)

(add-hook! 'markdown-mode-hook 'markdown-mode-config)
(add-hook! 'gfm-mode-hook 'markdown-mode-config)

;; cloudformation-mode

(define-derived-mode cfn-mode yaml-mode
  "Cloudformation"
  "Cloudformation template mode.")

(add-to-list 'auto-mode-alist '(".template\\'" . cfn-mode))
(after! flycheck
  (flycheck-define-checker cfn-lint
    "A Cloudformation linter using cfn-python-lint.

See URL 'https://github.com/awslabs/cfn-python-lint'."
    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns (
                     (warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end)
                     )
    :modes (cfn-mode)
    )
    (add-to-list 'flycheck-checkers 'cfn-lint)
  )

;; A bit more breathing room for git commit summaries.
;; This returns git-commit-summary-max-length to magit's default.

(defun +vc|enforce-git-commit-conventions ()
  (setq fill-column 72
        git-commit-summary-max-length 68
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

;; Fonts

(if (eq system-type 'gnu/linux)
    (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 13)))

;; Avoid [Display not ready] errors

(setq helm-exit-idle-delay 0)

;; Writing commands

(map! :leader
      (:desc "author" :prefix "a"
        :desc "Paragraph fill"       :nv "p" #'fill-paragraph))
