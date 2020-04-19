;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Set window movement keybindings to default (shift + arrow)

(windmove-default-keybindings)

;; Give me my themes back! >:(

(put 'customize-themes 'disabled nil)

;; Do not ignore files over SSH in recentf

(after! recentf
  (setq recentf-exclude (delete "^/ssh:" recentf-exclude)))

;; magit-todos misbehaves over tramp
(defadvice! +magit--disable-todos-over-tramp-a (orig-fn)
  :around #'magit-todos--insert-todos
  (unless (file-remote-p default-directory)
    (funcall orig-fn)))

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

;; easier comment toggle
(map! :desc "Comment line" :nv "SPC c ;" #'comment-line)

;; Fonts

(setq writerly-font-active nil)
(setq default-font (font-spec :family "Noto Mono" :size 13))
(setq writerly-font (font-spec :family "Go Mono" :size 13))

(defun toggle-writerly-font ()
  (interactive)
  (if (eq writerly-font-active t)
      (progn
        (setq doom-font default-font)
        (set-frame-font default-font)
        (setq writerly-font-active nil))
    (progn
      (setq doom-font writerly-font)
      (set-frame-font writerly-font)
      (setq writerly-font-active t))
    ))

(setq doom-font default-font)

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

  ;; Hack to get markdown-mode to re-eval after setting the above consts.

  (if (eq markdown-mode-rerun nil)
      (progn (setq markdown-mode-rerun t)
             (markdown-mode)))

)

(setq markdown-mode-rerun nil)

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

;; Avoid [Display not ready] errors

(setq helm-exit-idle-delay 0)

;; yaml-mode fixes
;; yaml-mode derives from text-mode, which loads things you'd normally want
;; when editing text, like flyspell and auto-fill-mode. But yaml is
;; configuration, not text, and these frequently get in the way.

(defun fix-yaml-mode ()
  (flyspell-mode -1)
  (auto-fill-mode -1)
  )

(add-hook! 'yaml-mode-hook 'fix-yaml-mode)

;; Writing commands

(setq author-mode-active nil)
(defun toggle-author-mode ()
  (interactive)
  (if (eq author-mode-active t)
      (progn
        (display-line-numbers-mode 1)
        (writeroom-mode -1)
        (if (eq writerly-font-active t) (toggle-writerly-font))
        (setq author-mode-active nil))
    (progn
      (display-line-numbers-mode -1)
      (writeroom-mode 1)
      (if (eq writerly-font-active nil) (toggle-writerly-font))
      (setq author-mode-active t))
    ))

(map! :leader
      (:desc "author" :prefix "a"
        :desc "Author mode"          :n "a" #'toggle-author-mode
        :desc "Writerly font"        :n "f" #'toggle-writerly-font
        :desc "Paragraph fill"       :nv "p" #'fill-paragraph))

;; python3

(setq python-shell-interpreter "python3"
      flycheck-python-flake8-executable "python3"
      flycheck-python-pycompile-executable "python3")
