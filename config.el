;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Give me my themes back! >:(

(put 'customize-themes 'disabled nil)

;; more sensible comint defaults

(setq comint-prompt-read-only nil)

;; org-mode

(after! org
  (setq org-todo-keywords
        '((sequence
           "STRT(s)"  ; A task that is in progress
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[-](S)"   ; Task is in progress
           "[ ](T)"   ; A task that needs doing
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)))
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo   todo-state-up priority-down category-keep)
          (tags   priority-down category-keep)
          (search category-keep)))
  (setq org-capture-templates '(("t" "Personal todo" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* TODO %?" :prepend t)
                                ("n" "Personal notes" entry
                                 (file+headline +org-capture-notes-file "Inbox")
                                 "* %u %?" :prepend t)
                                ("j" "Journal" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?" :prepend t)))
  (setq org-log-done t)
  (setq org-log-done-with-time nil)
  (map! :localleader :map org-mode-map :n "z" #'zpresent))

;; Do not ignore files over SSH in recentf

(after! recentf
  (setq recentf-exclude (delete "^/ssh:" recentf-exclude)))

;; magit-todos misbehaves over tramp
(defadvice! +magit--disable-todos-over-tramp-a (orig-fn)
  :around #'magit-todos--insert-todos
  (unless (file-remote-p default-directory)
    (funcall orig-fn)))

;; c-mode

(defun c-mode-config ()
  (c-set-style "cc-mode"))

(add-hook! 'c-mode-common-hook 'c-mode-config)

;; sourcepawn-mode

(define-derived-mode sourcepawn-mode c-mode
  "SourcePawn"
  "SourcePawn editing mode."

  (font-lock-add-keywords nil '(("\\<\\([A-Za-z]+\\|_\\):" . font-lock-builtin-face)))
  (setq-local indent-line-function 'c-indent-line)
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  (c-set-style "cc-mode")
  (display-fill-column-indicator-mode t)
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
  (setq yas-indent-line 'fixed))

(add-hook! 'yaml-mode-hook 'fix-yaml-mode)

;; Writing stuff

(setq olivetti-body-width 80)

(map! :leader
      (:desc "author" :prefix "a"
       :desc "Olivetti mode"   :n  "a" #'olivetti-mode
       :desc "Writerly font"   :n  "f" #'toggle-writerly-font
       :desc "Paragraph fill"  :nv "p" #'fill-paragraph))

;; spellcheck

; undo remapping of ispell-word
(map! [remap ispell-word] nil)

; https://github.com/hlissner/doom-emacs/issues/3592
(remove-hook! 'text-mode-hook #'+spell-remove-run-together-switch-for-aspell-h)

;; python

(setq python-shell-interpreter "python3"
      flycheck-python-flake8-executable "python3"
      flycheck-python-pycompile-executable "python3")

(setq python-pytest-executable "python3 -m pytest")

(add-hook! 'python-mode-hook
  (map! :localleader :map python-mode-map "t" nil)
  (map! :localleader
        :map python-mode-map
        "t" #'python-pytest-dispatch))

;; zpresent

(defun zpresent-mode-config ()
  (setq zpresent-fullscreen-on-zpresentation nil)
  (map! :map zpresent-mode-map :n [right] #'zpresent--next-slide)
  (map! :map zpresent-mode-map :n [left] #'zpresent--previous-slide)
  (map! :map zpresent-mode-map :n "q" #'kill-current-buffer))

(add-hook! 'zpresent-mode-hook 'zpresent-mode-config)

;; plantuml

(setq plantuml-jar-path "~/.local/share/plantuml/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

;; emojify

(use-package! emojify
  :hook (doom-first-input . global-emojify-mode))

;; runfile

(defun runfile-target ()
  (interactive)
  (let
      ((runfile-targets (split-string (shell-command-to-string "run --list"))))
    (ivy-read "run "
              runfile-targets
              :action '(lambda (x)
                         (make-comint-in-buffer
                          "runfile" "*runfile*"
                          "run" nil x)
                         (display-buffer "*runfile*")))))
