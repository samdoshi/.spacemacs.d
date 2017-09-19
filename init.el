(defun init/hashed-host-slug ()
  "Return a hashed version of the top-level domain name"
  (md5 (concat "UGXp4Adb.p8m;baTN8ybKxebV"
               (car (split-string (downcase (system-name)) "\\.")))))

(defconst init/solarized-base03  "#002b36")
(defconst init/solarized-base02  "#073642")
(defconst init/solarized-base01  "#586e75")
(defconst init/solarized-base00  "#657b83")
(defconst init/solarized-base0   "#839496")
(defconst init/solarized-base1   "#93a1a1")
(defconst init/solarized-base2   "#eee8d5")
(defconst init/solarized-base3   "#fdf6e3")
(defconst init/solarized-yellow  "#b58900")
(defconst init/solarized-orange  "#cb4b16")
(defconst init/solarized-red     "#dc322f")
(defconst init/solarized-magenta "#d33682")
(defconst init/solarized-violet  "#6c71c4")
(defconst init/solarized-blue    "#268bd2")
(defconst init/solarized-cyan    "#2aa198")
(defconst init/solarized-green   "#859900")

(defun dotspacemacs/layers ()
  (setq-default dotspacemacs-distribution 'spacemacs
                dotspacemacs-enable-lazy-installation 'unused
                dotspacemacs-ask-for-lazy-installation t
                dotspacemacs-configuration-layer-path '()
                dotspacemacs-configuration-layers
                '(auto-completion
                  better-defaults
                  colors
                  (c-c++ :variables
                         c-c++-enable-clang-support t)
                  dash
                  deft
                  emacs-lisp
                  finance
                  git
                  gtags
                  haskell
                  html
                  markdown
                  org
                  osx
                  python
                  (ranger :variables
                          ranger-show-preview t)
                  rust
                  (shell :variables
                         shell-default-shell 'eshell
                         shell-default-term-shell "/usr/local/bin/zsh")
                  (spell-checking :variables
                                  spell-checking-enable-by-default nil)
                  syntax-checking
                  version-control
                  yaml)
                dotspacemacs-additional-packages '(all-the-icons
                                                   editorconfig
                                                   extempore-mode)
                dotspacemacs-excluded-packages '()
                dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default dotspacemacs-editing-style 'vim
                dotspacemacs-themes '(solarized-dark
                                      solarized-light
                                      spacemacs-dark
                                      spacemacs-light
                                      leuven
                                      monokai
                                      zenburn)
                dotspacemacs-leader-key "SPC"
                dotspacemacs-emacs-leader-key "M-m"
                dotspacemacs-command-key "SPC"
                dotspacemacs-colorize-cursor-according-to-state t
                dotspacemacs-scratch-mode 'emacs-lisp-mode
                dotspacemacs-startup-lists '((recents . 7)
                                             (projects . 5))
                dotspacemacs-enable-paste-micro-state nil
                dotspacemacs-whitespace-cleanup 'changed
                dotspacemacs-line-numbers nil
                dotspacemacs-check-for-update nil)
  (setq-default spacemacs-evil-cursors
                `((normal       ,init/solarized-yellow   box)
                  (insert       ,init/solarized-green   (bar  . 2))
                  (emacs        ,init/solarized-blue     box)
                  (hybrid       ,init/solarized-blue    (bar  . 2))
                  (replace      ,init/solarized-magenta (hbar . 2))
                  (evilified    ,init/solarized-yellow   box)
                  (visual       ,init/solarized-orange  (hbar . 2))
                  (motion       ,init/solarized-violet   box)
                  (lisp         ,init/solarized-magenta  box)
                  (iedit        ,init/solarized-red      box)
                  (iedit-insert ,init/solarized-red     (bar  . 2))))

  (with-eval-after-load 'editorconfig
    (editorconfig-mode 1))

  (let ((host (init/hashed-host-slug))
        (osx-desktop "24e0c4b6b602908fd5cc6be519f8d96b")
        (osx-laptop "1808cc85340608cde18d5cab8b3be29d")
        (linux-desktop "1c093dc7c7abc05f76cdacd2ef83e5f5"))
    (message (concat "init/hashed-host-slug=" host))
    (when (equal host osx-desktop)
      (when window-system
        (menu-bar-mode 1)
        (set-frame-size (selected-frame) 100 60))
      (setq-default dotspacemacs-default-font '("Menlo"
                                                :size 17
                                                :weight normal
                                                :width normal
                                                :powerline-scale 1.1)))
    (when (equal host osx-laptop)
      (when window-system
        (menu-bar-mode 1)
        (set-frame-size (selected-frame) 120 46))
      (setq-default dotspacemacs-default-font '("Menlo"
                                                :size 13
                                                :weight normal
                                                :width normal
                                                :powerline-scale 1.1)))
    (when (equal host linux-desktop)
      (setq-default dotspacemacs-default-font '("Monospace"
                                                :size 30
                                                :weight normal
                                                :width normal
                                                :powerline-scale 1.1)))))

(defun dotspacemacs/user-init ()
  (setq all-the-icons-color-icons nil)
  (setq c-basic-offset 4)
  (setq css-indent-offset 2)
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (setq deft-directory "~/Dropbox/Notes")
  (setq display-time-24hr-format t
        display-time-default-load-average nil)
  (display-time-mode)
  (setq evil-lisp-state-enter-lisp-state-on-command nil)
  (setq evil-escape-key-sequence "jk"
        evil-escape-unordered-key-sequence t)
  (setq flycheck-c/c++-gcc-executable "gcc-5")
  (setq git-magit-status-fullscreen t)
  (setq haskell-process-show-debug-tips nil)
  (setq ispell-dictionary "british")
  (setq ledger-mode-should-check-version nil
        ledger-report-links-in-register nil
        ledger-binary-path "hledger")
  (setq neo-theme 'icons)
  (setq org-startup-indented t)
  (setq solarized-distinct-fringe-background t
        solarized-use-more-italic t
        solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil)
  (setq tab-width 4)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'prog-mode-hook 'turn-on-fci-mode))

(defun dotspacemacs/user-config ()
  (setq hl-paren-colors '("#d33682"))
  (setq org-directory "~/Dropbox/Notes"
        org-default-notes-file (concat org-directory "/Today.org")
        org-agenda-files (list (concat org-directory "/Today.org"))
        org-startup-folded "showall"
        org-bullets-bullet-list '("*"))
  (setq powerline-default-separator nil)

  (defun init/apple-uk-keymap (map)
    (define-key map (kbd "M-2") #'(lambda () (interactive) (insert "€")))
    (define-key map (kbd "M-3") #'(lambda () (interactive) (insert "#"))))

  (with-eval-after-load 'evil
    (init/apple-uk-keymap evil-insert-state-map))

  (with-eval-after-load 'helm
    (init/apple-uk-keymap helm-map))

  (defun init/kill-deft-window (orig-fun &rest args)
    (interactive)
    (kill-buffer "*Deft*"))

  (with-eval-after-load 'deft
    (advice-add 'deft-open-file :after 'init/kill-deft-window))

  (defun init/edit-Today.org ()
    (interactive)
    (find-file "~/Dropbox/Notes/Today.org"))

  (spacemacs/set-leader-keys "aN" 'init/edit-Today.org)

  (defun init/edit-Ledger ()
    (interactive)
    (find-file "~/Dropbox/Ledger/main.ledger"))

  (spacemacs/set-leader-keys "aB" 'init/edit-Ledger)

  ;; frame title
  (setq frame-title-format "%b - Emacs")
  ;; icon-title-format is for tabbed windows
  (setq icon-title-format "%b - Emacs")

  ;; Change frame font sizes with Super-Shift-{-=}
  (defun init/change-frame-font-height (delta)
    (let ((current-height (face-attribute 'default :height)))
      (set-face-attribute 'default
                          (selected-frame)
                          :height (+ current-height delta))))

  (with-eval-after-load 'undo-tree
    (unbind-key "M-_" undo-tree-map))
  (bind-key "M-+" '(lambda () (interactive) (init/change-frame-font-height +10)))
  (bind-key "M-_" '(lambda () (interactive) (init/change-frame-font-height -10)))

  ;; Use C-h for backspace everywhere
  (global-set-key "\C-h" 'delete-backward-char))
