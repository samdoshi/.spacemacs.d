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
                dotspacemacs-configuration-layer-path '()
                dotspacemacs-configuration-layers '(auto-completion
                                                    better-defaults
                                                    colors
                                                    c-c++
                                                    deft
                                                    emacs-lisp
                                                    git
                                                    haskell
                                                    markdown
                                                    org
                                                    osx
                                                    (shell :variables shell-default-term-shell "/usr/local/bin/zsh")
                                                    syntax-checking
                                                    version-control)
                dotspacemacs-additional-packages '()
                dotspacemacs-excluded-packages '()
                dotspacemacs-delete-orphan-packages t))

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
                dotspacemacs-command-key ":")
  (setq-default spacemacs-evil-cursors `((normal       ,init/solarized-yellow   box)
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
  (let ((host (init/hashed-host-slug))
        (desktop "24e0c4b6b602908fd5cc6be519f8d96b")
        (laptop "1808cc85340608cde18d5cab8b3be29d"))
    (message (concat "init/hashed-host-slug=" host))
    (when (equal host desktop)
      (progn
        (when window-system (set-frame-size (selected-frame) 100 60))
        (setq-default dotspacemacs-default-font '("Menlo"
                                                  :size 17
                                                  :weight normal
                                                  :width normal
                                                  :powerline-scale 1.1))))
    (when (equal host laptop)
      (progn
        (when window-system (set-frame-size (selected-frame) 120 46))
        (setq-default dotspacemacs-default-font '("Menlo"
                                                  :size 13
                                                  :weight normal
                                                  :width normal
                                                  :powerline-scale 1.1))))))

(defun dotspacemacs/user-init ()
  (setq deft-directory "/Users/sam/Dropbox/Notes")
  (setq flycheck-c/c++-gcc-executable  "gcc-5")
  (setq solarized-distinct-fringe-background t
        solarized-use-more-italic t
        solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil))

(defun dotspacemacs/user-config ()
  (setq powerline-default-separator nil)
  (with-eval-after-load 'org
    (setq org-startup-folded "showall"
          org-bullets-bullet-list '("*"))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
