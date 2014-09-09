;; Based on https://github.com/sachac/.emacs.d

(require 'package)                                                                                                      
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(require 'use-package)

;; Sacha package install

(defun sacha/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
    (package-install package))))

;; Backup

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Windows configuration

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; Winner mode

(sacha/package-install 'winner)
(use-package winner
  :config (winner-mode 1))

;; Helm - interactive completion

(use-package helm
  :init
  (progn 
    (require 'helm-config) 
    (setq helm-candidate-number-limit 10)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :config
  (progn
    ;; I don't like the way switch-to-buffer uses history, since
    ;; that confuses me when it comes to buffers I've already
    ;; killed. Let's use ido instead.
    (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido)))
  :bind (("C-c h" . helm-mini)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;; Mode line format

(use-package smart-mode-line
  :init
  (progn
  (setq-default
   mode-line-format 
   '("%e"
     mode-line-front-space
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     mode-line-frame-identification
     mode-line-buffer-identification
     "   "
     mode-line-position
     (vc-mode vc-mode)
     "  "
     mode-line-modes
     mode-line-misc-info
     mode-line-end-spaces))))

;; Change "yes or no" to "y or n"

(fset 'yes-or-no-p 'y-or-n-p)   

;; Set up a light-on-dark color scheme

(defadvice color-theme-alist (around sacha activate)
  (if (ad-get-arg 0)
      ad-do-it
    nil))
(sacha/package-install 'color-theme)
(sacha/package-install 'color-theme-solarized)
(defun sacha/setup-color-theme ()
  (interactive)
  (color-theme-solarized 'dark)
  (set-face-foreground 'secondary-selection "darkblue")
  (set-face-background 'secondary-selection "lightblue")
  (set-face-background 'font-lock-doc-face "black")
  (set-face-foreground 'font-lock-doc-face "wheat")
  (set-face-background 'font-lock-string-face "black")
  (set-face-foreground 'org-todo "green")
  (set-face-background 'org-todo "black"))
 
(use-package color-theme
  :init
  (when window-system
    (sacha/setup-color-theme)))

(when window-system
  (custom-set-faces
   '(erc-input-face ((t (:foreground "antique white"))))
   '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
   '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
   '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
   '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))))

;; Undo tree mode - visualize your undos and branches
;; C-x u (undo-tree-visualize)
(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; Killing text
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; Text size
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

;; Helm-swoop - quickly finding lines
(use-package helm-swoop
 :bind (("C-S-s" . helm-swoop)))

;; Windmove - switching between windows
(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)))

;; Key chords
(use-package key-chord
  :init
  (progn 
    (key-chord-mode 1)
    (key-chord-define-global "uu"     'undo)
    (key-chord-define-global "yy"     'browse-kill-ring)
    (key-chord-define-global "jj"     'ace-jump-word-mode)
    (key-chord-define-global "jw"     'ace-window)
    (key-chord-define-global "jl"     'ace-jump-line-mode)
    (key-chord-define-global "FF"     'find-file)
    (key-chord-define-global "JJ"     'sacha/switch-to-previous-buffer)))

;; Shortcuts
(global-set-key [f2] 'recentf-open-files)
(global-set-key [f3] 'goto-line)
(global-set-key [f4] 'find-file)
(global-set-key [f5] 'switch-to-buffer)
(global-set-key [f6] 'kill-current-buffer)
(global-set-key [f11] 'save-buffer)
(global-set-key [f12] 'buffer-menu)

;; Set the cursor color to "red"
(set-cursor-color "red")

;; Don't bring this silly startup screen
(setq inhibit-startup-message t)

;; Spell
(setq ispell-program-name "ispell")
(setq ispell-dictionary "american")
(put 'dired-find-alternate-file 'disabled nil)

;; Flyspell
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Multiple cursors
(require 'multiple-cursors) 
(global-unset-key (kbd "M-<down-mouse-1>")) 
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;; Dired
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))


;; C++ ide
(add-to-list 'load-path "~/build/rtags/src")
(require 'rtags)
;;(rtags-enable-standard-keybindings c-mode-base-map)

(defun use-rtags (&optional useFileManager)
  (and (rtags-executable-find "rc")
       (cond ((not (gtags-get-rootpath)) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

(defun tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
      (gtags-find-tag)))
(defun tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
      (gtags-find-rtag)))
(defun tags-find-symbol ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-find-symbol 'gtags-find-symbol)))
(defun tags-find-references ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-find-references 'gtags-find-rtag)))
(defun tags-find-file ()
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-find-file 'gtags-find-file)))
(defun tags-imenu ()
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

(define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
(define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
(define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
(define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
(define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

(define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
(define-key global-map (kbd "M-,") (function tags-find-references-at-point))
(define-key global-map (kbd "M-;") (function tags-find-file))
(define-key global-map (kbd "C-.") (function tags-find-symbol))
(define-key global-map (kbd "C-,") (function tags-find-references))
(define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key global-map (kbd "M-i") (function tags-imenu))
