;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'starter-kit-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
(if (file-exists-p user-specific-config) (load user-specific-config))


;; [4:28] Ian Eure: You will need these things: http://gist.github.com/457925
;; [4:28] Ian Eure: And http://gist.github.com/457933
;; [4:28] Ian Eure: Then M-x ssh RET hostname RET
;; [4:28] Ian Eure: Instead of M-x shell RET ssh hostname RET
;; [4:37] Benjamin Standefer: is the second gist also in ~/.emacs.d/ssh.el/
;; [4:37] Benjamin Standefer: err, ~/.emacs.d/ssh.el
;; [4:38] Ian Eure: No, you want it in ~/.emacs.d/init.el
;; [4:38] Ian Eure: The first form won't do anything. You want to M-x customize-variable that.
;; [4:38] Ian Eure: Also, I can't stop listening to http://www.youtube.com/watch?v=bnCHCcveteA
;; [4:39] Benjamin Standefer: "first form" being lines 4-14 of the 2nd gist?
;; [4:39] Ian Eure: ssh-directory-tracking-mode
;; [4:39] Ian Eure: Is what you want to customize. Then remove that and the comment above.
;; [4:40] Benjamin Standefer: so that sets it up to be t?
;; [4:40] Ian Eure: When you customize it, you want to set it to t.
;; [4:40] Ian Eure: M-x customize-variable RET ssh-directory-tracking-mode RET
;; [4:41] Ian Eure: And for some reason… Uh.
;; [4:41] Ian Eure: You want to choose "ftp" from that list.
;; [4:41] Ian Eure: Don't ask why, it makes no sense.
;; [4:44] Benjamin Standefer: so the first 2 lines of the 2nd gist I do not paste into init.el
;; [4:44] Ian Eure: Correct.
;; [4:44] Benjamin Standefer: just the hooks
;; [4:44] Benjamin Standefer: ok
;; [4:44] Ian Eure: Yes.

(require 'ssh)

(eval-after-load 'ssh
  '(progn
     (add-hook 'ssh-mode-hook
               (lambda ()
                 (shell-dirtrack-mode t)
                 (setq dirtrackp nil)
                 (setq show-trailing-whitespace nil)))

     (defadvice ssh (around ssh-always-local first activate)
       (let ((default-directory "~/"))
         ad-do-it))))

(eval-after-load 'shell
    '(progn
       (defadvice comint-send-input (before expand-input activate)
         "Expand input before sending"
         (expand-abbrev))
       (add-hook 'shell-mode-hook
                 (lambda ()
                   (ansi-color-for-comint-mode-on)
                   (setq shell-dirtrackp nil)
                   (dirtrack-mode t)
                   (setq show-trailing-whitespace nil)))))


;; Stuff to pimp ibuffer from Paul Lathrop

;;; ibuffer
(require 'ibuffer)
(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Dired" (mode . dired-mode))
         ("Shell Script" (mode . shell-script-mode))
         ("SSH" (mode . ssh-mode))
         ("Ruby" (mode . ruby-mode))
         ("Python" (mode . python-mode))
         ("Puppet" (mode . puppet-mode))
         ("Magit" (name . "magit:"))
         ("Tramp" (name . "*tramp"))
         ("ERC" (mode . erc-mode))
         ("Emacs" (name . "\\*.*\\*")))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;;; init.el ends here
