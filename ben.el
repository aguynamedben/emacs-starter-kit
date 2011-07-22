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
