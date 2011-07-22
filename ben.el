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


;; Enable winner-mode to switch between window configurations using
;; C-c + arrow
(when (fboundp 'winner-mode)
  (winner-mode 1))


;; Custom variables
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.

  ;; For this to work, your prompt needs to be set to export PS1='\h!\u:\w\$ '
 '(dirtrack-list (quote ("\\((.+)\\)?[a-z0-9]+.[a-z0-9]+:\\(.*\\)[$#] $" 2)))

 '(explicit-bash-args (quote ("--noediting" "-i" "-l")))
 '(ido-mode nil nil (ido))
 '(iswitchb-mode t)
 '(ns-command-modifier (quote meta))
 '(ssh-directory-tracking-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;; Make connecting to servers easier
(defvar ssh-connection-alist
  '())

(setq ssh-connection-alist
  '((sgdev . "ec2-204-236-167-168.us-west-1.compute.amazonaws.com")
    (sgtail . "ec2-204-236-167-168.us-west-1.compute.amazonaws.com")
    (sggate . "ec2-204-236-167-168.us-west-1.compute.amazonaws.com")
    (sgpushpin . "ec2-204-236-167-168.us-west-1.compute.amazonaws.com")
    (sgbulkloader . "ec2-204-236-167-168.us-west-1.compute.amazonaws.com")

    (bendev . "ec2-184-72-11-102.us-west-1.compute.amazonaws.com")))

(defun ssh-convenience ()
  "Make SSH convenience functions."
  (interactive)
  (mapcar (lambda (conn)
            (let ((name (symbol-name (car conn)))
                  (host (cdr conn)))
              (fset (intern name)
                    `(lambda nil
                       ,(format "Connect to %s SSH preset." name)
                       (interactive)
                       (let ((buffer-name ,(format "*ssh-%s*" name)))
                         (if (get-buffer buffer-name)
                             (pop-to-buffer buffer-name)
                           (ssh ,host (pop-to-buffer buffer-name))))))))
          ssh-connection-alist))

(ssh-convenience)


;; Set font to Monaco-12
(set-default-font "Monaco-12")


(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
