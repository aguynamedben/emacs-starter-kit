;; Set font to Monaco-11
(set-default-font "Monaco-11")

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


;; magit for awesome git integration!
;; http://github.com/philjackson/magit
(require 'magit)


;; Balance windows
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03686.html
(defun balance-windows (&optional horizontally)
  "Make all visible windows on the current frame the same size (approximately).
If optional prefix arg is not given, \"same size\" is same height.
When prefix arg is given,  \"same size\" is same width."
  (interactive "P")
  (let* (count size w cmjr resize max rpt
         (edge (if horizontally 0 1))  ;; Minor field to sort by 0=LEFT, 1=TOP
         (mjr (- 1 edge))              ;; Major field to sort
         (far (+ 2 edge))              ;; far edge (right/bottom)
         (windows nil)                 ;; list of windows
         (ix 0)
         nwin                          ;; number of windows
         (pass 1)                      ;; pass number
         (curw (selected-window))      ;; selected window (to return to)
         )
    ;; Build and sort list of all windows on frame
       (save-window-excursion
           (walk-windows (function (lambda (w)
                               (let ((ltrb (window-edges w)))
                                   (setq windows (cons (list
                                       (nth mjr  ltrb)
                                       (nth edge ltrb)
                                       (nth far  ltrb)
                                       w) windows)))))
                         'nomini)
           (setq windows (sort windows (lambda (e1 e2)
                                         (if (< (nth 0 e1) (nth 0 e2))
                                               t
                                           (if (= (nth 0 e1) (nth 0 e2))
                                               (if (< (nth 1 e1) (nth 2 e2))
                                                   t)))))))
       (setq nwin (length windows))
       ;; add 1 extra entry (for while check)
       (setq windows (append windows '((-1 -1 -1 nil))))

       (while (< ix nwin)                      ; walk on all (sorted) windows
           (setq count 0)                      ; number of windows in 1 column 
(or row)
           (setq cmjr (car (nth ix windows)))  ; column / raw identification
           (while (= cmjr (car (nth (+ count ix) windows)))    ; same
               (setq count (1+ count)))        ; count them
           (if (= count 1)                     ; only one window in this 
column/row
               (setq ix (1+ ix))               ; skip it
             ; compute and resize windows
             (setq size (if (= pass 1)         ; on pass 1 the saved edges have 
not changed
                          (- (nth 2 (nth (1- (+ count ix)) windows))
                             (nth 1 (nth ix windows)))
                        ;; previous changes may changed the window edges
                        (- (nth far (window-edges (nth 3 (nth (1- (+ count ix)) 
windows))))
                           (nth edge (window-edges (nth 3 (nth ix windows)))))))
             (setq size (/ (+ size count -1) count)) ; average window size

             (setq max (+ ix count))           ; index of next column/row
             ;; the resizing loop must be done twice
             ;; because later change may resize previous window
             (setq rpt 2)
             (while (> rpt 0)
               (setq rpt (1- rpt))
               (while (< ix max)
                 (setq w (nth 3 (nth ix windows)))
                 (setq resize (- size (- (nth far (window-edges w))
                                         (nth edge (window-edges w)))))
                 ; don't resize by 1 character
                 (if (or (> resize 1)
                         (< resize -1))
                     (progn
                       (select-window w)       ; window to work on
                       (enlarge-window resize horizontally)))
                 (setq ix (1+ ix)))
               (setq ix (- max count)))
             (setq ix max)
             (setq pass 2)))
           (select-window curw)))
(global-set-key (kbd "C-c w") 'balance-windows-area)


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


;; Enable line number and column number display
(setq line-number-mode t)
(setq column-number-mode t)

;; Shortcut for rename-buffer
(global-set-key (kbd "C-c b") 'rename-buffer)
