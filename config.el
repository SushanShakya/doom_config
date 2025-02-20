(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "Agave Nerd Font" :size 18))

(custom-set-faces
 '(hl-line ((t (:background "#444"))))) ;; Change background color

(defun my/reload-config ()
  "Reload `config.el` manually without restarting Doom."
  (interactive)
  (load-file (expand-file-name "~/.config/doom/config.el")))


(map! :leader "h r r" 'my/reload-config)

(map! :leader "h r R" 'doom/reload)

(map! "s-N" '+workspace/new)

(map! "s-P" 'projectile-switch-project)

(map! "s-p" 'projectile-find-file)

(setq display-line-numbers-mode 'relative)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame 'fullscreen 'maximized)))

;; (defun +workspa ce/close-window-or-workspace ()
;;   "Close the selected window. If it's the last window in the workspace, either
;; close the workspace (as well as its associated frame, if one exists) and move to
;; the next."
;;   (interactive)
;;   (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
;;     (if (window-dedicated-p)
;;         (funcall delete-window-fn)
;;       (let ((current-persp-name (+workspace-current-name)))
;;         (cond ((or (+workspace--protected-p current-persp-name)
;;                    (cdr (doom-visible-windows)))
;;                (funcall delete-window-fn))

;;               ((cdr (+workspace-list-names))
;;                (let ((frame-persp (frame-parameter nil 'workspace)))
;;                  (if (string= frame-persp (+workspace-current-name))
;;                      (delete-frame)
;;                    (+workspace/kill current-persp-name))))

;;               ((+workspace-er
                ;; ror "Can't delete last workspace" t)))))))

(defun my/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the workspace, either
close the workspace, but without deleting the frame."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-persp-name)
                   (cdr (doom-visible-windows)))
               (funcall delete-window-fn))

              ((cdr (+workspace-list-names))
               (+workspace/kill current-persp-name))

              ;; Keep this for handling the case of the last workspace
              ((+workspace-error "Can't delete last workspace" t)))))))



(map! "s-w" 'my/close-window-or-workspace)

(defun restore-last-killed-buffer ()
  "Reopen the most recently killed buffer."
  (interactive)
  (let ((recently-killed (car kill-ring)))
    (if recently-killed
        (find-file recently-killed)
      (message "No recently killed buffer to restore."))))


(map! "s-T" 'restore-last-killed-buffer)

(map! "s-q" nil)

(map! "s-q" 'evil-normal-state)



(map! "s-e" '+treemacs/toggle)
(map! "s-b" '+treemacs/toggle)
(map! :leader "e" '+treemacs/toggle)

(with-eval-after-load 'treemacs
  (defun my/enable-linum-in-treemacs ()
    "Enable line numbers in Treemacs."
    (display-line-numbers-mode 1))

  (add-hook 'treemacs-mode-hook #'my/enable-linum-in-treemacs))

(map! "s-u" '+term/toggle)

(map! "s-P" 'execute-extended-command)

(map! "s-L" '+evil/window-vsplit-and-follow)
(map! "s-J" '+evil/window-split-and-follow)

(map! "s-j" 'evil-window-down)
(map! "s-k" 'evil-window-up)
(map! "s-h" 'evil-window-left)
(map! "s-l" 'evil-window-right)

(global-set-key (kbd "s-c") 'kill-ring-save)   ;; Copy
(global-set-key (kbd "s-v") 'yank)             ;; Paste
(global-set-key (kbd "s-x") 'kill-region)      ;; Cut
(global-set-key (kbd "s-a") 'mark-whole-buffer) ;; Select All

(defun my/duplicate-lines-down ()
  "Duplicate the selected region or current line below, keeping the new region selected."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (beg-line (progn (goto-char beg) (line-beginning-position)))
             (end-line (progn (goto-char end) (line-end-position))))
        (let ((text (buffer-substring beg-line end-line)))
          (goto-char end-line)
          (newline)
          (insert text)
          ;; Keep the newly duplicated region selected
          (set-mark end-line)
          (goto-char (+ end-line (length text)))))
    ;; Duplicate single line if no region is selected
    (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
           (pos (point))) ;; Save cursor position
      (end-of-line)
      (newline)
      (insert line)
      ;; Move cursor down to the new line
      (goto-char (+ pos (length line)))
      (set-mark (line-beginning-position)))))

(map! "s-d" 'my/duplicate-lines-down)

(defun my/comment-region-and-keep-selection ()
  "Comment the selected region or the current line, maintaining selection."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (beg-line (progn (goto-char beg) (line-beginning-position)))
             (end-line (progn (goto-char end) (line-end-position))))
        ;; Comment the selected region
        (comment-or-uncomment-region beg-line end-line))
    ;; If no region is selected, comment the current line
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (comment-or-uncomment-region line-start line-end))))

(map! :n "s-/" nil)
(map! :ni "s-/" 'my/comment-region-and-keep-selection)
(map! :v "s-/" 'my/comment-region-and-keep-selection)

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("s-j" . vertico-next)
              ("s-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(map! :leader "T" 'org-babel-tangle)

(map! "s-f" '+evil:swiper)
