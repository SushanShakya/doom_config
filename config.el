(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "Agave Nerd Font" :size 18))

(setq-default line-spacing 0.4)

(custom-set-faces
 '(hl-line ((t (:background "#444"))))) ;; Change background color

(defun my/reload-config ()
  "Reload `config.el` manually without restarting Doom."
  (interactive)
  (load-file (expand-file-name "~/.config/doom/config.el")))


(map! :leader "h r r" 'my/reload-config)

(map! :leader "h r R" 'doom/reload)

(map! "s-N" '+workspace/new)

(map! "s-r" 'projectile-switch-project)

(map! "s-p" 'projectile-find-file)

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(map! :n "<escape>" 'keyboard-escape-quit)
(map! :i "<escape>" 'evil-normal-state)
(map! :v "<escape>" 'evil-normal-state)

(setq display-line-numbers-type 'relative)  ;; Enable relative line numbers
(global-display-line-numbers-mode 1)        ;; Enable globally

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
  (let ((delete-window-fn #'centaur-tabs-buffer-close-tab))
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



(map! "s-w" 'centaur-tabs--kill-this-buffer-dont-ask)

(global-set-key (kbd "s-W") '+workspace/kill)

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

;; (global-set-key (kbd "s-c") 'kill-ring-save)   ;; Copy
;; (global-set-key (kbd "s-v") 'yank)             ;; Paste
;; (global-set-key (kbd "s-x") 'kill-region)      ;; Cut
;; (global-set-key (kbd "s-a") 'mark-whole-buffer) ;; Select All

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

(setq org-babel-ditaa-jar-path "~/Documents/ditaa.jar")

(map! :leader "T" 'org-babel-tangle)

(map! :i "TAB" nil)

(map! "s-f" 'swiper)

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-enable nil)

(map! :i "s-j" #'company-select-next)
(map! :i "s-k" #'company-select-previous)

(map! :leader "jb" #'better-jumper-jump-backward)
(map! :leader "jd" '+lookup/definition)
(map! :leader "tc" 'treemacs-collapse-all-projects)

(after! lsp-mode
  (setq lsp-eslint-auto-fix-on-save t)  ;; ESLint auto-fix
  (setq lsp-format-on-save t)  ;; Format with LSP
  (add-hook 'before-save-hook #'lsp-organize-imports)
  (add-hook 'before-save-hook #'lsp-format-buffer))

(setq centaur-tabs-height 32)

(setq centaur-tabs-style "wave")

(setq centaur-tabs-set-bar 'none)
(setq x-underline-at-descent-line t)

(map! "M-n" 'centaur-tabs-forward)
(map! "M-p" 'centaur-tabs-backward)

(map! :nvm "L" 'centaur-tabs-forward)             
(map! :nvm "H" 'centaur-tabs-backward)            

(setq transient-mark-mode t)

(defvar flutter/daemon nil
  "Variable to store reference to Flutter Daemon which will be used to launch devices")

(defvar flutter-daemon-buffer "*Flutter Daemon*")

(defun flutter/start-daemon ()
  "Starts the Flutter Daemon if it's not running"
  (unless (and flutter/daemon                     
               (process-live-p flutter/daemon))
    (setq flutter/daemon
          (make-process
           :name "flutter-daemon"
           :buffer flutter-daemon-buffer          
           :command '("flutter" "daemon")
           :coding 'utf-8
           :noquery t
           :filter 'flutter/daemon-filter))       
    (message "Flutter daemon started.")))

(defun flutter/stop-daemon ()
  "Stops the Flutter Daemon if it's running"
  (when (and flutter/daemon
               (process-live-p flutter/daemon))
    (kill-process flutter/daemon)
    (kill-buffer flutter-daemon-buffer)
    (setq flutter/daemon nil)
    (message "Flutter daemon stopped.")))

(defun flutter/daemon-filter (proc output)
  "Handle Flutter daemon messages from PROC, processing OUTPUT."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output)))

(defun flutter/daemon-send-message (json-command) 
  "Send a JSON command to the Flutter daemon."
  (when (and flutter/daemon (process-live-p flutter/daemon))
    (process-send-string flutter/daemon (concat json-command "\n"))))

(defun flutter/request-emulator-list ()           
  "Request available devices and emulators from Flutter daemon."
  (flutter/daemon-send-message                    
   (json-encode `[(:id 1 :method "emulator.getEmulators")])
   )                                              
  )

(setq global-emulator-list nil)
(setq devices nil)

(defun flutter/parse-emulator-list-from-daemon ()
  "Parse the Emulator list from daemon"
  (with-current-buffer flutter-daemon-buffer
    (goto-char (point-min))
    (let (devices)
      (while (re-search-forward "\\({.*}\\)" nil t)
        (let* ((json (json-read-from-string (match-string 1)))
               (result (alist-get 'result json)))
          (when result
            (setq devices result))))
      devices)))

(defun flutter/emulators-from-daemon ()
  "Parse the device and emulator list from the last line of the Flutter daemon buffer."
  (flutter/start-daemon)
  (flutter/request-emulator-list)
  (sleep-for 0.6) ;; HACK: Required because getting data from flutter daemon is asynchronous (Wait for 600ms so that Flutter Daemon can give us the required information)
  (setq devices (flutter/parse-emulator-list-from-daemon))
  (flutter/stop-daemon)
  devices
)

(defun flutter/emulators ()
  (if global-emulator-list
      global-emulator-list
    (setq global-emulator-list (flutter/emulators-from-daemon))
    global-emulator-list)
)

(defun test/emulators ()
  (interactive)
  (setq devices (flutter/emulators))
  (message (prin1-to-string devices)))

(defun test/emulators-from-daemon ()
  (interactive)
  (setq devices (flutter/emulators-from-daemon))
  (message (prin1-to-string devices)))

(defun flutter/run-emulator ()
  "Show a menu to select a Flutter device or emulator."
  (interactive)
  (let* ((devices (flutter/emulators))
         (choices (mapcar (lambda (d)
                            (cons (alist-get 'name d) (alist-get 'id d)))
                          devices))
         (selection (completing-read "Select a Flutter device: " choices)))
    (when selection
      (message "Selected device: %s" selection)
      (shell-command (format "flutter emulators --launch %s" selection))
      )
    )
  )

(map! :leader "d" 'flutter/run-emulator)
