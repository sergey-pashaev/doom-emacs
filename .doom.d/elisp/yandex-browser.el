;;; yandex-browser.el --- Dev tools for chromium, yandex browser project

;;; Commentary:

;;; Code:
(require 'projectile)
(require 'f)
(require 'cl-lib)
(require 'url-util)
(require 'subr-x)
(require 'magit)

;; common
(defun yb-read-branch ()
  "Return current git branch.
Read branch name from minibuffer if called with prefix argument."
  (if current-prefix-arg
      (read-from-minibuffer "Branch: " (magit-get-current-branch))
    (magit-get-current-branch)))

(defun yb-read-symbol ()
  "Read symbol name from minibuffer."
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        (read-string "Symbol: " symbol))))

(defun yb-symbol-or-line ()
  "Return symbol or line."
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        symbol
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun yb-buffer-path ()
  "Return current buffer filename or default directory."
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun yb-put-to-clipboard (str)
  "Put STR into clipboard & kill ring."
  (when str
    (kill-new str)))

(defun yb-org-link (url text)
  "Return org link to URL with TEXT."
  (format "[[%s][%s]]" url text))

(defun yb-what-project (&optional root)
  "Return type of browser project for ROOT path.
Returns 'chromium, 'yandex-browser or nil if other."
  (let ((path (if root
                  root
                (projectile-project-root))))
    (cond
     ((chromium-project-path-p path)
      'chromium)
     ((yb-project-path-p path)
      'yandex-browser)
     (t
      nil))))

(defun yb-complete-dir (dir prompt)
  "Complete dir name inside DIR with PROMPT."
  (let ((dir-contents (directory-files dir)))
    (ido-completing-read (concat prompt " ")
                         (cl-remove-if-not (lambda (name)
                                             (file-directory-p (concat dir name)))
                                           dir-contents))))

;; code search
(defconst chromium-repo-path (expand-file-name "~/workspace/ya/chromium/src/")
  "Chromium project root path.")

(defconst chromium-cs-url-format
  "https://cs.chromium.org/search/?q=%s&sq=package:chromium&type=cs"
  "Chromium code search format, %s = SEARCH TERM.")

(defconst chromium-cs-symbol-reference-url-format
  "https://source.chromium.org/chromium/chromium/src/+/%s:%s;?q=%s&ss=chromium"
  "Chromium code search symbol reference format.")

(defconst chromium-cs-line-reference-url-format
  "https://source.chromium.org/chromium/chromium/src/+/%s:%s;l=%d?ss=chromium"
  "Chromium code search line reference format.")

(defun chromium-make-symbol-reference-url (branch filepath line symbol)
  "Construct url to SYMBOL at FILEPATH at BRANCH."
  (if symbol
      (format chromium-cs-symbol-reference-url-format branch filepath symbol)
    (format chromium-cs-line-reference-url-format branch filepath line)))

(defun chromium-project-path-p (path)
  "Whether given PATH is chromium project path."
  (string-prefix-p chromium-repo-path (expand-file-name path)))

(defun chromium-buffer-relative-path ()
  "Return current buffer path relative to chromium project root."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (chromium-project-path-p root))
        (let* ((abs-path (yb-buffer-path))
               (rel-path (substring abs-path (length root))))
          (if (s-starts-with? "src/" rel-path)
              (substring rel-path (length "src/"))
            rel-path))
      (user-error "Not in chromium project"))))

(defun chromium-symbol-reference ()
  "Return reference to current symbol as url."
  (interactive)
  (chromium-make-symbol-reference-url (yb-read-branch)
                                      (chromium-buffer-relative-path)
                                      (line-number-at-pos (point))
                                      (yb-read-symbol)))

(defun chromium-copy-symbol-reference ()
  "Copy current symbol reference to kill ring."
  (interactive)
  (let ((url (chromium-symbol-reference)))
    (yb-put-to-clipboard url)
    (message "Chromium url copied.")))

(defun chromium-copy-symbol-reference-org ()
  "Copy current symbol reference to kill ring as org link."
  (interactive)
  (let ((text (thing-at-point 'symbol))
        (url (chromium-symbol-reference)))
    (yb-put-to-clipboard (yb-org-link url text))
    (message "Chromium url for org-mode copied.")))

(defun chromium-browse-symbol-reference ()
  "Open current symbol in browser."
  (interactive)
  (browse-url-default-browser (chromium-symbol-reference)))

;; line reference
(defconst yb-project-url "https://bitbucket.browser.yandex-team.ru/projects/STARDUST/repos/browser")

(defconst yb-repo-path (expand-file-name "~/workspace/ya/browser"))

(defconst yb-repo-line-reference-url-format "%s/browse/%s?at=refs%%2Fheads%%2F%s#%d"
  "Line reference in repo browser url format.
1. project url string
2. filepath string
3. branch string
4. line number")

(defun yb-make-line-reference-url (branch filepath line)
  "Construct url to LINE at FILEPATH at BRANCH."
  (format yb-repo-line-reference-url-format
          yb-project-url
          filepath
          (url-hexify-string branch)
          line))

(defun yb-project-path-p (path)
  "Whether given PATH is yandex-browser project path."
  (string-prefix-p yb-repo-path (expand-file-name path) t))

(defun yb-buffer-relative-path ()
  "Return current buffer path relative to browser project root."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium)
      (chromium-buffer-relative-path))
     ((eq project 'yandex-browser)
      (yb-yandex-buffer-relative-path)))))

(defun yb-yandex-buffer-relative-path ()
  "Return current buffer path relative to browser project root."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (yb-project-path-p root))
        (let* ((abs-path (yb-buffer-path))
               (rel-path (substring abs-path (length root))))
          rel-path)
      (user-error "Not in yandex-browser project"))))

(defun yb-line-reference ()
  "Return reference to current line as url."
  (interactive)
  (yb-make-line-reference-url (yb-read-branch)
                              (yb-yandex-buffer-relative-path)
                              (line-number-at-pos (point))))

(defun yb-copy-line-reference ()
  "Copy current line reference to kill ring."
  (interactive)
  (let ((url (yb-line-reference)))
    (yb-put-to-clipboard url)
    (message "Url copied.")))

(defun yb-copy-line-reference-org ()
  "Copy current line reference to kill ring as org link."
  (interactive)
  (let ((text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (url (yb-line-reference)))
    (yb-put-to-clipboard (yb-org-link url text))
    (message "Url for org-mode copied.")))

(defun yb-browse-line-reference ()
  "Open current line in browser."
  (interactive)
  (browse-url-default-browser (yb-line-reference)))

(defun yb-copy-reference ()
  "Copy current line/symbol reference to kill ring."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium)
      (chromium-copy-symbol-reference))
     ((eq project 'yandex-browser)
      (yb-copy-line-reference)))))

(defun yb-copy-reference-org ()
  "Copy current line/symbol reference to kill ring as org link."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium) (chromium-copy-symbol-reference-org))
     ((eq project 'yandex-browser) (yb-copy-line-reference-org)))))

(defun yb-browse-reference ()
  "Browse current line/symbol at repo web interface."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium) (chromium-browse-symbol-reference))
     ((eq project 'yandex-browser) (yb-browse-line-reference)))))

(defhydra yb-reference-hydra (:hint t)
  "Current line operations"
  ("c" yb-copy-reference "copy url")
  ("o" yb-copy-reference-org "copy url for org-mode")
  ("g" yb-browse-reference "open url in browser"))

;; yandex buttons
(defconst yb-tracker-format "https://st.yandex-team.ru/%s"
  "Ticket URL format.
1. %s = ticket id")

(defconst yb-bitbucket-pr-format
  "https://bitbucket.browser.yandex-team.ru/projects/STARDUST/repos/browser/pull-requests/%s"
  "Bitbucket pull request URL format.
1. %s = pull request number")

(defun yb-browse-button-url (button)
  "Browse BUTTON."
  (browse-url (button-get button 'url)))

(defun yb-make-browse-button (beg end url)
  "Make button with URL from BEG to END positions in buffer."
  (make-button beg end
               'action 'yb-browse-button-url
               'follow-link t
               'url url
               'help-echo url))

(defun yb-match-browse-button (match url)
  "Make button with URL out of MATCH."
  (yb-make-browse-button (match-beginning match) (match-end match) url))

(defun yb-make-browse-buttons (beg end)
  "Add buttons for supported services in region from BEG to END."
  (interactive "r")
  ;; StarTrek
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search nil))
      (while (re-search-forward "[A-Z]\\{2,\\}-[0-9]+" end t)
        (let ((url (format yb-tracker-format (match-string 0))))
          (yb-match-browse-button 0 url)))))
  ;; pull requests
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search nil))
      (while (re-search-forward "pull request #\\([0-9]+\\)" end t)
        (let ((url (format yb-bitbucket-pr-format (match-string 1))))
          (yb-match-browse-button 0 url))))))

;;;###autoload
(defun magit-insert-revision-message--yandex-buttons (f &rest args)
  (let ((beg (point)))
    (apply f args)
    (yb-make-browse-buttons beg (point))))

;;;###autoload
(with-eval-after-load 'magit-diff
  (advice-add 'magit-insert-revision-message :around
              'magit-insert-revision-message--yandex-buttons))

(with-eval-after-load 'magit-status
  (advice-add 'magit-insert-revision-message :around
              'magit-insert-revision-message--yandex-buttons))

;; gn refs buffer
(defconst yb-depot-tools-path (expand-file-name "~/workspace/ya/depot_tools/"))
(defconst yb-gn-path (expand-file-name "gn" yb-depot-tools-path))
(defconst yb-gn-refs-buffer-name "*yb-gn-refs*")
(defconst yb-yakuza-path (expand-file-name "yakuza" yb-depot-tools-path))

(defvar yb-current-build-profile "")

(defun yb-build-command (build-profile target)
  "Return build command for TARGET in BUILD-PROFILE."
  (format "ninja -C %s -j 50 %s" build-profile target))

(defun yb-build-target (build-profile target)
  "Compile TARGET for BUILD-PROFILE."
  (interactive)
  (let ((default-directory (concat (projectile-project-root) "src/")))
    (compilation-start (yb-build-command build-profile target))))

(define-compilation-mode gn-refs-mode "gn-refs"
  (set (make-local-variable 'compilation-disable-input) t)
  (add-hook 'compilation-filter-hook 'yb-gn-refs-filter nil t))

(defun yb-gn-refs-filter ()
  "Handle match highlighting escape sequences inserted by the grep process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight target matches.
        (while (re-search-forward "//\\([a-zA-Z_/]+\\)?:\\([a-zA-Z_]+\\)" (point-max) t)
            (let* ((component (match-string 1))
                   (binary (match-string 2))
                   (target (if component
                               (format "%s:%s" component binary)
                             binary)))
              (yb-gn-refs-match-button 0 (yb-build-command yb-current-build-profile target)
                                       (concat (projectile-project-root) "src/"))
              (cl-incf grep-num-matches-found)))))))

(defun yb-gn-refs ()
  "Run gn refs for current source file.
List all gn refs that using current file in *yb-gn-refs* buffer."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (yb-project-path-p root))
        (let* ((file (psv/buffer-file-path))
               (dir (concat root "src/"))
               (build-dir (substring (yb-select-build-profile) (length dir)))
               (default-directory dir) ; used by process as default directory
               (cmd (format "%s refs %s --all %s"
                            yb-gn-path
                            build-dir
                            file)))
          (setq yb-current-build-profile build-dir)
          (compilation-start cmd 'gn-refs-mode))
      (user-error "Not in yandex-browser project"))))

(defun yb-gn-refs-button-action (button)
  "Action function for BUTTON in gn-refs buffer."
  (psv/put-to-clipboard (button-get button 'cmd))
  (message "Copied: %s for %s"
           (button-get button 'cmd)
           (button-get button 'dir))
  ;(yb-build-target (button-get button 'cmd))
  )

(defun yb-gn-refs-make-button (beg end cmd dir)
  "Make button from BEG to END with action w/ CMD & DIR."
  (make-button beg end
               'action 'yb-gn-refs-button-action
               'follow-link t
               'cmd cmd
               'dir dir
               'help-echo cmd))

(defun yb-gn-refs-match-button (match cmd dir)
  "Create button out of MATCH with given CMD & DIR as action."
  (yb-gn-refs-make-button (match-beginning match) (match-end match) cmd dir))

;; yb trace
(defconst yb-trace-buffer-name "*yb-trace*")
(defvar yb-trace-frames '() "Currently collected trace frames.")

(cl-defstruct (yb-trace-frame
               (:constructor yb-trace-make-frame
                             (project-type
                              project-root
                              filepath
                              line-text
                              line-num
                              &optional
                              branch
                              note
                              symbol)))
  "Trace frame struct."
  project-type
  project-root
  filepath
  line-text
  line-num
  branch
  note
  symbol)

(defun yb-trace-get-frame ()
  "Return yb-trace-frame for current line."
  (let ((project-type (yb-what-project))
        (project-root (projectile-project-root))
        (filepath (yb-buffer-relative-path))
        (line-text (string-trim
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
        (line-num (line-number-at-pos (point)))
        (cur-symbol
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol))))
    (yb-trace-make-frame
     project-type
     project-root
     filepath
     line-text
     line-num
     (magit-get-current-branch)
     nil
     cur-symbol)))

(defun yb-trace-make-link (frame)
  "Convert FRAME struct into `org-mode' link."
  (format "** [[%s][|YANDEX|]] [[%s][|CHROMIUM|]] [[file:%s::%s][%s]]\n"
          ;; remote yandex link
          (yb-make-line-reference-url
           (yb-trace-frame-branch frame)
           (yb-trace-frame-filepath frame)
           (yb-trace-frame-line-num frame))
          ;; remote chromium link
          (chromium-make-symbol-reference-url
           "master" ;; todo: fix it somehow
           (substring (yb-trace-frame-filepath frame) (length "src/"))
           (yb-trace-frame-line-num frame)
           (yb-trace-frame-symbol frame)
           )
          ;; local link
          (concat (yb-trace-frame-project-root frame)
                  (yb-trace-frame-filepath frame))
          (yb-trace-frame-line-text frame)
          ;; local text
          (yb-trace-frame-line-text frame)
          ))

(defun yb-trace-clear ()
  "Clear current trace."
  (interactive)
  (setq yb-trace-frames '()))

(defun yb-trace-start (name)
  "Start tracing with NAME."
  (interactive "s Name of trace: ")
  (if (get-buffer yb-trace-buffer-name)
      (kill-buffer yb-trace-buffer-name))
  (let ((buf (get-buffer-create yb-trace-buffer-name)))
    (with-current-buffer buf
      (org-mode)
      (org-insert-heading)
      (insert (format "%s\n" name)))))

;; todo: what to do with local uncommited changes in local branch?
(defun yb-trace-add ()
  "Add current line reference to trace buffer."
  (interactive)
  (let ((buf (get-buffer yb-trace-buffer-name))
        (frame (yb-trace-get-frame)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (yb-trace-make-link frame))
        (goto-char (point-max))))))

(defhydra yb-trace-action-hydra (:hint t)
  "trace actions"
  ("b" yb-trace-start "begin")
  ("a" yb-trace-add "add")
  ("c" yb-trace-clear "clear"))

;; include statement
(defun yb-buffer-relative-path-include ()
  "Return buffer relative path (and cut \"src/\" if needed."
  (let ((project (yb-what-project)))
    (cond ((eq project 'yandex-browser)
           (substring (yb-buffer-relative-path)
                      (length "src/")))
          ((eq project 'chromium)
           (chromium-buffer-relative-path)))))

(defun yb-make-include-statement ()
  "Generate include statement for current file."
  (format "#include \"%s\"" (yb-buffer-relative-path-include)))

(defun yb-copy-include-statement ()
  "Put the current file include statement into clipboard."
  (interactive)
  (let ((include (yb-make-include-statement)))
    (yb-put-to-clipboard include)
    (message "Copied: %s" include)))

;; jump to file in other project
(defun yb-select-other-project ()
  "Select other project root."
  (projectile-completing-read
   "Switch to file in project: "
   (projectile-relevant-known-projects)))

(defun yb-select-other-project-file ()
  "Return path to file in other project."
  (let* ((from-project (projectile-project-root))
         (from-project-type (yb-what-project from-project))
         (from-path (yb-buffer-relative-path))
         (to-project (yb-select-other-project))
         (to-project-type (yb-what-project to-project)))
    (cond
     ;; chromium -> yandex-browser (add src/)
     ((and (eq from-project-type 'chromium)
           (eq to-project-type 'yandex-browser))
      (concat to-project "src/" from-path))
     ;; yandex-browser -> chromium (cut src/)
     ((and (eq from-project-type 'yandex-browser)
           (eq to-project-type 'chromium))
      (concat to-project (substring from-path (length "src/"))))
     ;; others -> do nothing
     (t
      (concat to-project from-path)))))

(defun yb-visit-file-other-project ()
  "Visit file in other project with same relative path as current buffer.
With passed universal argument it visits file in other window."
  (interactive)
  (let ((position (point))
        (line-num (line-number-at-pos (point)))
        (from-path (yb-buffer-relative-path))
        (to-path (yb-select-other-project-file)))
    (if (f-exists? to-path)
        (if current-prefix-arg
            ;; visit in other window
            (progn
              (delete-other-windows)
              (split-window-right)
              (other-window 1)
              (find-file to-path)
              (goto-line line-num)
              ;; (goto-char position)
              (recenter-top-bottom)
              (other-window 1))
          ;; visit in current window
          (progn
            (find-file to-path)
            (goto-line line-num)
            ;; (goto-char position)
            (recenter-top-bottom)))
      (user-error (format "file [%s] doesn't exist" to-path)))))

(defun yb-diff-file-other-project ()
  "Diff current file with file on same path in other project."
  (interactive)
  (let ((from-path (yb-buffer-relative-path))
        (from-project (projectile-project-root))
        (to-path (yb-select-other-project-file)))
    (if (f-exists? to-path)
        (ediff (concat from-project from-path) to-path)
      (user-error (format "file [%s] doesn't exist" to-path)))))

(bind-key "C-c >" 'yb-visit-file-other-project)

;; ticket dir
(defun yb-guess-ticket ()
  "Guess current ticket from branch."
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (m (string-match "[A-Z]\\{2,\\}-[0-9]+" branch)))
    (match-string 0 branch)))

(defun yb-goto-ticket-notes ()
  "Go to ticket notes notes."
  (interactive)
  (let* ((dir (expand-file-name "~/workspace/ya/notes/"))
         (contents (directory-files dir))
         (ticket (ido-completing-read "Ticket? "
                                      (cl-remove-if-not (lambda (name) (file-directory-p (concat dir name))) ; include directories only
                                                        contents)
                                      nil
                                      nil
                                      (yb-guess-ticket)))
         (path (concat dir ticket))
         (notes (concat path "/notes.org")))
    (when (not (file-directory-p path))
      (dired-create-directory path))
    (when (not (file-exists-p notes))
      (f-write-text "" 'utf-8 notes))
    (find-file notes)
    (goto-char (point-max))))

;; compile single file
(defun yb-select-build-profile ()
  "Select build profile of current project."
  (interactive)
  (let* ((root (projectile-project-root))
         (out-dir (concat root "src/out/")))
    (if (f-exists? out-dir)
        (concat out-dir (yb-complete-dir out-dir
                                         "Profile:")))))

(defun yb-compile-single-file ()
  "Compile current file."
  (interactive)
  (let* ((build-dir (yb-select-build-profile))
         (path (substring (yb-buffer-relative-path) (length "src/")))
         (cmd (format "%s -i all -k 10000 -C %s ../../%s^"
                      yb-yakuza-path
                      build-dir
                      path)))
    (let ((default-directory (concat (projectile-project-root) "src/")))
      (compilation-start cmd))))

;; hydra
(defhydra yb-tools (:hint t)
  "yandex-browser tools"
  ("r" yb-reference-hydra/body "line/symbol reference operations" :exit t)
  ("g" yb-gn-refs "gn refs")
  ("c" yb-compile-single-file "compile file")
  ("t" yb-trace-action-hydra/body "trace" :exit t)
  ("o" yb-visit-file-other-project "other project")
  ("n" yb-goto-ticket-notes "ticket notes"))

(bind-key "C-c y" 'yb-tools/body)

(provide 'yandex-browser)
;;; yandex-browser.el ends here
