(require 'tramp)
(require 'base32)


(defvar tramp-nixshell-file-alist '())
(defvar tramp-nixshell-file-dir nil)

(setq tramp-nixshell-file-alist '())

(defconst tramp-nixshell-remote-params
  '((tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-c"))))

(add-to-list 'tramp-methods `("nixshell" ,tramp-nixshell-remote-params))

(add-to-list 'tramp-methods `("nixshellp" ,tramp-nixshell-remote-params))

(add-to-list 'tramp-methods `("nixshellfb" ,tramp-nixshell-remote-params))

(add-to-list 'tramp-methods `("nixshellfa" ,tramp-nixshell-remote-params))

(defun tramp-nixshell--method-parameter-advice (orig-fun vec param)
  (let ((method (tramp-file-name-method vec))
        (user (tramp-file-name-user vec)))
    (cond
     ((and (string-prefix-p "nixshell" method) (eq param 'tramp-login-program))
      (if (executable-find "cached-nix-shell") "cached-nix-shell" "nix-shell"))
     ((and (string= method "nixshellp") (eq param 'tramp-login-args))
      (list (list "-p" (replace-regexp-in-string "," " " user))))
     ((and (string= method "nixshellfb") (eq param 'tramp-login-args))
      (list (list (base32-decode user))))
     ((and (string= method "nixshellfa") (eq param 'tramp-login-args))
      (if-let ((file-path (alist-get (intern user) tramp-nixshell-file-alist)))
          (list (list file-path))
        (let ((path (concat tramp-nixshell-file-dir "/" user ".nix")))
          (if (file-exists-p path)
              (list (list path))
            (error "Unknown alias %s : (%s)" (intern user) path)))))
     (t
      (funcall orig-fun vec param)))))

(advice-add 'tramp-get-method-parameter :around
            #'tramp-nixshell--method-parameter-advice)

(defun tramp-nixshell--alias-completing-read (prompt)
  (completing-read
   prompt
   (append
    (seq-map (lambda (p) (car p))
             tramp-nixshell-file-alist)
    (seq-map (lambda (p) (file-name-sans-extension p))
             (if tramp-nixshell-file-dir
                 (directory-files
                  tramp-nixshell-file-dir
                  nil
                  "\\.nix"))))))

(transient-define-prefix tramp-nixshell ()
  :incompatible '(("--packages=" "--file=" "--alias="))
  ["Arguments"
   ("-p" "Packages (separator:,)" "--packages=")
   ("-f" "File" "--file="
    (lambda (prompt _initial-input _history) (read-file-name prompt)))
   ("-a" "Alias" "--alias="
    (lambda (prompt _initia _history)
      (tramp-nixshell--alias-completing-read prompt)))]
  ["Run"
   ("c" "Reopen current file/change directory"
    tramp-nixshell-reopen-current-file-change-directory)])

(defun tramp-nixshell-reopen-current-file-change-directory (args)
  (interactive (list (transient-args 'tramp-nixshell)))
  (let ((method
         (tramp-nixshell--format-method
          (transient-arg-value "--packages=" args)
          (transient-arg-value "--file=" args)
          (transient-arg-value "--alias=" args))))
    (if-let ((fpath (buffer-file-name (current-buffer))))
        (find-file (concat method fpath))
      (cd (concat method default-directory)))))

(defun tramp-nixshell--format-method (packages file alias)
  (cond
   (packages (concat "/nishellp:" packages "@:"))
   (file (concat "/nixshellfb:" (base32-encode file) "@:"))
   (alias (concat "/nixshellfa:" alias "@:"))
   (t "/nixshell::")))

(provide 'tramp-nixshell)
