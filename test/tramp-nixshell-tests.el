;;; tramp-nixshell-tests.el --- Tests for tramp-nixshell.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'tramp-nixshell)

(ert-deftest tramp-nixshell-test-method-parameters ()
  "Test the method parameter advice for various methods."
  ;; Test nixshell login program
  (let ((vec (make-tramp-file-name :method "nixshell")))
    (should (string-match-p "nix-shell\\|cached-nix-shell"
                           (tramp-get-method-parameter vec 'tramp-login-program))))

  ;; Test nixshellp login args
  (let ((vec (make-tramp-file-name :method "nixshellp" :user "hello,world")))
    (should (equal '(("-p" "hello world"))
                   (tramp-get-method-parameter vec 'tramp-login-args))))

  (kill-new (base32-encode "test.nix"))
  ;; Test nixshellfb login args
  (let* ((encoded "ORSXG5BONZUXQ===")  ; base32 encoding of "test.nix"
         (vec (make-tramp-file-name :method "nixshellfb" :user encoded)))
    (should (equal '(("test.nix"))
                   (tramp-get-method-parameter vec 'tramp-login-args))))

  ;; Test nixshellfa login args with an existing alias
  (let ((tramp-nixshell-file-alist '((test . "/path/to/test.nix")))
        (vec (make-tramp-file-name :method "nixshellfa" :user "test")))
    (should (equal '(("/path/to/test.nix"))
                   (tramp-get-method-parameter vec 'tramp-login-args))))

  ;; Test nixshellfa with file in directory
  (let ((tramp-nixshell-file-alist '())
        (tramp-nixshell-file-dir (make-temp-file "nixshell-test" t))
        (vec (make-tramp-file-name :method "nixshellfa" :user "test")))
    (unwind-protect
        (progn
          (with-temp-file (concat tramp-nixshell-file-dir "/test.nix")
            (insert "{ pkgs ? import <nixpkgs> {} }: pkgs.mkShell { buildInputs = []; }"))
          (should (equal (list (list (concat tramp-nixshell-file-dir "/test.nix")))
                         (tramp-get-method-parameter vec 'tramp-login-args))))
      (delete-directory tramp-nixshell-file-dir t))))

(ert-deftest tramp-nixshell-test-format-method ()
  "Test the tramp-nixshell--format-method function."
  ;; Test with packages
  (should (equal "/nishellp:python,ruby@:"
                 (tramp-nixshell--format-method "python,ruby" nil nil)))

  ;; Test with file
  (let ((file "/path/to/shell.nix"))
    (should (equal (concat "/nixshellfb:" (base32-encode file) "@:")
                   (tramp-nixshell--format-method nil file nil))))

  ;; Test with alias
  (should (equal "/nixshellfa:dev-env@:"
                 (tramp-nixshell--format-method nil nil "dev-env")))

  ;; Test with nothing (default)
  (should (equal "/nixshell::"
                 (tramp-nixshell--format-method nil nil nil))))

(provide 'tramp-nixshell-tests)
;;; tramp-nixshell-tests.el ends here
