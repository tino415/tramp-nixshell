# tramp-nixshell

Use nix-shell via TRAMP in Emacs

## Installation

### Manual

Download `tramp-nixshell.el` and place it in your `load-path`, then:

```elisp
(require 'tramp-nixshell)
```

### With `use-package`

```elisp
(use-package tramp-nixshell
  :load-path "/path/to/tramp-nixshell"
  :config
  (setq tramp-nixshell-file-dir "~/.shells"))
```

## Usage

This package provides methods to use nix-shell via TRAMP:

- `/nixshell::` - Plain nix-shell
- `/nixshellp:python,ruby@:` - Package-based nix-shell
- `/nixshellfb:BASE32-ENCODED-FILE-PATH@:` - File-based nix-shell
- `/nixshellfa:dev-env@:` - Aliased nix-shell files

You can use the transient interface with `M-x tramp-nixshell`.

## Features

### Performance Optimization

- **Automatic cached-nix-shell detection**: The package automatically uses [cached-nix-shell](https://github.com/xzfc/cached-nix-shell) instead of `nix-shell` when available, which can significantly improve shell startup performance.

If you frequently work with nix-shell environments, installing cached-nix-shell is recommended:

```bash
nix-env -iA nixpkgs.cached-nix-shell
```

The package will automatically detect and use it without any additional configuration.

## Configuration

### Variables

- `tramp-nixshell-file-alist`: An association list mapping alias symbols to nix-shell file paths. This allows you to define custom aliases for your nix shell files.

```elisp
;; Example:
(setq tramp-nixshell-file-alist
      '((python . "/path/to/python-shell.nix")
        (node . "/path/to/nodejs-shell.nix")))
```

- `tramp-nixshell-file-dir`: Directory containing nix-shell files that can be referenced by their basename without the `.nix` extension. When using `/nixshellfa:name@:`, the package will look for `name.nix` in this directory.

```elisp
;; Example:
(setq tramp-nixshell-file-dir "~/.shells")
```

## Development

### Testing

Run tests with:

```bash
make test
```

### Building

Byte-compile the package with:

```bash
make
```
