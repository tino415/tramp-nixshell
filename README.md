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
