# Emacs config

# Setup

Requires `emacs 25` or greater. See
the
[docs](https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html).

```sh
$ mv ~/.emacs.d ~/dot_emacs.d_backup
$ git clone github.com/garslo/emacs-config ~/.emacs.d
```

`init.el` contains a list of sort-of logical groupings of
configuration. On the initial run you may want to comment out the

```lisp
(load "go")
(load "go-helper-mode")
(load "ginkgo-mode")
```

portions; these will cause some errors without configuration.

Upon first run, Emacs will install those packages listed in
`behavor/packages.el`.

Also, change `go-tools-binary-path` to the workspace `bin/` directory
containing your Go tools.

## Necessary tools for Go

TODO

# Go

A quick run-through of useful Go features:

## Custom Keybindings

### go-mode

While editing a `.go` file, the following commands will work:

| Binding   | Package       | Function                          | Description                                                  |
| ---       | ---           | ---                               | ---                                                          |
| `C-c g r` | `go-mode`     | `go-run`                          | Calls `go run` on the current file                           |
| `C-c c`   | `go-mode`     | anonymous                         | Calls `go build` in the current directory                    |
| `C-c m i` | `gobb`        | `gobb-make-interface`             | Generates an interface for the struct under point            |
| `C-c m b` | `gobb`        | `gobb-make-builder`               | Generates a builder for the struct under point               |
| `C-c m m` | `gobb`        | `gobb-make-builder-and-interface` | Generates a builder and interface for the struct under point |
| `C-c s t` | `ginkgo-mode` | `ginkgo-set-test-dir`             | Prompts for a directory that will serve as the test dir      |
| `C-c t a` | `ginkgo-mode` | `ginkgo-run-all`                  | Runs all ginkgo tests                                        |
| `C-c t t` | `ginkgo-mode` | `ginkgo-run-this-container`       | Runs the test that point is currently in                     |
| `C-c t l` | `ginkgo-mode` | `ginkgo-run-last`                 | Runs the most recently ran container test                    |
| `C-c t p` | `ginkgo-mode` | `ginkgo-toggle-pwd-as-test-dir`   | Toggles using `pwd` as `ginkgo-test-dir`                     |
| `C-c g g` | `ginkgo-mode` | `ginkgo-generate`                 | Runs `ginkgo generate` for the current file                  |
| `C-c g b` | `ginkgo-mode` | `ginkgo-bootstrap`                | Runs `ginkgo bootstrap` in the current directory             |
| many      | `oracle-mode` | many                              | Type `C-h m` in a `.go` buffer to see bindings               |

These commands will work anywhere (in dired buffers, `*scratch*`, etc.)

| Binding   | Package          | Function                        | Description                                                                                      |
| ---       | ---              | ---                             | ---                                                                                              |
| `C-c r`   | `goh-mode`       | `goh-switch-repo`               | Prompts for a repo to switch to                                                                  |
| `C-c w`   | `goh-mode`       | `goh-switch-ws`                 | Prompts for a workspace to switch to                                                             |
| `C-c s w` | `goh-mode`       | `goh-set-pwd-as-ws`             | Sets `pwd` as the current workspace                                                              |
|           | `goh-mode`       | `goh-gen-mocks`                 | Prompts for a package, and calls `go-gen-mocks.sh` on that package                               |
|           | `goh-mode`       | `goh-get-current-package`       | Returns the current package name as a string                                                     |
|           | `goh-mode`       | `goh-set-gocode-lib-path`       | Sets the gocode lib path to match the current workspace. Automatically called by `goh-switch-ws` |
| `C-c s s` | `go-helper-mode` | `go-helper-set-oracle-scope`    | Sets the oracle scope to the current package                                                     |
| `C-c s g` | `go-helper-mode` | `go-helper-set-gocode-lib-path` | Sets the gocode lib path to match the current workspace                                          |
| `C-c i s` | `go-helper-mode` | `go-helper-install-subpackages` | Runs `go install ./...` in the current directory                                                 |


## Auto-complete

Uses `gocode` to auto-complete anything it can find. There is no
special key to bring up the menu, if it finds something that it can
autocomplete the menu comes up automatically.

If `gocode` fails, there's a non-Go-aware `auto-complete` backup that
attempts to complete based on text alone.

### Outside requirements

[`gocode`](https://github.com/nsf/gocode)

## Formatting and import-adding

Uses `goimports` as a substitute for `gofmt`. This is run immediately
before writing the file to disk when saving.

### Outside requirements

[`goimports`](https://github.com/bradfitz/goimports)

## Signature hints

Uses `go-eldoc` to obtain type signatures for functions and displays
them in the message bar.
