# Emacs config

# Setup

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

- `gocode`
- `godef`
- `errcheck`
- `golint`
- `oracle`
- `goimports`

Running `setup.sh` will fetch these for you (in `~/go/go-tools-ws` by default).

# Go

A quick run-through of useful Go features:

## Custom Keybindings

| Binding   | Package          | Description                                                                             |
| ---       | ---              | ---                                                                                     |
| `C-c w`   | `go-helper-mode` | Prompts for a workspace, switches to it, and keeps track of current workspace           |
| `C-c r`   | `go-helper-mode` | Prompts for repository and switches to it. Must be run after `go-helper-goto-workspace` |
| `C-c s s` | `go-helper-mode` | Sets oracle's scope to the current package                                              |
| `C-c s g` | `go-helper-mode` | Sets gocode's lib-path to the current workspace's `pkg/linux_amd64` directory           |
| `C-c i s` | `go-helper-mode` | Installs all subpackages of the current package                                         |
| `C-c g f` | `ginkgo-mode`    | Toggles the containing Ginkgo closure between focussed/non-focussed                     |
| `C-c g p` | `ginkgo-mode`    | Toggles the containing Ginkgo closure between pending/non-pending                       |
| `C-c c`   | `builtin`        | Runs `go build` on the current directory                                                |
| `C-c g r` | `gotest`         | Calls `go run` on the current file                                                      |
| `C-c t p` | `gotest`         | Calls `go test ./...` in the current directory                                          |
| `C-c t f` | `gotest`         | Runs `go test` on the test file corresponding to the current file                       |
| `C-c t t` | `gotest`         | Runs `go test` on the specific test corresponding to the current function               |
| `C-c g e` | `go-errcheck`    | Runs `errcheck` on the current file                                                     |
| `C-c l`   | `golint`         | Runs `golint` in the current file                                                       |
| `C-c C-j` | `go-mode`        | `godef` jump to definition                                                              |
| `C-c C-d` | `go-mode`        | `godef` describe                                                                        |
| Many      | `go-oracle`      | Execute `C-h a oracle` to see available bindings                                        |



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
