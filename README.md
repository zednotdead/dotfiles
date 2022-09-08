# My dotfiles

## IMPORTANT DISCLAIMER

**This is as much for me as it is for anyone else reading this README.**

If you use zsh, you need to have a `.zprofile` file instead of `.profile`, or else the GUI apps won't have access to your environment variables, such as those set by asdf.

## Dependencies

Everything else will be installed automatically, but you need `zsh` and `make`.

## Installation

To install the dotfiles, run:

```bash
$ make install
```

To uninstall the dotfiles, run:

```bash
$ make clean
```

If you are on a macOS device, the commands are respectively:

```bash
$ make install
$ make clean
```
