[![Open Source Saturday](https://img.shields.io/badge/%E2%9D%A4%EF%B8%8F-open%20source%20saturday-F64060.svg)](https://www.meetup.com/it-IT/Open-Source-Saturday-Milano/)

# Emacs CHIP-8 Emulator

## What

From Wikipedia: "CHIP-8 is an interpreted programming language, developed by
Joseph Weisbecker made on his 1802 Microprocessor. It was initially used on the
COSMAC VIP and Telmac 1800 8-bit microcomputers in the mid-1970s. CHIP-8
programs are run on a CHIP-8 virtual machine. It was made to allow video games
to be more easily programmed for these computers. The simplicity of CHIP-8, and
its long history and popularity, has ensured that CHIP-8 emulators and programs
are still being made to this day."

This is an attempt to write a full emulator able to run in Emacs.

TODO: screenshots

## How to install

We are not on MELPA yet, therefore you need to clone this repository

By using straight

```emacs-lisp
(straight-use-package
'(chip8 :type git :host github :repo "gabrielelana/emacs-chip-8"))
```

Manually

- Clone this repository
  ```console
  $ git clone https://github.com/gabrielelana/emacs-chip-8
  ```
- Inside of Emacs open `chip8.el` file, evaluate buffer with <kbd>M-x
  eval-buffer</kbd> or better, if you have native compilation available,
  <kbd>M-x emacs-lisp-native-compile-and-load</kbd>

### Requirements

- Emacs >= 28.1
- Emacs running in a graphical environment (not in a terminal).
- Emacs with [native compilation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Native-Compilation.html) support.
- You need to have installed on your system [Kreative Square SM](https://www.kreativekorp.com/software/fonts/ksquare/) font.

## How to use

TODO

## Why

TODO

## How

TODO

## Limitations

TODO

## License

[MIT](https://github.com/gabrielelana/emacs-chip-8/blob/master/LICENSE)
