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

![chip8-001](https://github.com/gabrielelana/chip8.el/blob/6519ba638ebbef793084340a1ccebc252215bc48/assets/chip8-001.png 'Timendus')
![chip8-002](https://github.com/gabrielelana/chip8.el/blob/6519ba638ebbef793084340a1ccebc252215bc48/assets/chip8-002.png 'Timendus')
![chip8-003](https://github.com/gabrielelana/chip8.el/blob/6519ba638ebbef793084340a1ccebc252215bc48/assets/chip8-003.png 'Astro Dodge')
![chip8-004](https://github.com/gabrielelana/chip8.el/blob/6519ba638ebbef793084340a1ccebc252215bc48/assets/chip8-004.png 'Astro Dodge')

## How to install

We are not on MELPA yet, therefore you need to clone this repository

By using straight

```emacs-lisp
(straight-use-package
'(chip8 :type git :host github :repo "gabrielelana/chip8.el"))
```

Manually

- Clone this repository
  ```console
  $ git clone https://github.com/gabrielelana/chip8.el
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

To play games you need ROMs, you can find some of them in [this repository](https://github.com/kripod/chip8-roms)

After `chip8.el` is loaded (see [How to
install](https://github.com/gabrielelana/chip8.el/tree/master#how-to-install))
you run <kbd>M-x chip8</kbd> then Emacs will ask you the file of the ROM to run.

Chip-8 has many extensions and quirks mode, if the ROM doesn't run correctly,
then you can choose to run a specific version of the emulator with the following commands.

- <kbd>M-x chip8-original</kbd>
- <kbd>M-x chip8-modern</kbd>
- <kbd>M-x chip8-superchip</kbd>
- <kbd>M-x chip8-xo-chip</kbd>

## Why

Because Emacs can do anything, so why not.

## How

I've experimented for a while on how to do retro graphic in Emacs to implement
simple games, this led to [a library](https://github.com/gabrielelana/retro.el)
capable to implement 2D retro games with decent frame rate and resolution.

Since the library is not ready to be released, I've vendored only the necessary
code in the emulator.

In the future, when (if?) the library will be release I will remove the code and
have the library as requirement for the emulator.

## Known limitations

Need to implement some XO-Chip features

## License

[MIT](https://github.com/gabrielelana/chip8.el/blob/master/LICENSE)
