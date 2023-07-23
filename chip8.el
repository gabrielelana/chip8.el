;;; chip8.el --- A CHIP-8 emulator written in EmacsLisp running in Emacs -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: http://github.com/gabrielelana/chip8.el
;; Keywords: CHIP-8, game, games, emulator

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A CHIP-8 emulator written in EmacsLisp running in Emacs

;;; Code:

(require 'cl-lib)
(require 'retro)

;;; TODO: remove
(defun chip8/example ()
  "This is an example."
  2)

;;; TODO: will be variables in chip8 data structure when support for SUPER-CHIP
(defconst chip8/SCREEN-WIDTH 64)
(defconst chip8/SCREEN-HEIGHT 32)

(defconst chip8/KEY-RELEASE-TIMEOUT 0.15
  "Time between the keypress event and the simulation of the keyrelease event.")

(defconst chip8/RAM-SIZE 4096
  "Dimension of the RAM.")

(defconst chip8/FRAME-DURATION 0.03
  "Duration of a single frame in emuation.")

(defconst chip8/INSTRUCTIONS-PER-FRAME 9
  "Number of instructions to execute per frame.")

;;; TODO: make the theme configurable
(defconst chip8/COLORS
  ;; [(#x00 #x00 #x00) (#xFF #xFF #xFF)]
  [(#x99 #x66 #x01) (#xFF #xCC #x01)]
  "List of colors supported by the emulator, they are indexed starting from zero.")

(defconst chip8/BUFFER-NAME "*CHIP8-EMULATOR*"
  "The name of the buffer used to show the emulator.")

(defconst chip8/FONT-ADDRESS #x50
  "Address where to find/load default font in RAM.")

(defconst chip8/ROM-ADDRESS #x200
  "Address where to find/load ROM to execute in RAM.")

(defconst chip8/FONT [ #xF0 #x90 #x90 #x90 #xF0 ; 0
                       #x20 #x60 #x20 #x20 #x70 ; 1
                       #xF0 #x10 #xF0 #x80 #xF0 ; 2
                       #xF0 #x10 #xF0 #x10 #xF0 ; 3
                       #x90 #x90 #xF0 #x10 #x10 ; 4
                       #xF0 #x80 #xF0 #x10 #xF0 ; 5
                       #xF0 #x80 #xF0 #x90 #xF0 ; 6
                       #xF0 #x10 #x20 #x40 #x40 ; 7
                       #xF0 #x90 #xF0 #x90 #xF0 ; 8
                       #xF0 #x90 #xF0 #x10 #xF0 ; 9
                       #xF0 #x90 #xF0 #x90 #x90 ; A
                       #xE0 #x90 #xE0 #x90 #xE0 ; B
                       #xF0 #x80 #x80 #x80 #xF0 ; C
                       #xE0 #x90 #x90 #x90 #xE0 ; D
                       #xF0 #x80 #xF0 #x80 #xF0 ; E
                       #xF0 #x80 #xF0 #x80 #x80 ; F
                       ]
  "Default font loaded as sprites in CHIP-8 RAM.")

(defvar chip8--current-rom-filename nil
  "File path of ROM loaded in the running CHIP-8 emulator.")

(defvar chip8--current-quirks nil
  "Current set of quirks used by CHIP-8 emulator..")

(defvar chip8--current-instance nil
  "Current instance of running CHIP-8 emulator.")

(defvar chip8-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'chip8-quit)
    (define-key map (kbd "0") #'(lambda () (interactive) (chip8--key-press #x0)))
    (define-key map (kbd "1") #'(lambda () (interactive) (chip8--key-press #x1)))
    (define-key map (kbd "2") #'(lambda () (interactive) (chip8--key-press #x2)))
    (define-key map (kbd "3") #'(lambda () (interactive) (chip8--key-press #x3)))
    (define-key map (kbd "4") #'(lambda () (interactive) (chip8--key-press #x4)))
    (define-key map (kbd "5") #'(lambda () (interactive) (chip8--key-press #x5)))
    (define-key map (kbd "6") #'(lambda () (interactive) (chip8--key-press #x6)))
    (define-key map (kbd "7") #'(lambda () (interactive) (chip8--key-press #x7)))
    (define-key map (kbd "8") #'(lambda () (interactive) (chip8--key-press #x8)))
    (define-key map (kbd "9") #'(lambda () (interactive) (chip8--key-press #x9)))
    (define-key map (kbd "a") #'(lambda () (interactive) (chip8--key-press #xA)))
    (define-key map (kbd "b") #'(lambda () (interactive) (chip8--key-press #xB)))
    (define-key map (kbd "c") #'(lambda () (interactive) (chip8--key-press #xC)))
    (define-key map (kbd "d") #'(lambda () (interactive) (chip8--key-press #xD)))
    (define-key map (kbd "e") #'(lambda () (interactive) (chip8--key-press #xE)))
    (define-key map (kbd "f") #'(lambda () (interactive) (chip8--key-press #xF)))
    map)
  "Keymap for `chip8-mode'.")

;;; TODO BC_test.ch8 (shift t, leave-i-unchanged t)

(cl-defstruct chip8-quirks
  "Quirks configuration for a CHIP-8 emulator.

Quirks are little differences in opcodes interpretation by
different CHIP-8 interpreters developed through the years.

The following list of quirks is taken from
https://github.com/chip-8/chip-8-database/blob/master/database/quirks.json"
  (shift nil :documentation "On most systems the shift opcodes take `vY` as input and stores
the shifted version of `vY` into `vX`. The interpreters for the
HP48 took `vX` as both the input and the output, introducing the
shift quirk. If t opcodes `8XY6` and `8XYE` take `vX` as both
input and output. If nil opcodes `8XY6` and `8XYE` take `vY` as
input and `vX` as output.")
  (increment-i-by-x nil :documentation "On most systems storing and retrieving data between registers and
memory increments the `i` register with `X + 1` (the number of
registers read or written). So for each register read or writen,
the index register would be incremented. The CHIP-48 interpreter
for the HP48 would only increment the `i` register by `X`,
introducing the first load/store quirk. If t opcodes `FX55` and
`FX65` increment the `i` register with `X`. If nil opcodes `FX55`
and `FX65` increment the `i` register with `X + 1`.")
  (leave-i-unchanged t :documentation "On most systems storing and retrieving data between registers and
memory increments the `i` register relative to the number of
registers read or written. The Superchip 1.1 interpreter for the
HP48 however did not increment the `i` register at all,
introducing the second load/store quirk. If t opcodes `FX55` and
`FX65` leave the `i` register unchanged. If nil opcodes `FX55`
and `FX65` increment the `i` register.")
  (wrap nil :documentation "Most systems, when drawing sprites to the screen, will clip
sprites at the edges of the screen. The Octo interpreter, which
spawned the XO-CHIP variant of CHIP-8, instead wraps the sprite
around to the other side of the screen. This introduced the wrap
quirk. If t the `DXYN` opcode wraps around to the other side of
the screen when drawing at the edges. If nil the `DXYN` opcode
clips when drawing at the edges of the screen.")
  (jump nil :documentation "The jump to `<address> + v0` opcode was wronly implemented on all
the HP48 interpreters as jump to `<address> + vX`, introducing
the jump quirk. If t opcode `BXNN` jumps to address `XNN + vX`.
If nil opcode `BNNN` jumps to address `NNN + v0`.")
  (vblank nil :documentation "The original Cosmac VIP interpreter would wait for vertical blank
before each sprite draw. This was done to prevent sprite tearing
on the display, but it would also act as an accidental limit on
the execution speed of the program. Some programs rely on this
speed limit to be playable. Vertical blank happens at 60Hz, and
as such its logic be combined with the timers. If t opcode `DXYN`
waits for vertical blank (so max 60 sprites drawn per second). If
nil opcode `DXYN` draws immediately (number of sprites drawn per
second only limited to number of CPU cycles per frame).")
  (logic t :documentation "On the original Cosmac VIP interpreter, `vF` would be reset after
each opcode that would invoke the maths coprocessor. Later
interpreters have not copied this behaviour. If t opcodes `8XY1`,
`8XY2` and `8XY3` (OR, AND and XOR) will set `vF` to zero after
execution (even if `vF` is the parameter `X`). If nil opcodes
`8XY1`, `8XY2` and `8XY3` (OR, AND and XOR) will leave `vF`
unchanged (unless `vF` is the parameter `X`)."))

(defconst chip8--original-quirks
  (make-chip8-quirks
   :shift nil
   :increment-i-by-x nil
   :leave-i-unchanged nil
   :wrap nil
   :jump nil
   :vblank t
   :logic t)
  "Quirks of original Cosmac VIP CHIP-8 implementation.
See https://github.com/chip-8/chip-8-database/blob/master/database/platforms.json")

(defconst chip8--modern-quirks
  (make-chip8-quirks
   :shift nil
   :increment-i-by-x nil
   :leave-i-unchanged nil
   :wrap nil
   :jump nil
   :vblank nil
   :logic nil)
  "Quirks of Modern CHIP-8 implementation.
See https://github.com/chip-8/chip-8-database/blob/master/database/platforms.json")

(defconst chip8--superchip-quirks
  (make-chip8-quirks
   :shift t
   :increment-i-by-x nil
   :leave-i-unchanged t
   :wrap nil
   :jump t
   :vblank nil
   :logic nil)
  "Quirks of Superchip CHIP-8 implementation.
See https://github.com/chip-8/chip-8-database/blob/master/database/platforms.json")

(defconst chip8--xo-chip-quirks
  (make-chip8-quirks
   :shift nil
   :increment-i-by-x nil
   :leave-i-unchanged nil
   :wrap t
   :jump nil
   :vblank nil
   :logic nil)
  "Quirks of XO-CHIP CHIP-8 implementation.
See https://github.com/chip-8/chip-8-database/blob/master/database/platforms.json")

(cl-defstruct chip8
  "CHIP-8 emulator state."
  (ram (make-vector chip8/RAM-SIZE 0) :documentation "4K of ram")
  (v (make-vector 16 0) :documentation "General purpose registers, 8 bits")
  (i 0 :documentation "Index register, 16 bits")
  (pc 0 :documentation "Program counter, 16 bits")
  (stack '() :documentation "Stack")
  (delay-timer 0 :documentation "Delay timer")
  (sound-timer 0 :documentation "Sound timer")
  (keys (make-vector 16 nil) :documentation "Keypad 16 current keys status representation")
  (current-canvas nil :documentation "Current retro.el canvas, display representation")
  (previous-canvas nil :documentation "Previous retro.el canvas, needed by retro.el")
  (waiting-for-key-release nil :documentation "Keycode of the key we are waiting to be relased. See Fx0A")
  (last-frame-at (current-time) :documentation "Timestamp when the last frame got rendered")
  (quirks (make-chip8-quirks) :documentation "Quirks configuration to use in the emulator"))

(define-derived-mode chip8-mode nil "CHIP-8 Emulator"
  (use-local-map chip8-mode-map)
  (setq chip8--current-instance (chip8--setup
                                 chip8--current-rom-filename
                                 chip8--current-quirks
                                 t))
  (run-at-time 0.001 nil 'chip8--run))

(defun chip8 (filename)
  "Run chip8 emulation loading FILENAME rom."
  (interactive "ffilename: ")
  (select-window (or (get-buffer-window chip8/BUFFER-NAME)
                     (selected-window)))
  (switch-to-buffer chip8/BUFFER-NAME)
  (setq chip8--current-rom-filename filename
        chip8--current-quirks (or chip8--current-quirks chip8--original-quirks))
  (chip8-mode))

(defun chip8-original (filename)
  "Run chip8 emulation loading FILENAME rom with original Cosmac VIP CHIP-8 quirks."
  (interactive "ffilename: ")
  (setq chip8--current-quirks chip8--original-quirks)
  (chip8 filename))

(defun chip8-modern (filename)
  "Run chip8 emulation loading FILENAME rom with modern CHIP-8 quirks."
  (interactive "ffilename: ")
  (setq chip8--current-quirks chip8--modern-quirks)
  (chip8 filename))

(defun chip8-superchip (filename)
  "Run chip8 emulation loading FILENAME rom with superchip CHIP-8 quirks."
  (interactive "ffilename: ")
  (setq chip8--current-quirks chip8--superchip-quirks)
  (chip8 filename))

(defun chip8-xo-chip (filename)
  "Run chip8 emulation loading FILENAME rom with superchip CHIP-8 quirks."
  (interactive "ffilename: ")
  (setq chip8--current-quirks chip8--xo-chip-quirks)
  (chip8 filename))

(defun chip8--key-press (keycode)
  "Will emulate the key press of KEYCODE in current EMULATOR."
  (aset (chip8-keys chip8--current-instance) keycode (current-time))
  (run-at-time chip8/KEY-RELEASE-TIMEOUT nil #'chip8--key-release keycode))

(defun chip8--key-release (keycode)
  "Will emulate the key release of KEYCODE in current EMULATOR."
  (let ((key-pressed-at (aref (chip8-keys chip8--current-instance) keycode)))
    (when (>= (float-time (time-subtract (current-time) key-pressed-at)) chip8/KEY-RELEASE-TIMEOUT)
      (aset (chip8-keys chip8--current-instance) keycode nil))))

(defun chip8--key-pressed-p (keycode emulator)
  "Return t if key with KEYCODE is pressed in EMULATOR."
  (if (aref (chip8-keys emulator) keycode)
      t
    nil))

(defun chip8--key-pressed (emulator)
  "Return keycode of first pressed key in EMULATOR, nil otherwise."
  (cl-loop for element across (chip8-keys emulator)
           for index from 0
           until (not (eq element nil))
           finally (return (if element index nil))))

(defmacro chip8--vx (emulator nimbles)
  "Given instruction NIMBLES get the Vx register of EMULATOR.

In most of the instructions the reference of the register name as
X is the second nimble (ex 3xKK). We take the reference of the
name of the register from the second nimble and then we return
the PLACE of this register in EMULATOR."
  `(aref (chip8-v ,emulator) (ash (logand ,nimbles #x0F00) -8)))

(defmacro chip8--vy (emulator nimbles)
  "Given instruction NIMBLES get the Vy register of EMULATOR.

In most of the instructions the reference of the register name as
Y is the third nimble (ex 5xy0). We take the reference of the
name of the register from the third nimble and then we return
the PLACE of this register in EMULATOR."
  `(aref (chip8-v ,emulator) (ash (logand ,nimbles #x00F0) -4)))

(defmacro chip8--vf (emulator)
  "Return the Vf register of EMULATOR."
  `(aref (chip8-v ,emulator) #xF))

(defun chip8-quit ()
  "Quit current game if any."
  (interactive)
  (setq chip8--current-instance nil
        chip8--current-quirks nil)
  (kill-buffer chip8/BUFFER-NAME))

(defun chip8--setup (filename quirks switch-to-buffer-p)
  "Setup game with rom FILENAME and QUIRKS.

Switch to CHIP-8 buffer when SWITCH-TO-BUFFER-P is \\='t'."
  (retro--init-color-palette chip8/COLORS 0)
  (let ((ram (make-vector chip8/RAM-SIZE 0))
        (canvas (chip8--setup-buffer
                 chip8/BUFFER-NAME
                 chip8/SCREEN-WIDTH
                 chip8/SCREEN-HEIGHT
                 0
                 switch-to-buffer-p)))
    (chip8--load-default-font ram)
    (chip8--load-rom filename ram)
    (make-chip8
     :pc chip8/ROM-ADDRESS
     :ram ram
     :quirks quirks
     :current-canvas canvas
     :previous-canvas (retro-canvas-copy canvas))))

(defun chip8--load-default-font (ram)
  "Load default font in CHIP-8 RAM."
  (dotimes (i (length chip8/FONT))
    (aset ram (+ i chip8/FONT-ADDRESS) (aref chip8/FONT i))))

(defun chip8--load-rom (filename ram)
  "Load rom FILENAME in CHIP-8 RAM."
  (when (or (not (file-exists-p filename)) (not (file-readable-p filename)) (file-directory-p filename))
    (user-error "ROM file %s does not exists or is not readable" filename))
  (let ((rom (seq-into
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (setq buffer-file-coding-system 'binary)
                (insert-file-contents-literally filename nil 0)
                (buffer-substring-no-properties (point-min) (point-max)))
              'vector)))
    (dotimes (i (length rom))
      (aset ram (+ i chip8/ROM-ADDRESS) (aref rom i)))))

(defun chip8--run ()
  "Make the current game run."
  (let ((buffer (get-buffer chip8/BUFFER-NAME))
         (current-canvas nil)
         (previous-canvas nil)
         (last-frame-at 0)
         (instructions-counter 0)
         (elapsed 0))
    ;; it runs only when we are in the emulator buffer
    (when (eq (current-buffer) buffer)
      (when (not chip8--current-instance)
        (error "Missing emulator instance, this should not happen"))
      (setq last-frame-at (chip8-last-frame-at chip8--current-instance)
            current-canvas (chip8-current-canvas chip8--current-instance)
            previous-canvas (chip8-previous-canvas chip8--current-instance))
      (while (< elapsed chip8/FRAME-DURATION)
        (if (>= instructions-counter chip8/INSTRUCTIONS-PER-FRAME)
            (sleep-for 0.01)
          (chip8--step chip8--current-instance)
          (setq instructions-counter (1+ instructions-counter))
          (sleep-for 0.001))
        (setq elapsed (float-time (time-subtract (current-time) last-frame-at))))
      ;; (message "FPS: %f, elapsed: %fs" (/ 1.0 elapsed) elapsed)
      (retro--buffer-render current-canvas previous-canvas)
      (chip8--canvas-copy current-canvas previous-canvas)
      (setf (chip8-last-frame-at chip8--current-instance) (current-time)
            (chip8-delay-timer chip8--current-instance) (max 0 (1- (chip8-delay-timer chip8--current-instance)))
            (chip8-sound-timer chip8--current-instance) (max 0 (1- (chip8-sound-timer chip8--current-instance))))
      (run-at-time 0.001 nil 'chip8--run))))

(defun chip8--step (emulator)
  "Run a single step of fetch/decode of the EMULATOR."
  (let* ((nimbles (chip8--fetch16 emulator))
         (opcode (logand nimbles #xF000)))
    ;; (message "[0x%04X: 0x%04X] i: 0x%04X \n\tregisters: %S\n\tstack: %S"
    ;;          (chip8-pc emulator)
    ;;          nimbles
    ;;          (chip8-i emulator)
    ;;          (seq-map-indexed (lambda (x i) (format "0x%X: 0x%02X" i x)) (chip8-v emulator))
    ;;          (seq-map (lambda (x) (format "0x%04X" x)) (chip8-stack emulator)))
    (cond
     ((eq nimbles #x00E0)
      ;; 00E0 - CLS
      ;; Clear the display.
      (retro--reset-canvas (chip8-current-canvas emulator))
      (cl-incf (chip8-pc emulator) 2))
     ((eq nimbles #x00EE)
      ;; 00EE - RET
      ;; Return from a subroutine.
      ;; The interpreter sets the program counter to the address at the top of
      ;; the stack, then subtracts 1 from the stack pointer.
      (setf (chip8-pc emulator) (pop (chip8-stack emulator))))
     ((eq opcode #x0000)
      ;; 0nnn - SYS addr
      ;; Jump to a machine code routine at nnn.
      ;; This instruction is only used on the old computers on which Chip-8 was originally
      ;; implemented. It is ignored by modern interpreters.
      (cl-incf (chip8-pc emulator) 2))
     ((eq opcode #xA000)
      ;; Annn - LD I, addr
      ;; Set I = nnn.
      (setf (chip8-i emulator) (logand nimbles #x0FFF))
      (cl-incf (chip8-pc emulator) 2))
     ((eq opcode #xB000)
      ;; Bnnn - JP V0, addr
      ;; Jump to location nnn + V0.
      ;; Quirk https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#bnnn-jump-with-offset
      (if (chip8-quirks-jump (chip8-quirks emulator))
          (setf (chip8-pc emulator) (+ (logand nimbles #x0FFF) (chip8--vx emulator nimbles)))
        (setf (chip8-pc emulator) (+ (logand nimbles #x0FFF) (aref (chip8-v emulator) #x0)))))
     ((eq opcode #x6000)
      ;; 6xkk - LD Vx, byte
      ;; Set Vx = kk.
      (setf (chip8--vx emulator nimbles) (logand nimbles #x00FF))
      (cl-incf (chip8-pc emulator) 2))
     ((eq opcode #xD000)
      ;; Dxyn - DRW Vx, Vy, nibble
      ;; Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
      (let* ((x (mod (chip8--vx emulator nimbles) chip8/SCREEN-WIDTH))
             (y (mod (chip8--vy emulator nimbles) chip8/SCREEN-HEIGHT))
             (n (logand nimbles #x000F))
             (sprite (chip8--read-bytes emulator n (chip8-i emulator)))
             (hit? (chip8--draw-sprite emulator x y n sprite)))
        (setf (aref (chip8-v emulator) #xF) (if hit? #x1 #x0))
        (cl-incf (chip8-pc emulator) 2)))
     ((eq opcode #x7000)
      ;; 7xkk - ADD Vx, byte
      ;; Set Vx = Vx + kk.
      (setf (chip8--vx emulator nimbles) (logand #xFF (+ (chip8--vx emulator nimbles)
                                                         (logand nimbles #x00FF))))
      (cl-incf (chip8-pc emulator) 2))
     ((eq opcode #x1000)
      ;; 1nnn - JP addr
      ;; Jump to location nnn.
      (setf (chip8-pc emulator) (logand nimbles #x0FFF)))
     ((eq opcode #x3000)
      ;; 3xkk - SE Vx, byte
      ;; Skip next instruction if Vx = kk.
      (if (eq (chip8--vx emulator nimbles) (logand nimbles #x00FF))
          (cl-incf (chip8-pc emulator) 4)
        (cl-incf (chip8-pc emulator) 2)))
     ((eq opcode #x4000)
      ;; 4xkk - SNE Vx, byte
      ;; Skip next instruction if Vx != kk.
      (if (eq (chip8--vx emulator nimbles)
              (logand nimbles #x00FF))
          (cl-incf (chip8-pc emulator) 2)
        (cl-incf (chip8-pc emulator) 4)))
     ((eq opcode #x5000)
      ;; 5xy0 - SE Vx, Vy
      ;; Skip next instruction if Vx = Vy.
      (if (eq (chip8--vx emulator nimbles) (chip8--vy emulator nimbles))
          (cl-incf (chip8-pc emulator) 4)
        (cl-incf (chip8-pc emulator) 2)))
     ((eq opcode #x9000)
      ;; 9xy0 - SNE Vx, Vy
      ;; Skip next instruction if Vx != Vy.
      (if (eq (chip8--vx emulator nimbles) (chip8--vy emulator nimbles))
          (cl-incf (chip8-pc emulator) 2)
        (cl-incf (chip8-pc emulator) 4)))
     ((eq opcode #x2000)
      ;; 2nnn - CALL addr
      ;; Call subroutine at nnn.
      ;; The interpreter increments the stack pointer, then puts the current PC
      ;; on the top of the stack. The PC is then set to nnn.
      (push (+ (chip8-pc emulator) 2) (chip8-stack emulator))
      (setf (chip8-pc emulator) (logand nimbles #x0FFF)))
     ((eq opcode #xC000)
      ;; Cxkk - RND Vx, byte
      ;; Set Vx = random byte AND kk.
      (setf (chip8--vx emulator nimbles) (logand (random 256) (logand nimbles #x00FF)))
      (cl-incf (chip8-pc emulator) 2))
     ((eq opcode #xF000)
      (let ((last-byte (logand nimbles #x00FF)))
        (cond
         ((eq last-byte #x0A)
          ;; Fx0A - LD Vx, K
          ;; Wait for a key press, store the value of the key in Vx.
          (let ((key-pressed (chip8-waiting-for-key-release emulator)))
            (if key-pressed
                (when (not (chip8--key-pressed-p key-pressed emulator))
                  (setf (chip8--vx emulator nimbles) key-pressed
                        (chip8-waiting-for-key-release emulator) nil)
                  (cl-incf (chip8-pc emulator) 2))
              (setq key-pressed (chip8--key-pressed emulator))
              (when key-pressed
                (setf (chip8-waiting-for-key-release emulator) key-pressed)))))
         ((eq last-byte #x07)
          ;; Fx07 - LD Vx, DT
          ;; Set Vx = delay timer value.
          (setf (chip8--vx emulator nimbles) (chip8-delay-timer emulator))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x15)
          ;; Fx15 - LD DT, Vx
          ;; Set delay timer = Vx.
          (setf (chip8-delay-timer emulator) (chip8--vx emulator nimbles))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x18)
          ;; Fx18 - LD ST, Vx
          ;; Set sound timer = Vx.
          (setf (chip8-sound-timer emulator) (chip8--vx emulator nimbles))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x29)
          ;; Fx29 - LD F, Vx
          ;; Set I = location of sprite for digit Vx.
          (setf (chip8-i emulator) (+ chip8/FONT-ADDRESS (* (chip8--vx emulator nimbles) 5)))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x33)
          ;; Fx33 - LD B, Vx
          ;; Store BCD representation of Vx in memory locations I, I+1, and I+2.
          ;; The interpreter takes the decimal value of Vx, and places the hundreds
          ;; digit in memory at location in I, the tens digit at location I+1, and
          ;; the ones digit at location I+2.
          (let ((digits (chip8--to-bdc (chip8--vx emulator nimbles)))
                (ri (chip8-i emulator)))
            (dotimes (i (length digits))
              (chip8--write-bytes emulator (nth i digits) 1 (+ ri i))))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x1E)
          ;; Fx1E - ADD I, Vx
          ;; Set I = I + Vx.
          (let ((i (+ (chip8-i emulator)
                      (chip8--vx emulator nimbles))))
            (setf (chip8-i emulator) (logand i #x0FFF)
                  (aref (chip8-v emulator) #xF) (if (eq i (logand i #x0FFF)) #x0 #x1)))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x65)
          ;; Fx65 - LD Vx, [I]
          ;; Read registers V0 through Vx from memory starting at location I.
          ;; NOTE: x in Fx65 is a value that must be considered literally,
          ;; F165 means to read registers V0 from I and V1 from (I + 1)
          ;; F265 means to read registers V0 from I, V1 from (I + 1) and V2 from (I + 2)
          (let ((vx (ash (logand nimbles #x0F00) -8))
                (ri (chip8-i emulator)))
            (cl-loop for i from 0 to vx
                     do (setf (aref (chip8-v emulator) i) (chip8--read-bytes emulator 1 (+ ri i))))
            ;; Quirk https://chip8.gulrak.net/#quirk11
            (unless (chip8-quirks-leave-i-unchanged (chip8-quirks emulator))
              (if (chip8-quirks-increment-i-by-x (chip8-quirks emulator))
                  (setf (chip8-i emulator) (+ (chip8-i emulator) vx))
                (setf (chip8-i emulator) (+ (chip8-i emulator) vx 1)))))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x55)
          ;; Fx55 - LD [I], Vx
          ;; Store registers V0 through Vx in memory starting at location I.
          ;; F165 means to store registers V0 to I and V1 to (I + 1)
          ;; F265 means to store registers V0 to I, V1 to (I + 1) and V2 to (I + 2)
          (let ((vx (ash (logand nimbles #x0F00) -8))
                (ri (chip8-i emulator)))
            (cl-loop for i from 0 to vx
                     do (chip8--write-bytes emulator (aref (chip8-v emulator) i) 1 (+ ri i)))
            ;; Quirk https://chip8.gulrak.net/#quirk11
            (unless (chip8-quirks-leave-i-unchanged (chip8-quirks emulator))
              (if (chip8-quirks-increment-i-by-x (chip8-quirks emulator))
                  (setf (chip8-i emulator) (+ (chip8-i emulator) vx))
                (setf (chip8-i emulator) (+ (chip8-i emulator) vx 1)))))
          (cl-incf (chip8-pc emulator) 2))
         (t (error "TODO: opcode 0x%04X not yet implemented at 0x%04X" nimbles (chip8-pc emulator))))))
     ((eq opcode #x8000)
      (let ((last-nimble (logand nimbles #x000F)))
        (cond
         ((eq last-nimble #x0)
          ;; 8xy0 - LD Vx, Vy
          ;; Set Vx = Vy.
          (setf (chip8--vx emulator nimbles) (chip8--vy emulator nimbles))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-nimble #x1)
          ;; 8xy1 - OR Vx, Vy
          ;; Set Vx = Vx OR Vy.
          (setf (chip8--vx emulator nimbles) (logior (chip8--vx emulator nimbles)
                                                     (chip8--vy emulator nimbles)))
          ;; Quirk https://chip8.gulrak.net/#quirk4
          (when (chip8-quirks-logic (chip8-quirks emulator))
            (setf (chip8--vf emulator) #x0))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-nimble #x2)
          ;; 8xy2 - AND Vx, Vy
          ;; Set Vx = Vx AND Vy.
          (setf (chip8--vx emulator nimbles) (logand (chip8--vx emulator nimbles)
                                                     (chip8--vy emulator nimbles)))
          ;; Quirk https://chip8.gulrak.net/#quirk4
          (when (chip8-quirks-logic (chip8-quirks emulator))
            (setf (chip8--vf emulator) #x0))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-nimble #x3)
          ;; 8xy3 - XOR Vx, Vy
          ;; Set Vx = Vx XOR Vy.
          (setf (chip8--vx emulator nimbles) (logxor (chip8--vx emulator nimbles)
                                                     (chip8--vy emulator nimbles)))
          ;; Quirk https://chip8.gulrak.net/#quirk4
          (when (chip8-quirks-logic (chip8-quirks emulator))
            (setf (chip8--vf emulator) #x0))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-nimble #x4)
          ;; 8xy4 - ADD Vx, Vy
          ;; Set Vx = Vx + Vy, set VF = carry.
          (let ((res (+ (chip8--vx emulator nimbles)
                        (chip8--vy emulator nimbles))))
            (setf
             (chip8--vx emulator nimbles) (logand res #xFF)
             (aref (chip8-v emulator) #xF) (if (not (eq res (logand res #xFF)))
                                               #x1 #x0)))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-nimble #x5)
          ;; 8xy5 - SUB Vx, Vy
          ;; Set Vx = Vx - Vy, set VF = NOT borrow.
          (let ((vx (chip8--vx emulator nimbles))
                (vy (chip8--vy emulator nimbles)))
            (setf (chip8--vx emulator nimbles) (chip8--complement-byte (- vx vy))
                  (aref (chip8-v emulator) #xF) (if (> vx vy) #x1 #x0)))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-nimble #x7)
          ;; 8xy7 - SUBN Vx, Vy
          ;; Set Vx = Vy - Vx, set VF = NOT borrow.
          (let ((vx (chip8--vx emulator nimbles))
                (vy (chip8--vy emulator nimbles)))
            (setf (chip8--vx emulator nimbles) (chip8--complement-byte (- vy vx))
                  (aref (chip8-v emulator) #xF) (if (> vy vx) #x1 #x0)))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-nimble #x6)
          ;; 8xy6 - SHR Vx {, Vy}
          ;; Set Vx = Vx SHR 1.
          ;; If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0.
          ;; Then Vx is divided by 2.
          (let ((vx (chip8--vx emulator nimbles))
                (vy (chip8--vy emulator nimbles)))
            ;; Quirk https://chip8.gulrak.net/#quirk5
            (if (chip8-quirks-shift (chip8-quirks emulator))
                (setf (chip8--vx emulator nimbles) (logand #xFF (ash vx -1))
                      (chip8--vf emulator) (if (> (logand vx #x01) 0) #x1 #x0))
              (setf (chip8--vx emulator nimbles) (logand #xFF (ash vy -1))
                    (chip8--vf emulator) (if (> (logand vy #x01) 0) #x1 #x0))))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-nimble #xE)
          ;; 8xyE - SHL Vx {, Vy}
          ;; Set Vx = Vx SHL 1.
          ;; If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0.
          ;; Then Vx is multiplied by 2.
          (let ((vx (chip8--vx emulator nimbles))
                (vy (chip8--vy emulator nimbles)))
            ;; Quirk https://chip8.gulrak.net/#quirk5
            (if (chip8-quirks-shift (chip8-quirks emulator))
                (setf (chip8--vx emulator nimbles) (logand #xFF (ash vx 1))
                      (chip8--vf emulator) (if (> (logand vx #x80) 0) #x1 #x0))
              (setf (chip8--vx emulator nimbles) (logand #xFF (ash vy 1))
                    (chip8--vf emulator) (if (> (logand vy #x80) 0) #x1 #x0))))
          (cl-incf (chip8-pc emulator) 2))
         (t (error "TODO: opcode 0x%04X not yet implemented at 0x%04X" nimbles (chip8-pc emulator))))))
     ((eq opcode #xE000)
      (let ((last-byte (logand nimbles #x00FF)))
        (cond
         ((eq last-byte #x9E)
          ;; Ex9E - SKP Vx
          ;; Skip next instruction if key with the value of Vx is pressed.
          (if (chip8--key-pressed-p (chip8--vx emulator nimbles) emulator)
              (cl-incf (chip8-pc emulator) 4)
            (cl-incf (chip8-pc emulator) 2)))
         ((eq last-byte #xA1)
          ;; ExA1 - SKNP Vx
          ;; Skip next instruction if key with the value of Vx is not pressed.
          (if (not (chip8--key-pressed-p (chip8--vx emulator nimbles) emulator))
              (cl-incf (chip8-pc emulator) 4)
            (cl-incf (chip8-pc emulator) 2)))
         (t (error "TODO: opcode 0x%04X not yet implemented at 0x%04X" nimbles (chip8-pc emulator))))))
     (t (error "TODO: opcode 0x%04X not yet implemented at 0x%04X" nimbles (chip8-pc emulator))))))

(defun chip8--draw-sprite (emulator x y n sprite)
  "Draw SPRITE made of N bytes on EMULATOR's canvas at coordinates (X, Y).

A sprite is 8 pixel wide and N pixel tall (where N is the number
of bytes), every byte is a line of the sprite, every bit of the
byte is a pixel, if the bit is 1 then we need to turn \"on\" the
corresponding pixel, otherwise we need to turn it \"off\".

Returns a boolean value indicating if there was a collision (aka
if any pixel on the CANVAS was turned off)."
  (let ((collision? nil)
        (canvas-pixels (retro-canvas-pixels (chip8-current-canvas emulator)))
        (canvas-width (retro-canvas-width (chip8-current-canvas emulator)))
        (sprite-index (* n 8))
        canvas-pixel
        sprite-pixel
        xi yi)
    (dotimes (yd n)
      (dotimes (xd 8)
        (setq xi (+ x xd)
              yi (+ y yd)
              sprite-index (1- sprite-index))
        (when (chip8-quirks-wrap (chip8-quirks emulator))
          (setq xi (mod xi chip8/SCREEN-WIDTH)
                yi (mod yi chip8/SCREEN-HEIGHT)))
        (when (and (< xi chip8/SCREEN-WIDTH) (< yi chip8/SCREEN-HEIGHT))
          (setq canvas-pixel (chip8--get-pixel xi yi canvas-pixels canvas-width)
                sprite-pixel (ash (logand sprite (ash #x1 sprite-index)) (- sprite-index)))
          (when (and (> canvas-pixel #x0) (> sprite-pixel #x0))
            (setq collision? t))
          (retro--plot-pixel
           xi
           yi
           (logxor canvas-pixel sprite-pixel)
           canvas-pixels
           canvas-width))))
    collision?))

(defun chip8--fetch16 (emulator)
  "Fetch 16 bits from EMULATOR's RAM at PC."
  (let ((ram (chip8-ram emulator)))
    (logior (ash (aref ram (chip8-pc emulator)) 8)
            (aref ram (1+ (chip8-pc emulator))))))

(defun chip8--read-bytes (emulator n address)
  "Read N bytes from EMULATOR's RAM at ADDRESS."
  (let ((bytes 0))
    (dotimes (i n)
      (setq bytes (logior (ash bytes 8)
                          (aref (chip8-ram emulator) (+ i address)))))
    bytes))

(defun chip8--write-bytes (emulator bytes n address)
  "Write N BYTES to EMULATOR's RAM at ADDRESS."
  ;; (message "chip8--write-bytes 0x%04X %d 0x%04X" bytes n address)
  (let ((mask (ash #xFF (* (- n 1) 8))))
    (dotimes (i n)
      (setf (aref (chip8-ram emulator) (+ i address))
            (ash (logand bytes mask) (- (* (- n i 1) 8))))
      (setq mask (ash mask (- 8))))))

(defun chip8--to-bdc (x)
  "Encode X as Binary Encoded Decimal.

Split decimal value of X in a list of its digits."
  (when (< x 0)
    (error "Cannot encode %d as BDC" x))
  (let ((res '()))
    (setq res (cons (mod x 10) res)
          x (floor (/ x 10)))
    (while (> x 0)
      (setq res (cons (mod x 10) res)
            x (floor (/ x 10))))
    res))

(defun chip8--complement-byte (x)
  "Represent X in binary complement.

It returns X if X is a non negative number. It returns binary
complement of X if X is a negative number."
  (logand #xFF
          (if (< x 0)
              (+ #x100 x)
            x)))

;;; TODO: move to retro
;;; TODO: retro--get-pixel
;;; TODO: retro--get-pixel-canvas
(defun chip8--get-pixel (x y pixels width)
  "Get pixel color at (X, Y) in PIXELS with a certain WIDTH."
  (aref pixels (+ (* y width) x)))

;;; TODO: move to retro
;;; TODO: retro--canvas-pixels-copy
(defun chip8--canvas-copy (from to)
  "Copy pixels in canvas FROM to pixels in canvas TO."
  (setf (retro-canvas-pixels to) (copy-sequence (retro-canvas-pixels from))))

;;; TODO: move to retro
;;; TODO: retro-init-buffer or something
(defun chip8--setup-buffer (buffer-name screen-width screen-height background-color switch-to-buffer-p)
  "Setup buffer BUFFER-NAME as retro.el requires.

Will return a retro.el canvas ready to be used as screen with
retro.el primitives.

Canvas will be initialized with a screen width SCREEN-WIDTH,
SCREEN-HEIGHT and a background color with index BACKGROUND-COLOR.

When setup is completed will switch to BUFFER-NAME
if SWITCH-TO-BUFFER-P is \\='t'."
  (select-window (or (get-buffer-window buffer-name)
                     (selected-window)))
  (with-current-buffer (get-buffer-create buffer-name)
    (let* ((window (selected-window))
           ;; NOTE: without switching to buffer, buffer calibration is not
           ;; reliable, I didn't find a way to make it work but since we don't
           ;; need precision because we are not looking at the buffer, a dummy
           ;; but credible calibration will do
           (calibration (if switch-to-buffer-p
                            (retro--calibrate-canvas-in-window
                             screen-width
                             screen-height
                             window)
                          (list 10 screen-width screen-height)))
           (pixel-size (nth 0 calibration))
           (window-width (nth 1 calibration))
           (window-height (nth 2 calibration)))
      (when (not calibration) (error "Failed to calibrate pixel size in buffer %s" buffer-name))
      (set-face-attribute 'retro-default-face nil :height pixel-size)
      (buffer-face-set 'retro-default-face)
      (let* ((margin-top (/ (- window-height screen-height) 2))
             (margin-left (/ (- window-width screen-width) 2))
             (canvas (retro-canvas-create :margin-left margin-left
                                          :margin-top margin-top
                                          :width screen-width
                                          :height screen-height
                                          :background-color background-color))
             (margin-top-string (propertize (make-string (+ margin-left screen-width) 32) 'face 'default))
             (margin-left-string (propertize (make-string margin-left 32) 'face 'default))
             (canvas-string (propertize (make-string screen-width 32) 'face (aref retro-palette-faces background-color))))
        (dotimes (_ margin-top)
          (insert margin-top-string)
          (insert "\n"))
        (dotimes (_ screen-height)
          (insert margin-left-string)
          (insert canvas-string)
          (insert "\n"))
        (setq-local buffer-read-only t
                    visible-cursor nil
                    cursor-type nil
                    inhibit-modification-hooks t
                    inhibit-compacting-font-caches t
                    bidi-inhibit-bpa t
                    bidi-display-reordering nil
                    bidi-paragraph-direction 'left-to-right
                    buffer-read-only nil
                    mode-line-format nil)
        (when switch-to-buffer-p
          (switch-to-buffer buffer-name))
        canvas))))

(provide 'chip8)

;; Local Variables:
;; coding: utf-8
;; End:
;;; chip8.el ends here
