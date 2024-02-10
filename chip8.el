;;; chip8.el --- A CHIP-8 emulator -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: http://github.com/gabrielelana/chip8.el
;; Keywords: CHIP-8, game, games, emulator

;; This file is not part of GNU Emacs

;; SPDX-License-Identifier: MIT

;; Use of this source code is governed by an MIT-style
;; license that can be found in the LICENSE file or at
;; https://opensource.org/licenses/MIT.

;;; Commentary:

;; A CHIP-8 emulator

;;; Code:

(require 'cl-lib)

;;; The following stuff is vendoring of https://github.com/gabrielelana/retro.el

(defconst chip8--retro-palette-size 65536)

(defvar chip8--retro-palette-faces (make-vector chip8--retro-palette-size 0))

(defconst chip8--retro-default-face-height 20)

(defvar chip8--retro-square-font-family "Kreative Square SM"
  "Font family used to create the illusion of pixels.")

(defface chip8--retro-default-face `((t :family ,chip8--retro-square-font-family :height ,chip8--retro-default-face-height))
  "Face used as default face for retro buffers."
  :group 'chip8)

(cl-defstruct (chip8--retro-canvas (:constructor chip8--retro-canvas--create)
                                   (:copier nil))
  "Canvas data structure."
  (margin-left 0 :type number)
  (margin-top 0 :type number)
  (width 0 :type number)
  (height 0 :type number)
  (background-color 0 :type number)
  (pixels nil :type hashmap)
  (buffer-before-length 0 :type number)
  (buffer-line-length 0 :type number))

(cl-defun chip8--retro-canvas-create (&key margin-left margin-top width height background-color)
  "Create CANVAS.

with the following attributes: MARGIN-LEFT, MARGIN-TOP, WIDTH,
HEIGHT, BACKGROUND-COLOR."
  (chip8--retro-canvas--create :margin-left margin-left
                               :margin-top margin-top
                               :width width
                               :height height
                               :background-color background-color
                               :pixels (make-vector (* width height) background-color)
                               :buffer-before-length (* (+ margin-left width 1) margin-top)
                               :buffer-line-length (+ margin-left width 1)))

(defun chip8--retro-canvas-copy (canvas)
  "Copy CANVAS, no memory is shared."
  (chip8--retro-canvas-create :margin-left (chip8--retro-canvas-margin-left canvas)
                              :margin-top (chip8--retro-canvas-margin-top canvas)
                              :width (chip8--retro-canvas-width canvas)
                              :height (chip8--retro-canvas-height canvas)
                              :background-color (chip8--retro-canvas-background-color canvas)))

(defun chip8--retro-canvas-pixels-copy (from to)
  "Copy pixels in canvas FROM to pixels in canvas TO."
  (setf (chip8--retro-canvas-pixels to) (copy-sequence (chip8--retro-canvas-pixels from))))

(defun chip8--retro-reset-canvas (canvas)
  "Clean all CANVAS pixels."
  (fillarray (chip8--retro-canvas-pixels canvas) (chip8--retro-canvas-background-color canvas)))

(defun chip8--retro-check-requirements ()
  "Will check environment requirements to make retro run."
  (when (not (find-font (font-spec :name chip8--retro-square-font-family)))
    (error
     "Unable to find needed font in current environment. Please install `%s` font so that Emacs can use it" chip8--retro-square-font-family))
  (when (not (native-comp-available-p))
    (warn
     "Native compilation is not required but it's strongly recommended")))

(defun chip8--retro-init-buffer (buffer-name screen-width screen-height background-color switch-to-buffer-p)
  "Setup buffer BUFFER-NAME as retro.el requires.

Will return a retro.el canvas ready to be used as screen with
retro.el primitives.

Canvas will be initialized with a screen width SCREEN-WIDTH,
SCREEN-HEIGHT and a background color with index BACKGROUND-COLOR.

When setup is completed will switch to BUFFER-NAME
if SWITCH-TO-BUFFER-P is t."
  (chip8--retro-check-requirements)
  (select-window (or (get-buffer-window buffer-name)
                     (selected-window)))
  (with-current-buffer (get-buffer-create buffer-name)
    ;; Disable mode-line before calibration
    (let* ((window (selected-window))
           ;; NOTE: without switching to buffer, buffer calibration is not
           ;; reliable, I didn't find a way to make it work but since we don't
           ;; need precision because we are not looking at the buffer, a dummy
           ;; but credible calibration will do
           (calibration (if switch-to-buffer-p
                            (chip8--retro-calibrate-canvas-in-window
                             screen-width
                             screen-height
                             window)
                          (list 10 screen-width screen-height)))
           (pixel-size (nth 0 calibration))
           (window-width (nth 1 calibration))
           (window-height (nth 2 calibration)))
      (when (not calibration) (error "Failed to calibrate pixel size in buffer %s" buffer-name))
      ;; Buffer settings to not display text but display graphics
      (erase-buffer)
      (buffer-disable-undo)
      (font-lock-mode -1)
      (mouse-wheel-mode -1)
      (auto-save-mode -1)
      (set-buffer-multibyte nil)
      (setq-local visible-cursor nil
                  hl-line-mode nil
                  mode-line-format nil
                  cursor-type nil
                  inhibit-modification-hooks t
                  inhibit-compacting-font-caches t
                  bidi-inhibit-bpa t
                  bidi-display-reordering nil
                  bidi-paragraph-direction 'left-to-right)
      ;; Buffer initialization with background pixels
      (goto-char (point-min))
      (set-face-attribute 'chip8--retro-default-face nil :height pixel-size)
      (buffer-face-set 'chip8--retro-default-face)
      (let* ((margin-top (/ (- window-height screen-height) 2))
             (margin-left (/ (- window-width screen-width) 2))
             (canvas (chip8--retro-canvas-create :margin-left margin-left
                                                 :margin-top margin-top
                                                 :width screen-width
                                                 :height screen-height
                                                 :background-color background-color))
             (margin-top-string (propertize (make-string (+ margin-left screen-width) 32) 'face 'default))
             (margin-left-string (propertize (make-string margin-left 32) 'face 'default))
             (canvas-string (propertize (make-string screen-width 32) 'face (aref chip8--retro-palette-faces background-color))))
        (setq-local buffer-read-only nil)
        (dotimes (_ margin-top)
          (insert margin-top-string)
          (insert "\n"))
        (dotimes (_ screen-height)
          (insert margin-left-string)
          (insert canvas-string)
          (insert "\n"))
        (setq-local buffer-read-only t)
        (when switch-to-buffer-p
          (switch-to-buffer buffer-name))
        canvas))))

(defun chip8--retro-calibrate-canvas-in-window (width height window)
  "Return optimal size of pixel in WINDOW for canvas WIDTH x HEIGHT.

We want to calculate the size in pixel of a single character
coming from `retro-square-font-family (a pixel of our canvas) so
that we will minimize the margin of the canvas with the wanted
resolution in WINDOW."
  (let* ((min-pixel-size 1)
         (max-pixel-size 300)
         (current-pixel-size nil)
         (result nil)
         (stop nil))
    (while (not stop)
      (setq current-pixel-size (+ (/ (- max-pixel-size min-pixel-size) 2) min-pixel-size))
      (if (eq min-pixel-size max-pixel-size)
          (setq stop t)
        (with-temp-buffer
          (when display-line-numbers
            (display-line-numbers-mode -1))
          (set-window-buffer window (current-buffer))
          (set-face-attribute 'chip8--retro-default-face nil :height current-pixel-size)
          (buffer-face-set 'chip8--retro-default-face)
          ;; TODO (setq-local mode-line-format nil)
          (let* ((window-width (window-body-width window t))
                 (font-width (window-font-width window))
                 ;; window-mode-line-height lies with doom-modeline
                 ;; TODO: remove this since we remove the modeline when we run?
                 (mode-line-height (or (and (boundp 'doom-modeline-mode) doom-modeline-mode
                                            (boundp 'doom-modeline-height) doom-modeline-height)
                                       (window-mode-line-height window)))
                 (window-height (- (window-body-height window t) (window-header-line-height window) mode-line-height))
                 (font-height (window-font-height window))
                 (n-columns (/ window-width font-width))
                 (n-lines (floor (/ window-height font-height)))
                 (waste (+ (- n-columns width) (- n-lines height))))
            ;; (message "current-pixel-size: %S [%S, %S]" current-pixel-size min-pixel-size max-pixel-size)
            ;; (message "n-columns: %S (< %S)" n-columns width)
            ;; (message "n-lines: %S (< %S)" n-lines height)
            ;; (message "waste: %S (< %S)" waste (and result (car result)))
            (if (or (< n-columns width) (< n-lines height))
                ;; current-pixel-size is too big
                (setq max-pixel-size current-pixel-size)
              ;; current-pixel-size is ok
              (if (or (not (car result)) (< waste (car result)))
                  ;; we did improve
                  (setq min-pixel-size current-pixel-size
                        result (list waste current-pixel-size n-columns n-lines))
                ;; we did not improve, bail
                (setq stop t)))))))
    (set-face-attribute 'chip8--retro-default-face nil :height chip8--retro-default-face-height)
    (cdr result)))

(defun chip8--retro-init-color-palette (colors offset)
  "Initialize retro palette with COLORS starting from OFFSET."
  ;; TODO: colors are list of three color components RGB
  ;; TODO: explain offet
  (setq chip8--retro-palette-faces (make-vector chip8--retro-palette-size 0))
  (dotimes (i (length colors))
    (let* ((color (aref colors i))
           (color-hex (format "#%02X%02X%02X"
                              (nth 0 color)
                              (nth 1 color)
                              (nth 2 color)))
           (palette-index (+ offset i))
           (face-name (intern (format "chip8--retro-mode-face-%s" (substring color-hex 1)))))
      (eval `(defface ,face-name
               '((t :inherit chip8--retro-default-face :background ,color-hex))
               ,(format "Face for pixel with color %s" color-hex)
               :group 'chip8))
      (aset chip8--retro-palette-faces palette-index face-name))))

(defun chip8--retro-buffer-render (current-canvas previous-canvas)
  "Render CURRENT-CANVAS given PREVIOUS-CANVAS into current buffer."
  (let* ((cpxs (chip8--retro-canvas-pixels current-canvas))
         (ppxs (chip8--retro-canvas-pixels previous-canvas))
         (width (chip8--retro-canvas-width current-canvas))
         (height (chip8--retro-canvas-height current-canvas))
         (bll (chip8--retro-canvas-buffer-line-length current-canvas))
         (cl (* width height))          ; canvas length
         (column 0)
         (bbcll (+ (chip8--retro-canvas-buffer-before-length current-canvas)
                   (chip8--retro-canvas-margin-left current-canvas)
                   1))
         (start 0)
         (buffer-start nil)
         (buffer-end nil)
         (length 0)
         (cpc nil)                      ; current-canvas previous color
         (ccc nil)                      ; current-canvas current color
         (pcc nil)                      ; previous-canvas current color
         (i 0))
    (setq-local buffer-read-only nil)
    (catch 'stop
      (while (< i cl)
        ;; line routine
        (setq cpc ccc
              ccc (aref cpxs i)
              pcc (aref ppxs i))
        ;; if previous canvas pixel and current canvas pixel are the same
        (while (and (eq ccc pcc) (< column width))
          ;; skip those pixels and do nothing, the current canvas is ok as it is
          (setq i (1+ i)
                column (1+ column)
                cpc ccc)
          (when (>= i cl)
            (throw 'stop nil))
          (setq ccc (aref cpxs i)
                pcc (aref ppxs i)))
        ;; start a stroke
        (when (< column width)
          (setq start column
                length 1
                i (1+ i)
                column (1+ column)
                cpc ccc)
          (when (>= i cl)
            (throw 'stop nil))
          (setq ccc (aref cpxs i)))
        ;; if previous pixel and current pixel are the same
        (while (and (eq cpc ccc) (< column width))
          ;; accumulate pixels in the current stroke
          (setq i (1+ i)
                column (1+ column)
                length (1+ length)
                cpc ccc)
          (when (>= i cl)
            (throw 'stop nil))
          (setq ccc (aref cpxs i)))
        ;; plot the stroke
        (setq buffer-start (+ bbcll start)
              buffer-end (+ buffer-start length))
        (put-text-property buffer-start buffer-end 'face (aref chip8--retro-palette-faces cpc))
        (setq length 0
              start 0)
        ;; next line
        (when (>= column width)
          (setq column 0
                cpc nil
                ccc nil
                pcc nil
                bbcll (+ bbcll bll)))))
    (when (> length 0)
      (setq buffer-start (+ bbcll start)
            buffer-end (+ buffer-start length))
      (put-text-property buffer-start buffer-end 'face (aref chip8--retro-palette-faces cpc)))
    (setq-local buffer-read-only t)))

(defun chip8--retro-canvas-pixels-pixel (x y pixels width)
  "Get pixel color at (X, Y) in PIXELS with a certain WIDTH."
  (aref pixels (+ (* y width) x)))

;;; The previous stuff is vendoring of https://github.com/gabrielelana/retro.el

;;; TODO: remove
(defun chip8--example ()
  "This is an example."
  2)

;;; TODO: documentation
(defconst chip8-SCREEN-WIDTH 128)
(defconst chip8-SCREEN-HEIGHT 64)

(defconst chip8-KEY-RELEASE-TIMEOUT 0.15
  "Time between the keypress event and the simulation of the keyrelease event.")

(defconst chip8-RAM-SIZE 4096
  "Dimension of the RAM.")

(defconst chip8-FRAME-DURATION 0.02
  "Duration of a single frame in emuation.")

(defconst chip8-INSTRUCTIONS-PER-FRAME 30
  "Number of instructions to execute per frame.")

;;; TODO: make the theme configurable
(defconst chip8-COLORS
  ;; [(#x00 #x00 #x00) (#xFF #xFF #xFF)]
  [(#x99 #x66 #x01) (#xFF #xCC #x01)]
  "List of colors supported by the emulator, they are indexed starting from zero.")

(defconst chip8-BUFFER-NAME "*chip8-EMULATOR*"
  "The name of the buffer used to show the emulator.")

(defconst chip8-FONT-ADDRESS #x50
  "Address where to find/load default font in RAM.")

(defconst chip8-HIRES-FONT-ADDRESS #xA1
  "Address where to find/load default hires font in RAM.")

(defconst chip8-ROM-ADDRESS #x200
  "Address where to find/load ROM to execute in RAM.")

(defconst chip8-FONT [ #xF0 #x90 #x90 #x90 #xF0 ; 0
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

(defconst chip8-HIRES-FONT [ 0x3C, 0x7E, 0xE7, 0xC3, 0xC3, 0xC3, 0xC3, 0xE7, 0x7E, 0x3C, ; 0
                             0x18, 0x38, 0x58, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x3C, ; 1
                             0x3E, 0x7F, 0xC3, 0x06, 0x0C, 0x18, 0x30, 0x60, 0xFF, 0xFF, ; 2
                             0x3C, 0x7E, 0xC3, 0x03, 0x0E, 0x0E, 0x03, 0xC3, 0x7E, 0x3C, ; 3
                             0x06, 0x0E, 0x1E, 0x36, 0x66, 0xC6, 0xFF, 0xFF, 0x06, 0x06, ; 4
                             0xFF, 0xFF, 0xC0, 0xC0, 0xFC, 0xFE, 0x03, 0xC3, 0x7E, 0x3C, ; 5
                             0x3E, 0x7C, 0xC0, 0xC0, 0xFC, 0xFE, 0xC3, 0xC3, 0x7E, 0x3C, ; 6
                             0xFF, 0xFF, 0x03, 0x06, 0x0C, 0x18, 0x30, 0x60, 0x60, 0x60, ; 7
                             0x3C, 0x7E, 0xC3, 0xC3, 0x7E, 0x7E, 0xC3, 0xC3, 0x7E, 0x3C, ; 8
                             0x3C, 0x7E, 0xC3, 0xC3, 0x7F, 0x3F, 0x03, 0x03, 0x3E, 0x7C  ; 9
                             ]
  "Default font loaded as sprites in CHIP-8 RAM for SUPER-CHIP hi resolution mode.")

(defvar chip8--current-rom-filename nil
  "File path of ROM loaded in the running CHIP-8 emulator.")

(defvar chip8--current-quirks nil
  "Current set of quirks used by CHIP-8 emulator..")

(defvar chip8--current-instance nil
  "Current instance of running CHIP-8 emulator.")

(defvar chip8-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'chip8-quit)
    (define-key map (kbd "0") (lambda () (interactive) (chip8--key-press #x0)))
    (define-key map (kbd "1") (lambda () (interactive) (chip8--key-press #x1)))
    (define-key map (kbd "2") (lambda () (interactive) (chip8--key-press #x2)))
    (define-key map (kbd "3") (lambda () (interactive) (chip8--key-press #x3)))
    (define-key map (kbd "4") (lambda () (interactive) (chip8--key-press #x4)))
    (define-key map (kbd "5") (lambda () (interactive) (chip8--key-press #x5)))
    (define-key map (kbd "6") (lambda () (interactive) (chip8--key-press #x6)))
    (define-key map (kbd "7") (lambda () (interactive) (chip8--key-press #x7)))
    (define-key map (kbd "8") (lambda () (interactive) (chip8--key-press #x8)))
    (define-key map (kbd "9") (lambda () (interactive) (chip8--key-press #x9)))
    (define-key map (kbd "a") (lambda () (interactive) (chip8--key-press #xA)))
    (define-key map (kbd "b") (lambda () (interactive) (chip8--key-press #xB)))
    (define-key map (kbd "c") (lambda () (interactive) (chip8--key-press #xC)))
    (define-key map (kbd "d") (lambda () (interactive) (chip8--key-press #xD)))
    (define-key map (kbd "e") (lambda () (interactive) (chip8--key-press #xE)))
    (define-key map (kbd "f") (lambda () (interactive) (chip8--key-press #xF)))
    (define-key map [t] #'ignore)
    map)
  "Keymap for `chip8-mode'.")

(cl-defstruct chip8-quirks
  "Quirks configuration for a CHIP-8 emulator.

Quirks are little differences in opcodes interpretation by
different CHIP-8 interpreters developed through the years.

The following list of quirks is taken from
https://github.com/chip-8/chip-8-database/blob/master/database/quirks.json"
  (shift nil :documentation "On most systems the shift opcodes take
`vY` as input and stores the shifted version of `vY` into `vX`.
The interpreters for the HP48 took `vX` as both the input and the
output, introducing the shift quirk. If t opcodes `8XY6` and
`8XYE` take `vX` as both input and output. If nil opcodes `8XY6`
and `8XYE` take `vY` as input and `vX` as output.")
  (increment-i-by-x nil :documentation "On most systems storing
 and retrieving data between registers and memory increments the
`i` register with `X + 1` (the number of registers read or
written). So for each register read or writen, the index register
would be incremented. The CHIP-48 interpreter for the HP48 would
only increment the `i` register by `X`, introducing the first
load/store quirk. If t opcodes `FX55` and `FX65` increment the
`i` register with `X`. If nil opcodes `FX55` and `FX65` increment
the `i` register with `X + 1`.")
  (leave-i-unchanged t :documentation "On most systems storing and
 retrieving data between registers and memory increments the `i`
register relative to the number of registers read or written. The
Superchip 1.1 interpreter for the HP48 however did not increment
the `i` register at all, introducing the second load/store quirk.
If t opcodes `FX55` and `FX65` leave the `i` register unchanged.
If nil opcodes `FX55` and `FX65` increment the `i` register.")
  (wrap nil :documentation "Most systems, when drawing sprites to
 the screen, will clip sprites at the edges of the screen. The
Octo interpreter, which spawned the XO-CHIP variant of CHIP-8,
instead wraps the sprite around to the other side of the screen.
This introduced the wrap quirk. If t the `DXYN` opcode wraps
around to the other side of the screen when drawing at the edges.
If nil the `DXYN` opcode clips when drawing at the edges of the
screen.")
  (jump nil :documentation "The jump to `<address> + v0` opcode was
 wronly implemented on all the HP48 interpreters as jump to
`<address> + vX`, introducing the jump quirk. If t opcode `BXNN`
jumps to address `XNN + vX`. If nil opcode `BNNN` jumps to
address `NNN + v0`.")
  (vblank nil :documentation "The original Cosmac VIP interpreter
 would wait for vertical blank before each sprite draw. This was
done to prevent sprite tearing on the display, but it would also
act as an accidental limit on the execution speed of the program.
Some programs rely on this speed limit to be playable. Vertical
blank happens at 60Hz, and as such its logic be combined with the
timers. If t opcode `DXYN` waits for vertical blank (so max 60
sprites drawn per second). If nil opcode `DXYN` draws
immediately (number of sprites drawn per second only limited to
number of CPU cycles per frame).")
  (logic t :documentation "On the original Cosmac VIP interpreter,
`vF` would be reset after each opcode that would invoke the maths
coprocessor. Later interpreters have not copied this behaviour.
If t opcodes `8XY1`, `8XY2` and `8XY3` (OR, AND and XOR) will set
`vF` to zero after execution (even if `vF` is the parameter `X`).
If nil opcodes `8XY1`, `8XY2` and `8XY3` (OR, AND and XOR) will
leave `vF` unchanged (unless `vF` is the parameter `X`).")
  (count-collisions t :documentation "On the original Cosmac VIP
interpreter, would set the `vF` register to 0x1 when a collision
was detected after `DXYN` instruction. The Superchip 1.1
interpreter will punt in `vF` the number of sprite rows collision
plus the number of the rows clipped at the bottom border."))

(defconst chip8--original-quirks
  (make-chip8-quirks
   :shift nil
   :increment-i-by-x nil
   :leave-i-unchanged nil
   :wrap nil
   :jump nil
   :vblank t
   :logic t
   :count-collisions nil)
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
   :logic nil
   :count-collisions nil)
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
   :logic nil
   :count-collisions t)
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
   :logic nil
   :count-collisions t)
  "Quirks of XO-CHIP CHIP-8 implementation.
See https://github.com/chip-8/chip-8-database/blob/master/database/platforms.json")

(cl-defstruct chip8
  "CHIP-8 emulator state."
  (ram (make-vector chip8-RAM-SIZE 0) :documentation "4K of ram")
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
  (quirks (make-chip8-quirks) :documentation "Quirks configuration to use in the emulator")
  (display-scale nil :documentation "Pixels scale factor. Low resolution (lores): 2. Hi resolution (hires): 1")
  (display-width nil :documentation "Pixels width of the display")
  (display-height nil :documentation "Pixels height of the display"))

(defgroup chip8 nil
  "A CHIP-8 emulator."
  :group 'games)

(defvar chip8-configure-on-sha1-alist
  '(("9df1689015a0d1d95144f141903296f9f1c35fc5" .
     ((:filename . "BC_test.ch8")
      (:description . "Test the conditional jumps, the mathematical and logical operations of Chip 8")
      (:url . "https://github.com/cj1128/chip8-emulator/blob/74bca24c32c2954955d2e520a71041001baf9e78/rom/BC_test.ch8")
      (:platform . "superchip")))
    ("5c28a5f85289c9d859f95fd5eadbdcb1c30bb08b" .
     ((:filename . "invaders.ch8")
      (:description . "Space Invaders (1978), by David Winter. The well known game. Destroy the invaders with your ship. Shoot with 5, move with 4 and 6. Press 5 to begin a game.")
      (:platform . "superchip"))))
  "Alist mapping a CHIP-8 ROM's SHA1 with the appropriate emulator configuration.

If you have a ROM that needs a specific configuration you can add
your ROM configuration to this list.

The elements of the list have the form (SHA1 . CONFIGURATION)
where SHA1 is the sha1 of the ROM's file and CONFIGURATION is an
alist with the following recognized keys

- :filename The original filename of the ROM (only for documentation purpose)
- :description Description of what the ROM does (only for documentation purpose)
- :url Canonical URL where to find the ROM file (only for documentation purpose)
- :platform Selects a platform configuration. Select a set of quirks and set of
  configurable parameters to match the behaviour of the platform. Supported
  platforms are: \"original\", \"modern\", \"superchip\", \"xo-chip\".")

(define-derived-mode chip8-mode nil "CHIP-8 Emulator"
  (use-local-map chip8-mode-map)
  (setq chip8--current-instance (chip8--setup
                                 chip8--current-rom-filename
                                 chip8--current-quirks
                                 t))
  (run-at-time 0.001 nil #'chip8--run))

;;;###autoload
(defun chip8 (filename)
  "Run chip8 emulation with FILENAME ROM.

If called from function `chip8-PLATFORM' will run the ROM with
the configuration associated with PLATFORM.

If filename SHA1 is found in `chip8-configure-on-sha1-alist'
alist then will run the ROM with the associated configuration.

Otherwise will run the ROM as the original platform."
  (interactive "ffilename: ")
  (when (or (not (file-exists-p filename)) (not (file-readable-p filename)) (file-directory-p filename))
    (user-error "ROM file %s does not exists or is not readable" filename))
  (select-window (or (get-buffer-window chip8-BUFFER-NAME)
                     (selected-window)))
  (switch-to-buffer chip8-BUFFER-NAME)
  (setq chip8--current-rom-filename filename
        chip8--current-quirks (or chip8--current-quirks
                                  (chip8--quirks-from-sha (chip8--sha1-rom filename))
                                  chip8--original-quirks))
  (chip8-mode))

;;;###autoload
(defun chip8-original (filename)
  "Run chip8 emulation loading FILENAME rom with original Cosmac VIP CHIP-8 quirks."
  (interactive "ffilename: ")
  (setq chip8--current-quirks chip8--original-quirks)
  (chip8 filename))

;;;###autoload
(defun chip8-modern (filename)
  "Run chip8 emulation loading FILENAME rom with modern CHIP-8 quirks."
  (interactive "ffilename: ")
  (setq chip8--current-quirks chip8--modern-quirks)
  (chip8 filename))

;;;###autoload
(defun chip8-superchip (filename)
  "Run chip8 emulation loading FILENAME rom with superchip CHIP-8 quirks."
  (interactive "ffilename: ")
  (setq chip8--current-quirks chip8--superchip-quirks)
  (chip8 filename))

;;;###autoload
(defun chip8-xo-chip (filename)
  "Run chip8 emulation loading FILENAME rom with superchip CHIP-8 quirks."
  (interactive "ffilename: ")
  (setq chip8--current-quirks chip8--xo-chip-quirks)
  (chip8 filename))

(defun chip8--key-press (keycode)
  "Will emulate the key press of KEYCODE in current EMULATOR."
  (aset (chip8-keys chip8--current-instance) keycode (current-time))
  (run-at-time chip8-KEY-RELEASE-TIMEOUT nil #'chip8--key-release keycode))

(defun chip8--key-release (keycode)
  "Will emulate the key release of KEYCODE in current EMULATOR."
  (let ((key-pressed-at (aref (chip8-keys chip8--current-instance) keycode)))
    (when (>= (float-time (time-subtract (current-time) key-pressed-at)) chip8-KEY-RELEASE-TIMEOUT)
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
           until element
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
  (kill-buffer chip8-BUFFER-NAME))

(defun chip8--setup (filename quirks switch-to-buffer-p)
  "Setup game with rom FILENAME and QUIRKS.

Switch to CHIP-8 buffer when SWITCH-TO-BUFFER-P is \\='t'."
  (chip8--retro-init-color-palette chip8-COLORS 0)
  (let ((ram (make-vector chip8-RAM-SIZE 0))
        (canvas (chip8--retro-init-buffer
                 chip8-BUFFER-NAME
                 chip8-SCREEN-WIDTH
                 chip8-SCREEN-HEIGHT
                 0
                 switch-to-buffer-p)))
    (chip8--load-default-font ram)
    (chip8--load-rom filename ram)
    (make-chip8
     :pc chip8-ROM-ADDRESS
     :ram ram
     :quirks quirks
     :current-canvas canvas
     :previous-canvas (chip8--retro-canvas-copy canvas)
     :display-scale 2
     :display-width (/ chip8-SCREEN-WIDTH 2)
     :display-height (/ chip8-SCREEN-HEIGHT 2))))

(defun chip8--load-default-font (ram)
  "Load default font in CHIP-8 RAM."
  (dotimes (i (length chip8-FONT))
    (aset ram (+ i chip8-FONT-ADDRESS) (aref chip8-FONT i)))
  (dotimes (i (length chip8-HIRES-FONT))
    (aset ram (+ i chip8-HIRES-FONT-ADDRESS) (aref chip8-HIRES-FONT i))))

(defun chip8--load-rom (filename ram)
  "Load rom FILENAME in CHIP-8 RAM."
  (let ((rom (seq-into
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (setq buffer-file-coding-system 'binary)
                (insert-file-contents-literally filename nil 0)
                (buffer-substring-no-properties (point-min) (point-max)))
              'vector)))
    (dotimes (i (length rom))
      (aset ram (+ i chip8-ROM-ADDRESS) (aref rom i)))))

(defun chip8--run ()
  "Make the current game run."
  (let ((buffer (get-buffer chip8-BUFFER-NAME))
        (current-canvas nil)
        (previous-canvas nil)
        (last-frame-at 0)
        (instructions-counter 0)
        (elapsed 0))
    ;; it runs only when we are in the emulator buffer
    (when (eq (current-buffer) buffer)
      (when (not chip8--current-instance)
        (error "Missing emulator instance (this should not happen)"))
      (setq last-frame-at (chip8-last-frame-at chip8--current-instance)
            current-canvas (chip8-current-canvas chip8--current-instance)
            previous-canvas (chip8-previous-canvas chip8--current-instance))
      (while (< elapsed chip8-FRAME-DURATION)
        (if (>= instructions-counter chip8-INSTRUCTIONS-PER-FRAME)
            (sleep-for 0.001)
          (chip8--step chip8--current-instance)
          (setq instructions-counter (1+ instructions-counter)))
        (setq elapsed (float-time (time-subtract (current-time) last-frame-at))))
      ;; (message "FPS: %f, elapsed: %fs" (/ 1.0 elapsed) elapsed)
      (chip8--retro-buffer-render current-canvas previous-canvas)
      (chip8--retro-canvas-pixels-copy current-canvas previous-canvas)
      (setf (chip8-last-frame-at chip8--current-instance) (current-time)
            (chip8-delay-timer chip8--current-instance) (max 0 (1- (chip8-delay-timer chip8--current-instance)))
            (chip8-sound-timer chip8--current-instance) (max 0 (1- (chip8-sound-timer chip8--current-instance))))
      (run-at-time 0.001 nil #'chip8--run))))

(defun chip8--step (emulator)
  "Run a single step of fetch/decode of the EMULATOR."
  (let* ((nimbles (chip8--fetch16 emulator))
         (opcode (logand nimbles #xF000))
         (opcode3 (logand nimbles #xFFF0)))
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
      (chip8--retro-reset-canvas (chip8-current-canvas emulator))
      (cl-incf (chip8-pc emulator) 2))
     ((eq nimbles #x00EE)
      ;; 00EE - RET
      ;; Return from a subroutine.
      ;; The interpreter sets the program counter to the address at the top of
      ;; the stack, then subtracts 1 from the stack pointer.
      (setf (chip8-pc emulator) (pop (chip8-stack emulator))))
     ((eq nimbles #x00FE)
      ;; 00FD - Exit
      (chip8-quit))
     ((eq nimbles #x00FE)
      ;; 00FE - Disable hires
      (setf (chip8-display-scale emulator) 2
            (chip8-display-width emulator) (/ chip8-SCREEN-WIDTH 2)
            (chip8-display-height emulator) (/ chip8-SCREEN-HEIGHT 2))
      (cl-incf (chip8-pc emulator) 2))
     ((eq nimbles #x00FF)
      ;; 00FF - Enable hires
      (setf (chip8-display-scale emulator) 1
            (chip8-display-width emulator) chip8-SCREEN-WIDTH
            (chip8-display-height emulator) chip8-SCREEN-HEIGHT)
      (cl-incf (chip8-pc emulator) 2))
     ((eq nimbles #x00FB)
      ;; 00FB - Scroll right by 4 pixels; in low resolution mode, 2 pixels
      (chip8--scroll-right (/ 4 (chip8-display-scale emulator))
                           (chip8-current-canvas emulator))
      (cl-incf (chip8-pc emulator) 2))
     ((eq nimbles #x00FC)
      ;; 00FC - Scroll left by 4 pixels; in low resolution mode, 2 pixels
      (chip8--scroll-left (/ 4 (chip8-display-scale emulator))
                          (chip8-current-canvas emulator))
      (cl-incf (chip8-pc emulator) 2))
     ((eq opcode3 #x00C0)
      ;; 00CN: Scroll display N pixels down; in low resolution mode, N/2 pixels
      (chip8--scroll-down (/ (logand nimbles #x000F)
                             (chip8-display-scale emulator))
                          (chip8-current-canvas emulator))
      (cl-incf (chip8-pc emulator) 2))
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
      (let* ((x (mod (chip8--vx emulator nimbles) (chip8-display-width emulator)))
             (y (mod (chip8--vy emulator nimbles) (chip8-display-height emulator)))
             (n (logand nimbles #x000F))
             (tall n)
             (wide 8)
             (count-collisions (chip8-quirks-count-collisions (chip8-quirks emulator)))
             sprite collisions)
        ;; TODO: (chip8--hires? emulator)
        ;; TODO: (chip8--lores? emulator)
        ;; Dxy0 - 16x16 pixels sprite in superchip hires mode
        (when (and (eq (chip8-display-scale emulator) #x1) (eq n 0))
          (setq n 32
                wide 16
                tall 16))
        (setq sprite (chip8--read-bytes emulator n (chip8-i emulator))
              collisions (chip8--draw-sprite emulator x y tall wide sprite count-collisions))
        (setf (aref (chip8-v emulator) #xF) (if count-collisions
                                                (logand collisions #xFF)
                                              (when (> collisions 0) #x0 #x1)))
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
          (setf (chip8-i emulator) (+ chip8-FONT-ADDRESS (* (chip8--vx emulator nimbles) 5)))
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x30)
          ;; FX30 - Point I to 10-byte font sprite for digit VX (only digits 0-9)
          ;; (when (< (chip8--vx emulator nimbles) #xA)
          ;;   (setf (chip8-i emulator) (+ chip8-HIRES-FONT-ADDRESS (* (chip8--vx emulator nimbles) 10))))
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
         ((eq last-byte #x75)
          ;; Fx75 - Store V0..VX in RPL user flags (X <= 7)
          ;; TODO: implement this, how? RPL should be persistent?
          (cl-incf (chip8-pc emulator) 2))
         ((eq last-byte #x85)
          ;; Fx85 - Read V0..VX from RPL user flags (X <= 7)
          ;; TODO: implement this, how? RPL should be persistent?
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


(defsubst chip8--retro-plot-pixel (x y color pixels width)
  "Plot a pixel with COLOR at (X, Y) in CANVAS."
  (aset pixels (+ (* y width) x) color))

(defun chip8--draw-sprite (emulator x y tall wide sprite count-clipped)
  "Draw SPRITE on EMULATOR's canvas at coordinates (X, Y).

The sprite is TALL bits tall and WIDE bits wide. In hires mode
the sprite is always 8 bits wide and TALL (< 15) bits tall. In
hires mode the sprite can be 16 bits wide and TALL (<= 16) bits
tall.

Every bit of the sprite is a pixel, if the bit is 1 then we need
to turn \"on\" the corresponding pixel, otherwise we need to turn
it \"off\".

Returns the number of collisions (aka if any pixel on the CANVAS
was turned off) plus the number of rows clipped at the bottom of
the screen if COUNT-CLIPPED is t."
  (let ((collisions 0)
        (canvas-pixels (chip8--retro-canvas-pixels (chip8-current-canvas emulator)))
        (canvas-width (chip8--retro-canvas-width (chip8-current-canvas emulator)))
        (sprite-bits (* tall wide))
        (sprite-index (* tall wide))
        (sprite-rows-with-collision (make-vector tall #x0))
        (display-scale (chip8-display-scale emulator))
        (display-width (chip8-display-width emulator))
        (display-height (chip8-display-height emulator))
        canvas-pixel
        sprite-pixel
        sprite-row
        xi yi xj yj)
    (when count-clipped
      (cl-incf collisions (max 0 (- (+ y (/ tall 8)) display-height))))
    (dotimes (yd tall)
      (dotimes (xd wide)
        (setq xi (+ x xd)
              yi (+ y yd)
              sprite-index (1- sprite-index)
              sprite-pixel (ash (logand sprite (ash #x1 sprite-index)) (- sprite-index)))
        (when (chip8-quirks-wrap (chip8-quirks emulator))
          (setq xi (mod xi display-width)
                yi (mod yi display-height)))
        (when (and (< xi display-width) (< yi display-height))
          (dotimes (ys display-scale)
            (dotimes (xs display-scale)
              (setq xj (+ (* xi display-scale) xs)
                    yj (+ (* yi display-scale) ys)
                    canvas-pixel (chip8--retro-canvas-pixels-pixel xj yj canvas-pixels canvas-width))
              (when (and (> canvas-pixel #x0) (> sprite-pixel #x0))
                (setq sprite-row (/ (- sprite-bits (1+ sprite-index)) wide))
                (setf (aref sprite-rows-with-collision sprite-row) #x1))
              (chip8--retro-plot-pixel
               xj
               yj
               (logxor canvas-pixel sprite-pixel)
               canvas-pixels
               canvas-width))))))
    (+ collisions (apply #'+ (seq-into sprite-rows-with-collision 'list)))))

;; TODO: write tests
;; TODO: use background color
(defun chip8--scroll-down (n c)
  "Scroll N pixels down what's represented in C."
  (let ((pixels (chip8--retro-canvas-pixels c))
        (width (chip8--retro-canvas-width c)))
    (setf (chip8--retro-canvas-pixels c)
          (vconcat
           (make-vector (* n width) #x0)
           (seq-subseq pixels 0 (- (length pixels) (* n width)))))))

;; TODO: write tests
;; TODO: use background color
(defun chip8--scroll-up (n canvas)
  "Scroll N pixels down what's represented in CANVAS."
  (let* ((pixels (chip8--retro-canvas-pixels canvas))
         (width (chip8--retro-canvas-width canvas)))
    (setf (chip8--retro-canvas-pixels canvas)
          (vconcat
           (seq-subseq pixels (* n width))
           (make-vector (* n width) #x0)))))

;; TODO: write tests
;; TODO: use background color
(defun chip8--scroll-right (n canvas)
  "Scroll N pixels right what's represented in CANVAS."
  (let* ((pixels (chip8--retro-canvas-pixels canvas))
         (width (chip8--retro-canvas-width canvas))
         (height (chip8--retro-canvas-height canvas)))
    (setf (chip8--retro-canvas-pixels canvas)
          (apply #'vconcat
                 (cl-loop for i below height
                          for rows = (cons (vconcat (make-vector n #x0)
                                                    (seq-subseq pixels (* i width) (- (* (1+ i) width) n)))
                                           rows)
                          finally (return (seq-reverse rows)))))))

;; TODO: write tests
;; TODO: use background color
(defun chip8--scroll-left (n canvas)
  "Scroll N pixels left what's represented in CANVAS."
  (let* ((pixels (chip8--retro-canvas-pixels canvas))
         (width (chip8--retro-canvas-width canvas))
         (height (chip8--retro-canvas-height canvas)))
    (setf (chip8--retro-canvas-pixels canvas)
          (apply #'vconcat
                 (cl-loop for i below height
                          for rows = (cons (vconcat (seq-subseq pixels (+ (* i width) n) (* (1+ i) width))
                                                    (make-vector n #x0))
                                           rows)
                          finally (return (seq-reverse rows)))))))

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

(defun chip8--quirks-from-sha (rom-sha)
  "Return emulator quirks of a ROM which sha1 is ROM-SHA.

Return quirks if ROM is found in in associated list
`chip8-configure-on-sha1-alist', otherwise return nil."
  (let* ((configuration (alist-get rom-sha chip8-configure-on-sha1-alist nil nil 'equal))
         (platform (when configuration (alist-get :platform configuration)))
         (quirks-s (when platform (intern (format "chip8--%s-quirks" platform)))))
    (when (and platform (not (boundp quirks-s)))
      (user-error "Found SHA %s in `chip8-configure-on-sha1-alist' but specified platform %s is not supported"
                  rom-sha platform))
    (if platform
        (symbol-value quirks-s)
      nil)))

(defun chip8--sha1-rom (filepath)
  "Return SHA1 of FILEPATH if exists."
  (when (or (not (file-exists-p filepath)) (not (file-readable-p filepath)))
    (user-error "File %s do not exists or is not reable" filepath))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally filepath nil 0)
    (sha1 (current-buffer))))

(provide 'chip8)

;; Local Variables:
;; coding: utf-8
;; End:
;;; chip8.el ends here
