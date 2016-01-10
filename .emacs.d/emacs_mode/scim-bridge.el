;;; scim-bridge.el

;; Copyright (C) 2008 S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Input Method, i18n

(defconst scim-mode-version "0.7.1")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; The Smart Common Input Method platform (SCIM) is an input
;; method (IM) platform containing support for more than thirty
;; languages (CJK and many European languages) for POSIX-style
;; operating systems including Linux and BSD.

;; This program is SCIM-Bridge client for GNU Emacs. It is, however,
;; not part of official SCIM-Bridge.

;;
;; Installation:
;;
;; First, save this file as scim-bridge.el and byte-compile in
;; a directory that is listed in load-path.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'scim-bridge)
;;
;; After that, execute Emacs by typing on command line:
;;
;;   XMODIFIERS=@im=none emacs
;;
;; and turn on scim-mode:
;;
;;   M-x scim-mode
;;
;;
;; Here is the example of settings in .emacs:
;;
;;   (require 'scim-bridge)
;;   ;; Use C-SPC for Set Mark command
;;   (scim-define-common-key ?\C-\  nil)
;;   ;; Use C-/ for Undo command
;;   (scim-define-common-key ?\C-/ nil)
;;   ;; Change cursor color depending on SCIM status
;;   (setq scim-cursor-color "red")
;;   ;; Turn on scim-mode automatically
;;   (scim-mode t)
;;
;;
;; Note that this program requires GNU Emacs 22 or later, and
;; doesn't work when Emacs is running on terminal emulator.
;;

;; History:
;; 2008-10-20  S. Irie
;;         * Add option about behavior in minibuffer
;;         * Bug fixes
;;         * Version 0.7.1
;;
;; 2008-10-04  S. Irie
;;         * Add options about cursor color, shape, and location
;;         * Add functions for lcalization
;;         * Bug fixes
;;         * Version 0.7.0
;;
;; 2008-09-08  S. Irie
;;         * Change default value of `scim-mode-local' into t
;;         * Disable keymap in ebrowse-tree-mode
;;         * Available for ERC (IRC Client)
;;         * Bug fixes
;;         * Version 0.6.9
;;
;; 2008-09-05  S. Irie
;;         * Add treatment for read-only text
;;         * Bug fixes
;;         * Version 0.6.8
;;
;; 2008-07-26  S. Irie
;;         * Comment out debug codes
;;         * Some unimportant changes
;;         * Version 0.6.7
;;
;; 2008-06-12  S. Irie
;;         * Available for Xming multi-window mode
;;         * Version 0.6.6
;;
;; 2008-06-05  S. Irie
;;         * Change default value of `scim-common-function-key-list'
;;         * Bug fixes
;;         * Version 0.6.5
;;
;; 2008-06-01  S. Irie
;;         * Add/modify documentation strings
;;         * Version 0.6.4
;;
;; 2008-05-24  S. Irie
;;         * Solve kana-RO key problem
;;         * Bug fix
;;         * Version 0.6.3
;;
;; 2008-05-22  S. Irie
;;         * Modify treatment of kana-RO key
;;         * Bug fix
;;         * Version 0.6.2
;;
;; 2008-05-20  S. Irie
;;         * Bug fix
;;         * Version 0.6.1
;;
;; 2008-05-18  S. Irie
;;         * Manage meta-to-alt mapping
;;         * Bug fix
;;         * Version 0.6.0
;;
;; 2008-05-15  S. Irie
;;         * Experimental first release
;;         * Version 0.5.0
;;
;; 2008-05-12  S. Irie
;;         * Version 0.4.0
;;
;; 2008-05-08  S. Irie
;;         * Version 0.3.0
;;
;; 2008-05-06  S. Irie
;;         * Version 0.2.0
;;
;; 2008-04-20  S. Irie
;;         * Version 0.1.0

;; ToDo:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup scim nil
  "The Smart Common Input Method platform"
  :prefix "scim-"
  :group 'editing :group 'wp)

;; Basic settings
(defgroup scim-basic nil
  "Settings of operation, such as mode management and keyboard"
  :group 'scim)

(defcustom scim-mode-local t
  "If the value is non-nil, IMContexts are registered for each buffer
so that the input method of buffers can be toggled individually.
Otherwise, the input method is globally toggled."
  :type 'boolean
  :group 'scim-basic)

(defcustom scim-imcontext-temporary-for-minibuffer t
  "If non-nil, an one-time IMContext is used for a minibuffer so that
the minibuffer always starts with SCIM's input status off. This option
is effective only when the option `scim-mode-local' is active (non-nil)."
  :type 'boolean
  :group 'scim-basic)

(defun scim-customize-key (var value)
  (set var value)
  (if (fboundp 'scim-update-key-bindings)
      (scim-update-key-bindings var)))

(defcustom scim-common-function-key-list
  '((control ".")
    (control ",")
    (control "<")
    (control ">")
    (control "/")
    (control " ")
    (shift " ")
    (control alt left)
    (control alt right)
    (control alt up)
    (control alt down)
    (zenkaku-hankaku)
    (henkan)
    (shift henkan)
    (muhenkan)
    (hiragana-katakana)
    (alt romaji)
    (f6)
    (f7)
    (f8)
    (shift f8)
    (f9)
    (f10)
    (f11)
    (f12)
    (kp-space)
    (kp-equal)
    (kp-multiply)
    (kp-add)
    (kp-separator)
    (kp-subtract)
    (kp-decimal)
    (kp-divide)
    (kp-0)
    (kp-1)
    (kp-2)
    (kp-3)
    (kp-4)
    (kp-5)
    (kp-6)
    (kp-7)
    (kp-8)
    (kp-9))
  "This list indicates which keystrokes SCIM takes over at both direct
insert mode and preediting mode. You can also add/remove the elements
using the function `scim-define-common-key'.
NOTICE: Don't set prefix keys in this option, such as ESC and C-x.
If you do so, operating Emacs might become impossible."
  :set 'scim-customize-key
  :type '(repeat (list :format "%v"
		       (set :format "%v"
			    :inline t
			    (const :format "M- " meta)
			    (const :format "C- " control)
			    (const :format "S- " shift)
			    (const :format "H- " hyper)
			    (const :format "s- " super)
			    (const :format "A- " alt))
		       (restricted-sexp :format "%v"
					:match-alternatives
					(symbolp stringp))))
  :group 'scim-basic)

(defcustom scim-preedit-function-key-list
  '((escape)
    (left)
    (right)
    (up)
    (down)
    (home)
    (end)
    (prior)
    (next)
    (return)
    (shift left)
    (shift right)
    (shift up)
    (shift down)
    (shift return)
    (tab)
    (iso-lefttab)
    (shift tab)
    (shift iso-lefttab)
    (backtab)
    (backspace)
    (delete)
    (kp-enter)
    (kp-tab))
  "This list indicates which keystrokes SCIM takes over when the
preediting area exists. You can also add/remove the elements using
the function `scim-define-preedit-key'."
  :set 'scim-customize-key
  :type '(repeat (list :format "%v"
		       (set :format "%v"
			    :inline t
			    (const :format "M- " meta)
			    (const :format "C- " control)
			    (const :format "S- " shift)
			    (const :format "H- " hyper)
			    (const :format "s- " super)
			    (const :format "A- " alt))
		       (restricted-sexp :format "%v"
					:match-alternatives
					(symbolp stringp))))
  :group 'scim-basic)

(defcustom scim-use-kana-ro-key nil
  "If you use Japanese kana typing method with jp-106 keyboard, turn
on (non-nil) this option to input a kana character `ろ' without pushing
the shift key.
 This option is made effectual by temporarily modifying the X-window
system's keyboard configurations with a shell command `xmodmap'."
  :set 'scim-customize-key
  :type 'boolean
  :group 'scim-basic)

(defcustom scim-key-release-delay nil
  "If you use Japanese thumb shift typing method on SCIM-Anthy with
jp-106 keyboard, set the time delay of the key release (in seconds).
Set a smaller value than the simultaneous pressing time setting of
SCIM-Anthy."
  :type '(choice (const :tag "none" nil)
		 (number :tag "delay (in seconds)"
			 :value 0.1))
  :group 'scim-basic)

(defcustom scim-undo-by-committed-string nil
  "If the value is nil, undo is performed bringing some short
committed strings together or dividing the long committed string
within the range which does not exceed 20 characters. Otherwise, undo
is executed every committed string."
  :type 'boolean
  :group 'scim-basic)

(defcustom scim-clear-preedit-when-unexpected-event nil
  "If the value is non-nil, the preediting area is cleared in the
situations that the unexpected event happens during preediting.
The unexpected event is, for example, that the string is pasted
with the mouse."
  :type 'boolean
  :group 'scim-basic)

;; Appearance
(defgroup scim-appearance nil
  "Faces, candidate window, etc."
  :group 'scim)

(defface scim-preedit-default-face
;  nil
  '((t :inherit underline))
  "This face indicates the whole of the preediting area."
  :group 'scim-appearance)

(defface scim-preedit-underline-face
  '((t :inherit underline))
  "This face corresponds to the text attribute `Underline' in SCIM
GUI Setup Utility."
  :group 'scim-appearance)

(defface scim-preedit-highlight-face
;  '((t :inherit (scim-preedit-underline-face highlight)))
  '((t :inherit highlight))
  "This face corresponds to the text attribute `Highlight' in SCIM
GUI Setup Utility."
  :group 'scim-appearance)

(defface scim-preedit-reverse-face
;  '((t :inherit scim-preedit-underline-face :inverse-video t))
  '((t :inverse-video t))
  "This face corresponds to the text attribute `Reverse' in SCIM
GUI Setup Utility."
  :group 'scim-appearance)

(defun scim-customize-cursor-color (var value)
  (set var value)
  (if (fboundp 'scim-set-cursor-color)
      (scim-set-cursor-color)))

(defcustom scim-cursor-color
  nil
  "If the value is a string, it specifies the cursor color applied
when SCIM is on. If a cons cell, its car and cdr are the cursor colors
which indicate that SCIM is on and off, respectively. The value nil
means that the cursor color is not controlled at all."
  :set 'scim-customize-cursor-color
  :type '(choice (const :tag "none (nil)" nil)
		 (color :tag "red" :format "red (%{sample%})\n" :value "red")
		 (color :tag "blue" :format "blue (%{sample%})\n" :value "blue")
		 (color :tag "green" :format "green (%{sample%})\n" :value "green")
		 (color :tag "other" :value "red")
		 (cons  :tag "other (ON . OFF)"
			(color :format "ON: %v (%{sample%})  " :value "red")
			(color :format "OFF: %v (%{sample%})\n" :value "blue")))
  :group 'scim-appearance)

(defcustom scim-cursor-type-for-candidate
  nil
  "This option specifies the cursor shape which is applied when the
preediting area shows conversion candidates. The value nil means that
the cursor shape is not changed."
  :type '(choice (const :tag "default (nil)" nil)
		 (const :tag "box" box)
		 (const :tag "bar" bar)
		 (cons :tag "bar (specify width)"
		       (const :format "" bar)
		       (integer :tag "width" :value 1))
		 (const :tag "invisible" (bar . 0)))
  :group 'scim-appearance)

(defcustom scim-put-cursor-on-candidate
  nil
  "When the preediting area shows conversion candidates, the cursor
is put on the selected segment if this option is non-nil. Otherwise,
the cursor is put to the tail of the preediting area."
  :type 'boolean
  :group 'scim-appearance)

(defcustom scim-adjust-window-x-position
;  'gnome
  nil
  "This option specifies whether the position of candidate window
is adjusted so that the inline candidate and the candidates in that
window may just line up in the vertical direction. If the value is
`gnome', the adjustment will be done using the font size setting of
GNOME desktop environment. Otherwise, if the value is given as an
integer, that indicates the amount of the gap from normal position
by the number of pixels.
 This is not suitable for input method of the type to which the
candidate window is always displayed such as SCIM-pinyin (chinese),
because there is a possibility that the window hides the cursor when
the cursor is on the bottom of screen."
  :type '(choice (const :tag "use GNOME's font size" gnome)
		 (integer :tag "specify by pixel number" :value 24)
		 (const :tag "off" nil))
  :group 'scim-appearance)

(defcustom scim-adjust-window-y-position
  t
  "If the value is non-nil, the vertical position of candidate window
is adjusted to the bottom of cursor by using a shell command `xwininfo'.
Otherwise, the adjustment isn't done and therefore the window might
be displayed a little below from the exact location."
  :type 'boolean
  :group 'scim-appearance)

(defcustom scim-prediction-window-position
  '(nil . nil)
  "(For Japanese IM only) The value should be given as (POS . ADJ).
If POS is non-nil, the forecast window is displayed under the head
of the preediting area. If the value of ADJ is non-nil, the horizontal
position of it is adjusted same as `scim-adjust-window-x-position'."
  :type '(cons
	       (choice :tag "Position"
		       (const :tag "Tail of preediting area" nil)
		       (const :tag "Head of preediting area" t))
	       (choice :tag "Adjustment"
		       (const :tag "same as conversion window" t)
		       (const :tag "off" nil)))
  :group 'scim-appearance)

(defcustom scim-mode-line-string " SCIM"
  "This variable specify a string that appears in the mode line
when scim-mode is active, and not otherwise. This string should be
a short string which starts with a space and represents scim-mode."
  :type 'string
  :group 'scim-appearance)

;; Advanced settings
(defgroup scim-expert nil
  "Advanced settings"
  :group 'scim)

(defcustom scim-focus-update-interval 0.3
  "The window focus is checked with this cycle measured in seconds."
  :type 'number
  :group 'scim-expert)

(defcustom scim-focus-update-interval-long 1.0
  "When window focus cannot be observed with a shell command `xprop'
and `xwininfo' is used instead, not `scim-focus-update-interval' but
this value is used as a cycle."
  :type 'number
  :group 'scim-expert)

(defcustom scim-kana-ro-x-keysym "F24"
  "When Japanese kana-RO key is used, this option specifies the
substitute KeySym name used in X window system for the key. This
program sets the substitute KeySym for backslash key to distinguish
it from yen-mark key."
  :set 'scim-customize-key
  :type 'string
  :group 'scim-expert)

(defcustom scim-kana-ro-key-symbol 'f24
  "When Japanese kana-RO key is used, this option specifies the event
corresponding to the substitute KeySym given in `scim-kana-ro-x-keysym'
as a symbol. This program sets the substitute KeySym for backslash key
to distinguish it from yen-mark key."
  :set 'scim-customize-key
  :type '(choice (symbol)
		 (const :tag "none" nil))
  :group 'scim-expert)

(defcustom scim-bridge-timeout 500
  "Don't change this setting as long as there is no special reason."
  :type 'integer
  :group 'scim-expert)

(defcustom scim-bridge-wait-reply t
  "Don't change this setting as long as there is no special reason."
  :type 'boolean
  :group 'scim-expert)

(defcustom scim-bridge-socket-use-script nil
  "If the value is non-nil, the script is forced to use for connecting
to scim-bridge agent through UNIX domain socket instead of a built-in
function `make-network-process'. This script is not usually necessary
for Emacs which major version is 22 or later."
  :type 'boolean
  :group 'scim-expert)

(defcustom scim-bridge-socket-script-path "~/bin"
  "The destination of the socket script. When the directory doesn't
exist, it is made automatically."
  :type 'directory
  :group 'scim-expert)

(defcustom scim-bridge-socket-script-name "scim-bridge-client.pl"
  "The filename of supplementary script."
  :type 'string
  :group 'scim-expert)

(defcustom scim-bridge-socket-script
  "#!/usr/bin/perl

use Socket;
use strict;
use FileHandle;
use threads;

my $name = shift;

socket(SOCKET, PF_UNIX, SOCK_STREAM, 0) || die \"socket: $!\";
connect(SOCKET, sockaddr_un($name)) || die \"connect: $!\";

my $thread = threads->new(\\&receive);

SOCKET->autoflush(1);
while (<>) {
    print SOCKET $_;
}
$thread->join;
exit;

sub receive {
    while (<SOCKET>) {
	print $_;
    }
}"
  "This is the real content of the supplementary script. The first
command line argument must be used as the socket name."
  :type 'string
  :group 'scim-expert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System settings and constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar scim-debug nil)
(defvar scim-log-buffer "*scim-bridge-log*")

;(defvar scim-bridge-socket-path "/tmp/scim-bridge-0.3.0.socket-1000@localhost:0.0")
(defvar scim-bridge-compat-version "0.3.0")
(defvar scim-bridge-socket-dir "/tmp/")
(defvar scim-bridge-socket-name "socket")
(defvar scim-bridge-name "scim-bridge")
(defvar scim-bridge-host-name "localhost")
(defvar scim-bridge-socket-path
  (let ((disp (getenv "DISPLAY")))
    (concat scim-bridge-socket-dir scim-bridge-name "-"
	    scim-bridge-compat-version "." scim-bridge-socket-name "-"
	    (number-to-string (user-uid)) "@" scim-bridge-host-name
	    (substring disp (string-match ":" disp)))))

(defvar scim-config-file "~/.scim/config"
  "The name of SCIM's configuration file, which is used to detect
the change of SCIM settings.")

(defvar scim-meta-key-exists
  (string< "" (shell-command-to-string "xmodmap -pke | grep '= Meta'"))
  "t is set in this variable if there is mata modifier key in the
keyboard. When automatic detection doesn't go well, please set the
value manually before scim-bridge.el is loaded.")

(defvar scim-tmp-buffer-name " *scim-bridge*"
  "This is working buffer name used for communicating with the agent.")

(defvar scim-incompatible-mode-hooks
  '(ediff-mode-hook ebrowse-tree-mode-hook w3m-mode-hook)
  "When these hooks run, scim-mode-map become invalid.")

(defvar scim-reply-alist
  '(
    ;; Status
    ("imengine_status_changed"		. scim-imengine-status-changed)
    ("imcontext_registered"		. scim-imcontext-registered)
    ("preedit_mode_changed"		. scim-preedit-mode-changed)
    ("focus_changed"			. scim-focus-changed)
    ("cursor_location_changed"		. scim-cursor-location-changed)
    ("key_event_handled"		. scim-key-event-handled)
    ("imcontext_deregister"		. scim-imcontext-deregister)
    ("imcontext_reseted"		. scim-imcontext-reseted)
    ;; Request
    ("forward_key_event"		. scim-forward-key-event)
    ("update_preedit"			. scim-update-preedit)
    ("set_preedit_shown"		. scim-set-preedit-shown)
    ("set_preedit_cursor_position"	. scim-set-preedit-cursor-position)
    ("set_preedit_string"		. scim-set-preedit-string)
    ("set_preedit_attributes"		. scim-set-preedit-attributes)
    ("set_commit_string"		. scim-set-commit-string)
    ("commit_string"			. scim-commit-string)
    ("get_surrounding_text"		. scim-get-surrounding-text)
    ("delete_surrounding_text"		. scim-delete-surrounding-text)
    ("replace_surrounding_text"		. scim-replace-surrounding-text)
    ("beep"				. scim-beep)
    ))

(defvar scim-modifier-alist
  `(
    ;; Keyboard
    (shift . "shift")
    (control . "control")
    (,(if scim-meta-key-exists 'alt 'meta) . "alt")
    (meta . "meta")
;    (super . "hyper")
    (super . "super")
    (hyper . "hyper")
    ;;
    (caps-lock . "caps_lock")
    (num-lock . "num_lock")
    (kana-RO . "kana_ro")
    ;; Mouse
;    (up . "up")
;    (down . "down")
;    (drag . "drag")
;    (click . "click")
;    (double . "double")
;    (triple . "triple")
    ))

(defvar scim-alt-modifier-alist
  '(
    (hiragana-katakana . romaji)
    (zenkaku-hankaku . kanji)
    (henkan . mode-switch)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key code table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar scim-keycode-alist
  '(
    ;; *** Function keys ***********************************************
    (backspace . ?\xff08)
    (tab . ?\xff09)
    (linefeed . ?\xff0a)
    (clear . ?\xff0b)
    (return . ?\xff0d)
    (pause . ?\xff13)
;    (scroll-lock . ?\xff14)
;    (sys-req . ?\xff15)
    (escape . ?\xff1b)
    (delete . ?\xffff)
    ;; *** International & multi-key character composition *************
;    (multi-key . ?\xff20)
;    (codeinput . ?\xff37)
;    (singlecandidate . ?\xff3c)
;    (multiplecandidate . ?\xff3d)
;    (previouscandidate . ?\xff3e)
    ;; Japanese keyboard support ***************************************
    (kanji . ?\xff21)
    (muhenkan . ?\xff22)
;    (henkan-mode . ?\xff23)
    (henkan . ?\xff23)
    (romaji . ?\xff24)
    (hiragana . ?\xff25)
    (katakana . ?\xff26)
    (hiragana-katakana . ?\xff27)
    (zenkaku . ?\xff28)
    (hankaku . ?\xff29)
    (zenkaku-hankaku . ?\xff2a)
    (touroku . ?\xff2b)
    (massyo . ?\xff2c)
    (kana-lock . ?\xff2d)
    (kana-shift . ?\xff2e)
    (eisu-shift . ?\xff2f)
    (eisu-toggle . ?\xff30)
;    (kanji-bangou . ?\xff37)
;    (zen-koho . ?\xff3d)
;    (mae-koho . ?\xff3e)
    ;; *** Cursor control & motion *************************************
    (home . ?\xff50)
    (left . ?\xff51)
    (up . ?\xff52)
    (right . ?\xff53)
    (down . ?\xff54)
    (prior . ?\xff55)
;    (page-up . ?\xff55)
    (next . ?\xff56)
;    (page-down . ?\xff56)
    (end . ?\xff57)
    (begin . ?\xff58)
    ;; *** Misc Functions **********************************************
    (select . ?\xff60)
    (print . ?\xff61)
    (execute . ?\xff62)
    (insert . ?\xff63)
    (undo . ?\xff65)
    (redo . ?\xff66)
    (menu . ?\xff67)
    (find . ?\xff68)
    (cancel . ?\xff69)
    (help . ?\xff6a)
    (break . ?\xff6b)
    (mode-switch . ?\xff7e) ; This key cannot be recognized to Emacs.
;    (num-lock . ?\xff7f)
    ;; *** Keypad ******************************************************
    (kp-space . ?\xff80)
    (kp-tab . ?\xff89)
    (kp-enter . ?\xff8d)
    (kp-f1 . ?\xff91)
    (kp-f2 . ?\xff92)
    (kp-f3 . ?\xff93)
    (kp-f4 . ?\xff94)
    (kp-home . ?\xff95)
    (kp-left . ?\xff96)
    (kp-up . ?\xff97)
    (kp-right . ?\xff98)
    (kp-down . ?\xff99)
    (kp-prior . ?\xff9a)
;    (kp-page-up . ?\xff9a)
    (kp-next . ?\xff9b)
;    (kp-page-down . ?\xff9b)
    (kp-end . ?\xff9c)
    (kp-begin . ?\xff9d)
    (kp-insert . ?\xff9e)
    (kp-delete . ?\xff9f)
    (kp-equal . ?\xffbd)
    (kp-multiply . ?\xffaa)
    (kp-add . ?\xffab)
    (kp-separator . ?\xffac)
    (kp-subtract . ?\xffad)
    (kp-decimal . ?\xffae)
    (kp-divide . ?\xffaf)
    (kp-0 . ?\xffb0)
    (kp-1 . ?\xffb1)
    (kp-2 . ?\xffb2)
    (kp-3 . ?\xffb3)
    (kp-4 . ?\xffb4)
    (kp-5 . ?\xffb5)
    (kp-6 . ?\xffb6)
    (kp-7 . ?\xffb7)
    (kp-8 . ?\xffb8)
    (kp-9 . ?\xffb9)
    ;; *** Auxilliary functions ****************************************
    (f1 . ?\xffbe)
    (f2 . ?\xffbf)
    (f3 . ?\xffc0)
    (f4 . ?\xffc1)
    (f5 . ?\xffc2)
    (f6 . ?\xffc3)
    (f7 . ?\xffc4)
    (f8 . ?\xffc5)
    (f9 . ?\xffc6)
    (f10 . ?\xffc7)
    (f11 . ?\xffc8)
    (f12 . ?\xffc9)
    (f13 . ?\xffca)
    (f14 . ?\xffcb)
    (f15 . ?\xffcc)
    (f16 . ?\xffcd)
    (f17 . ?\xffce)
    (f18 . ?\xffcf)
    (f19 . ?\xffd0)
    (f20 . ?\xffd1)
    (f21 . ?\xffd2)
    (f22 . ?\xffd3)
    (f23 . ?\xffd4)
    (f24 . ?\xffd5)
    (f25 . ?\xffd6)
    (f26 . ?\xffd7)
    (f27 . ?\xffd8)
    (f28 . ?\xffd9)
    (f29 . ?\xffda)
    (f30 . ?\xffdb)
    (f31 . ?\xffdc)
    (f32 . ?\xffdd)
    (f33 . ?\xffde)
    (f34 . ?\xffdf)
    (f35 . ?\xffe0)
    ;; *** Modifier keys ***********************************************
;    (shift-l . ?\xffe1)
;    (shift-r . ?\xffe2)
;    (control-l . ?\xffe3)
;    (control-r . ?\xffe4)
;    (caps-lock . ?\xffe5)
    (capslock . ?\xffe5)
;    (shift-lock . ?\xffe6)
;    (meta-l . ?\xffe7)
;    (meta-r . ?\xffe8)
;    (alt-l . ?\xffe9)
;    (alt-r . ?\xffea)
;    (super-l . ?\xffeb)
;    (super-r . ?\xffec)
;    (hyper-l . ?\xffed)
;    (hyper-r . ?\xffee)
    ;; *** ISO 9995 function and modifier keys *************************
;    (iso-lock . ?\xfe01)
;    (iso-level2-latch . ?\xfe02)
;    (iso-level3-shift . ?\xfe03)
;    (iso-level3-latch . ?\xfe04)
;    (iso-level3-lock . ?\xfe05)
;    (iso-group-shift . ?\xff7e)
;    (iso-group-latch . ?\xfe06)
;    (iso-group-lock . ?\xfe07)
;    (iso-next-group . ?\xfe08)
;    (iso-next-group-lock . ?\xfe09)
;    (iso-prev-group . ?\xfe0a)
;    (iso-prev-group-lock . ?\xfe0b)
;    (iso-first-group . ?\xfe0c)
;    (iso-first-group-lock . ?\xfe0d)
;    (iso-last-group . ?\xfe0e)
;    (iso-last-group-lock . ?\xfe0f)
;    (iso-left-tab . ?\xfe20)
    (iso-lefttab . ?\xfe20)
    (iso-move-line-up . ?\xfe21)
    (iso-move-line-down . ?\xfe22)
    (iso-partial-line-up . ?\xfe23)
    (iso-partial-line-down . ?\xfe24)
    (iso-partial-space-left . ?\xfe25)
    (iso-partial-space-right . ?\xfe26)
    (iso-set-margin-left . ?\xfe27)
    (iso-set-margin-right . ?\xfe28)
    (iso-release-margin-left . ?\xfe29)
    (iso-release-margin-right . ?\xfe2a)
    (iso-release-both-margins . ?\xfe2b)
    (iso-fast-cursor-left . ?\xfe2c)
    (iso-fast-cursor-right . ?\xfe2d)
    (iso-fast-cursor-up . ?\xfe2e)
    (iso-fast-cursor-down . ?\xfe2f)
    (iso-continuous-underline . ?\xfe30)
    (iso-discontinuous-underline . ?\xfe31)
    (iso-emphasize . ?\xfe32)
    (iso-center-object . ?\xfe33)
    (iso-enter . ?\xfe34)
    ;; *** Lispy accent keys *******************************************
    (dead-grave . ?\xfe50)
    (dead-acute . ?\xfe51)
    (dead-circumflex . ?\xfe52)
    (dead-tilde . ?\xfe53)
    (dead-macron . ?\xfe54)
    (dead-breve . ?\xfe55)
    (dead-abovedot . ?\xfe56)
    (dead-diaeresis . ?\xfe57)
    (dead-abovering . ?\xfe58)
    (dead-doubleacute . ?\xfe59)
    (dead-caron . ?\xfe5a)
    (dead-cedilla . ?\xfe5b)
    (dead-ogonek . ?\xfe5c)
    (dead-iota . ?\xfe5d)
    (dead-voiced-sound . ?\xfe5e)
    (dead-semivoiced-sound . ?\xfe5f)
    (dead-belowdot . ?\xfe60)
    (dead-hook . ?\xfe61)
    (dead-horn . ?\xfe62)
    ;; *** Katakana ****************************************************
    (overline . ?\x47e)
    (kana-fullstop . ?\x4a1)
    (kana-openingbracket . ?\x4a2)
    (kana-closingbracket . ?\x4a3)
    (kana-comma . ?\x4a4)
    (kana-conjunctive . ?\x4a5)
;    (kana-middledot . ?\x4a5)
    (kana-WO . ?\x4a6)
    (kana-a . ?\x4a7)
    (kana-i . ?\x4a8)
    (kana-u . ?\x4a9)
    (kana-e . ?\x4aa)
    (kana-o . ?\x4ab)
    (kana-ya . ?\x4ac)
    (kana-yu . ?\x4ad)
    (kana-yo . ?\x4ae)
    (kana-tsu . ?\x4af)
;    (kana-tu . ?\x4af)
    (prolongedsound . ?\x4b0)
    (kana-A . ?\x4b1)
    (kana-I . ?\x4b2)
    (kana-U . ?\x4b3)
    (kana-E . ?\x4b4)
    (kana-O . ?\x4b5)
    (kana-KA . ?\x4b6)
    (kana-KI . ?\x4b7)
    (kana-KU . ?\x4b8)
    (kana-KE . ?\x4b9)
    (kana-KO . ?\x4ba)
    (kana-SA . ?\x4bb)
    (kana-SHI . ?\x4bc)
    (kana-SU . ?\x4bd)
    (kana-SE . ?\x4be)
    (kana-SO . ?\x4bf)
    (kana-TA . ?\x4c0)
    (kana-CHI . ?\x4c1)
;    (kana-TI . ?\x4c1)
    (kana-TSU . ?\x4c2)
;    (kana-TU . ?\x4c2)
    (kana-TE . ?\x4c3)
    (kana-TO . ?\x4c4)
    (kana-NA . ?\x4c5)
    (kana-NI . ?\x4c6)
    (kana-NU . ?\x4c7)
    (kana-NE . ?\x4c8)
    (kana-NO . ?\x4c9)
    (kana-HA . ?\x4ca)
    (kana-HI . ?\x4cb)
    (kana-FU . ?\x4cc)
;    (kana-HU . ?\x4cc)
    (kana-HE . ?\x4cd)
    (kana-HO . ?\x4ce)
    (kana-MA . ?\x4cf)
    (kana-MI . ?\x4d0)
    (kana-MU . ?\x4d1)
    (kana-ME . ?\x4d2)
    (kana-MO . ?\x4d3)
    (kana-YA . ?\x4d4)
    (kana-YU . ?\x4d5)
    (kana-YO . ?\x4d6)
    (kana-RA . ?\x4d7)
    (kana-RI . ?\x4d8)
    (kana-RU . ?\x4d9)
    (kana-RE . ?\x4da)
    (kana-RO . ?\x4db)
    (kana-WA . ?\x4dc)
    (kana-N . ?\x4dd)
    (voicedsound . ?\x4de)
    (semivoicedsound . ?\x4df)
;    (kana-switch . ?\xFF7E)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode management
(defcustom scim-mode nil
  "Toggle scim-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `scim-mode'."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :version "22.1"
  :type 'boolean
  :group 'scim
  :require 'scim-bridge)

;; Manage key bindings
(defvar scim-mode-map nil)
(defvar scim-mode-preedit-map nil)
(defvar scim-mode-map-alist nil)
(defvar scim-mode-map-disabled nil)
(make-variable-buffer-local 'scim-mode-map-disabled)
(defvar scim-mode-map-prev-disabled nil)
(make-variable-buffer-local 'scim-mode-map-prev-disabled)
(defvar scim-kana-ro-prev-x-keysym nil)

;; Communication & buffer editing
(defvar scim-bridge-socket nil)
(defvar scim-tmp-buffer nil)
(defvar scim-last-command-event nil)
(defvar scim-last-command-buffer nil)
(defvar scim-buffer-undo-list nil)
(defvar scim-buffer-modified-p nil)
(defvar scim-frame-focus nil)
(defvar scim-focus-update-timer nil)
(defvar scim-focus-observation-infrequent nil)
(defvar scim-string-insertion-failed nil)
(defvar scim-config-last-modtime nil)

;; Preediting area
(defvar scim-imcontext-id nil)
(defvar scim-imcontext-status nil)
(defvar scim-preedit-buffer nil)
(defvar scim-preedit-point (make-marker))
(defvar scim-preedit-update nil)
(defvar scim-preedit-shown "")
(defvar scim-preedit-string "")
(defvar scim-preedit-prev-string "")
(defvar scim-preedit-curpos 0)
(defvar scim-preedit-attributes nil)
(defvar scim-preedit-default-attr nil)
(defvar scim-preedit-overlays nil)
(defvar scim-committed-string "")
(defvar scim-frame-extents '(0 0 0 0))
(defvar scim-adjust-window-x-offset 0)
(defvar scim-adjust-window-y-offset 0)
(defvar scim-surrounding-text-modified nil)
(defvar scim-cursor-type-saved nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
(defun scim-escape-string (str)
  (let* ((tmp (append str nil))
	 cur
	 (next tmp))
    (while (setq cur (memq ?\\ next))
      (setq next (cdr cur))
      (setcdr cur (cons ?\\ next)))
    (setq next tmp)
    (while (setq cur (memq ?\n next))
      (setq next (cdr cur))
      (setcar cur ?\\)
      (setcdr cur (cons ?n next)))
    (setq next tmp)
    (while (setq cur (memq ?\  next))
      (setq next (cdr cur))
      (setcar cur ?\\)
      (setcdr cur (cons ?s next)))
    (concat tmp)))

(defun scim-construct-command (list)
  (mapconcat (lambda (f) (scim-escape-string f)) list " "))

(defun scim-unescape-string (str)
  (let* ((tmp (append str nil))
	 (cur tmp)
	 next)
    (while (setq cur (memq ?\\ cur))
      (setq next (cdr cur))
      (setcar cur (car (or (rassq (car next)
				  '((?\  . ?s)
				    (?\n . ?n)
;				    (?\\ . ?\\)
				    ))
			   next)))
      (setq cur (setcdr cur (cdr next))))
    (concat (delq nil tmp))))

(defun scim-split-commands (commands)
  (mapcar (lambda (line)
	    (mapcar 'scim-unescape-string (split-string line " ")))
	  (split-string (substring commands 0 -1) "\n")))

(defun scim-decode-event (event)
  ;; Convert Emacs event to scim-bridge command
  (let ((modifiers (mapcar (lambda (mod)
			     (cdr (assq mod scim-modifier-alist)))
			   (event-modifiers event)))
	(key-code (event-basic-type event)))
    (if (numberp key-code)
	(if (and (member "shift" modifiers)
		 (>= key-code ?a)
		 (<= key-code ?z))
	    (setq key-code (- key-code 32)))
      (if (member "alt" modifiers)
	  (setq key-code
		(or (cdr (assq key-code scim-alt-modifier-alist))
		    key-code)))
      (if (and scim-use-kana-ro-key
	       scim-kana-ro-key-symbol
	       (eq key-code scim-kana-ro-key-symbol))
	  (setq key-code ?\\
		modifiers (cons "kana_ro" modifiers)))
      (setq key-code (or (cdr (assq key-code scim-keycode-alist))
			 key-code)))
    (cons key-code modifiers)))

(defun scim-encode-event (key-code modifiers)
  ;; Convert scim-bridge command to Emacs event
  (setq key-code (string-to-number key-code))
  (let* ((bas (or (car (rassq key-code scim-keycode-alist))
		  (if (< key-code 128) key-code)))
	 (mods nil))
    (if (member "alt" modifiers)
	(setq bas (or (car (rassq bas scim-alt-modifier-alist))
		      bas)))
    (while modifiers
      (let ((m (car (rassoc (car modifiers) scim-modifier-alist))))
	(if m (cond ((eq m 'caps-lock)
		     nil) ; Ignore `caps_lock' modifier
		    ((eq m 'num-lock)
		     nil) ; Ignore `num_lock' modifier
		    ((eq m 'kana-RO)
		     (if (and scim-use-kana-ro-key
			      scim-kana-ro-key-symbol
			      (eq bas ?\\)
			      (not scim-mode-map-prev-disabled))
			 (setq bas scim-kana-ro-key-symbol)))
		    (t (add-to-list 'mods m)))))
      (setq modifiers (cdr modifiers)))
    (if bas (event-convert-list (nconc mods (list bas))))))

(defconst scim-hex-table
  (vconcat (make-vector ?0 0)
	   [0 1 2 3 4 5 6 7 8 9]
	   (make-vector (- ?A ?9 1) 0)
	   [10 11 12 13 14 15]
	   (make-vector (- ?a ?F 1) 0)
	   [10 11 12 13 14 15]
	   (make-vector (- 127 ?f) 0)))

(defun scim-hexstr-to-number (string)
  (let ((len (length string))
	(ret 0)	(pos 0))
    (while (< (setq ret (+ (* ret 16)
			   (aref scim-hex-table (aref string pos)))
		    pos (1+ pos))
	      len))
    ret))

;(defun scim-hexstr-to-number (string &optional length)
;  (let ((pos (1- (or length (length string)))))
;    (+ (if (= pos 0)
;	   0
;	 (* 16 (scim-hexstr-to-number string pos)))
;       (aref scim-hex-table (aref string pos)))))

(defun scim-twos-complement (string)
  (let ((num (string-to-number string)))
    (if (< num 2147483648.0)
	(round num)
      (round (- num 4294967296.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Localization
(defun scim-set-group-doc (group string)
  "Change the documentation string of GROUP into STRING.
If STRING is empty or nil, the documentation string is left original."
  (if (> (length string) 0)
      (put group 'group-documentation string)))

(defun scim-set-variable-doc (variable string &optional custom-type)
  "Change the documentation string of VARIABLE into STRING.
If STRING is empty or nil, the documentation string is left original.
If CUSTOM-TYPE is non-nil, it is set to the `custom-type' property of
VARIABLE, which corresponds to the :type keyword in `defcustom'."
  (if (> (length string) 0)
      (put variable 'variable-documentation string))
  (if custom-type
      (put variable 'custom-type custom-type)))

(defun scim-set-face-doc (face string)
  "Change the documentation string of FACE into STRING.
If STRING is empty or nil, the documentation string is left original."
  (if (> (length string) 0)
      (put face 'face-documentation string)))

(defun scim-set-function-doc (function string)
  "Change the documentation string of FUNCTION into STRING.
If STRING is empty or nil, the documentation string is left original."
  (if (> (length string) 0)
      (let ((func (symbol-function function)))
	(if (byte-code-function-p func)
	    (let ((new-func (append func nil)))
	      (setcar (nthcdr 4 new-func) string)
	      (fset function (apply 'make-byte-code new-func)))
	  (setcar (nthcdr 2 func) string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages & Log
(defmacro scim-message (format-string &rest args)
  `(if (not scim-debug)
       (message (concat "SCIM: " ,format-string) ,@args)
     (let ((log-str (format ,format-string ,@args)))
       (save-excursion
	 (set-buffer (get-buffer-create scim-log-buffer))
	 (goto-char (point-max))
	 (insert (concat (format log-str) "\n"))))))

(defmacro scim-show-undo-list (format-string &rest args)
  `(progn
     (scim-message ,format-string ,@args)
     (if (not (listp buffer-undo-list))
	 (scim-message "undo list (disabled): %s" buffer-undo-list)
       (scim-message " top: %s" (car buffer-undo-list))
       (scim-message " 2nd: %s" (car (cdr buffer-undo-list)))
       (scim-message " 3rd: %s" (car (cdr (cdr buffer-undo-list))))
       (scim-message " 4th: %s" (car (cdr (cdr (cdr buffer-undo-list))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control buffer-undo-list
(defun scim-disable-undo ()
  (unless scim-preedit-buffer
    (if (eq major-mode 'erc-mode) (buffer-disable-undo))
    (setq scim-buffer-undo-list buffer-undo-list
	  scim-buffer-modified-p (buffer-modified-p))
    (buffer-disable-undo)
    (setq scim-preedit-buffer (current-buffer))
;#    (if scim-debug (scim-message "disable undo list: %s" scim-preedit-buffer))
    ))

(defun scim-enable-undo ()
  (when scim-preedit-buffer
    (with-current-buffer scim-preedit-buffer
      (buffer-enable-undo)
      (unless (eq major-mode 'erc-mode)
	(setq buffer-undo-list scim-buffer-undo-list))
      (set-buffer-modified-p scim-buffer-modified-p))
;#    (if scim-debug (scim-message "enable undo list: %s" scim-preedit-buffer))
    (setq scim-preedit-buffer nil)))

(defun scim-insert-and-modify-undo-list (str)
  (let* ((prev-list (if (car-safe buffer-undo-list)
			buffer-undo-list
		      (cdr-safe buffer-undo-list)))
	 (prev (car-safe prev-list))
	 (consp-prev (and (consp prev)
			  (integerp (car prev))
			  (integerp (cdr prev))))
	 (consecutivep (and consp-prev
			   (= (cdr prev) (point))
			   (not (= (preceding-char) ?\n))
			   (<= (+ (cdr prev) (length str))
			       (+ (car prev) 20))))) ; max 20 chars
;#    (if scim-debug (scim-show-undo-list "previous undo list"))
    (when (and consp-prev
	       (integerp (car (cdr prev-list))))
      (setcdr prev-list (cdr (cdr prev-list)))
;#      (if scim-debug (scim-show-undo-list "get rid of point setting entry"))
      )
    (insert str)
;#    (if scim-debug (scim-show-undo-list "insert string: %S" str))
    (when (integerp (car (cdr-safe buffer-undo-list)))
      (setcdr buffer-undo-list (cdr (cdr buffer-undo-list)))
;#      (if scim-debug (scim-show-undo-list "get rid of point setting entry"))
      )
    (if (and consecutivep
	     (eq (cdr (cdr buffer-undo-list)) prev-list))
	(progn
	  (setcar (car buffer-undo-list) (car (car prev-list)))
	  (setcdr buffer-undo-list (cdr prev-list))
;#	  (if scim-debug (scim-show-undo-list "unify consecutive insertion entries"))
	  )
      (when (and (> (length str) 20)
		 (listp buffer-undo-list)) ; Undo enabled?
	(let ((beg (car (car buffer-undo-list)))
	      (end (cdr (car buffer-undo-list)))
	      (new-list (cdr buffer-undo-list)))
	  (while (> (- end beg) 20)
	    (setq new-list (cons nil (cons (cons beg (+ beg 20)) new-list))
		  beg (+ beg 20)))
	  (setq buffer-undo-list (cons (cons beg end) new-list))
;#	  (if scim-debug (scim-show-undo-list "divide long insertion entry"))
	  )))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control keyboard
(defun scim-enable-preedit-keymap ()
  (setq minor-mode-map-alist
	(cons (cons 'scim-mode scim-mode-preedit-map)
	      (delq (assq 'scim-mode minor-mode-map-alist)
		    minor-mode-map-alist))
	scim-mode-map-alist
	(list (assoc 'scim-mode minor-mode-map-alist))))

(defun scim-disable-preedit-keymap ()
  (setcdr (assq 'scim-mode minor-mode-map-alist)
	  scim-mode-map)
  (setq scim-mode-map-alist nil))

(defun scim-enable-kana-ro-key (&optional keysym)
  (unless keysym (setq keysym scim-kana-ro-x-keysym))
;#  (if scim-debug (scim-message "enable Kana-RO key: %s" keysym))
  (shell-command-to-string
   (concat "xmodmap -pke | sed -n 's/= backslash underscore/= "
	   keysym " underscore/p' | xmodmap -"))
  (setq scim-kana-ro-prev-x-keysym keysym))

(defun scim-disable-kana-ro-key (&optional keysym)
  (unless keysym (setq keysym scim-kana-ro-prev-x-keysym))
  (when keysym
;#    (if scim-debug (scim-message "disable Kana-RO key: %s" keysym))
    (shell-command-to-string
     (concat "xmodmap -pke | sed -n 's/= " keysym
	     " underscore/= backslash underscore/p' | xmodmap -"))
    (setq scim-kana-ro-prev-x-keysym nil)))

;(defun scim-get-keyboard-layout ()
;  (let ((kbd (shell-command-to-string
;	      "xprop -root _XKB_RULES_NAMES | sed -n 's/.* = \"[^\"]*\", *\"\\([^\"]*\\)\".*/\\1/p'")))
;    (unless (string= kbd "") (substring kbd 0 -1))))

(defun scim-update-kana-ro-key (enable)
  (if (and window-system
	   (not (string=
		 (shell-command-to-string
		  "xprop -root _XKB_RULES_NAMES | grep '\"jp106\"'")
		 "")))
      (if (and enable scim-frame-focus)
	  (let ((cycle (if scim-focus-observation-infrequent
			   scim-focus-update-interval-long
			 scim-focus-update-interval)))
	    (run-at-time (+ cycle 0.1) nil 'scim-enable-kana-ro-key))
	(scim-disable-kana-ro-key))))

(defun scim-enable-keymap-internal ()
  (if (and scim-use-kana-ro-key scim-frame-focus)
      (scim-update-kana-ro-key t))
  (setq minor-mode-overriding-map-alist
	(delete '(scim-mode) minor-mode-overriding-map-alist))
  (setq scim-mode-map-prev-disabled nil))

(defun scim-disable-keymap-internal ()
  (if (and scim-use-kana-ro-key scim-frame-focus)
      (scim-update-kana-ro-key nil))
  (add-to-list 'minor-mode-overriding-map-alist '(scim-mode))
  (setq scim-mode-map-prev-disabled t))

(defun scim-enable-keymap ()
  (interactive)
  (scim-enable-keymap-internal)
  (setq scim-mode-map-disabled nil))

(defun scim-disable-keymap ()
  (interactive)
  (scim-disable-keymap-internal)
  (setq scim-mode-map-disabled t))

(defun scim-make-keymap-internal (map keys &rest ranges)
  (while ranges
    (let ((i (caar ranges))
	  (max (cdar ranges)))
      (while (<= i max)
	(define-key map (char-to-string i) 'scim-handle-event)
	(setq i (1+ i))))
    (setq ranges (cdr ranges)))
  (while keys
    (let* ((key (reverse (car keys)))
	   (bas (car key))
	   (mods (cdr key)))
      (if (stringp bas)
	  (setq bas (string-to-char bas)))
      (when (memq 'alt mods)
	(unless scim-meta-key-exists
	  (setq mods (cons 'meta (delq 'alt mods))))
	(setq bas (or (car (rassq bas scim-alt-modifier-alist))
		      bas)))
      (define-key map (vector (nconc mods (list bas))) 'scim-handle-event)
      (setq keys (cdr keys))))
  map)

(defun scim-combine-modifiers (base modifiers)
  (if modifiers
      (apply 'nconc
	     (mapcar
	      (lambda (k) (list (cons (car modifiers) k) k))
	      (scim-combine-modifiers base (cdr modifiers))))
    (list (list base))))

(defun scim-make-kana-ro-map ()
  (scim-make-keymap-internal (make-keymap)
			     (if scim-kana-ro-key-symbol
				 (scim-combine-modifiers
				  scim-kana-ro-key-symbol
				  '(meta control hyper super alt)))))

(defun scim-make-common-map ()
  (scim-make-keymap-internal (scim-make-kana-ro-map)
			     scim-common-function-key-list
			     '(32 . 126)))

(defun scim-make-preedit-map ()
  (scim-make-keymap-internal (scim-make-common-map)
			     scim-preedit-function-key-list
			     '(0 . 26) '(28 . 31)))

(defun scim-update-key-bindings (&optional symbol)
  (when (or (null symbol)
	    (eq symbol 'scim-use-kana-ro-key)
	    (and (eq symbol 'scim-kana-ro-x-keysym)
		 scim-use-kana-ro-key))
    (unless (eq symbol 'scim-use-kana-ro-key)
      (scim-update-kana-ro-key nil))
    (scim-update-kana-ro-key (and scim-use-kana-ro-key
				  (not scim-mode-map-prev-disabled))))
  (when (or (null symbol)
	    (memq symbol '(scim-common-function-key-list
			   scim-preedit-function-key-list
			   scim-kana-ro-key-symbol)))
    (unless (eq symbol 'scim-preedit-function-key-list)
;#      (if scim-debug (scim-message "update scim-mode-map"))
      (setq scim-mode-map (scim-make-common-map)))
;#    (if scim-debug (scim-message "update scim-mode-preedit-map"))
    (setq scim-mode-preedit-map (scim-make-preedit-map))
;#;    (if scim-debug (scim-message "scim-mode-preedit-map: %s" scim-mode-preedit-map))
    (if scim-preedit-buffer
	(scim-enable-preedit-keymap)
      (scim-disable-preedit-keymap))))

(defun scim-define-key (symbol keys handle)
  ;; If keys is given as an array, it doesn't indicate key sequence,
  ;; but multiple definitions of single keystroke.
  (let ((keys-list (if (arrayp keys)
		       (listify-key-sequence keys)
		     (list keys))))
    (while keys-list
      (let ((key (car keys-list)))
	(if (listp key)
	    (let* ((n (1- (length key)))
		   (bas (nth n key)))
	      ;; If the key event is specified by a list and the last
	      ;; element is given as a string, the code number for the first
	      ;; character of the string is used for an event basic type.
	      (when (stringp bas)
		(setq key (copy-sequence key))
		(setcar (nthcdr n key) (string-to-char bas)))
	      (setq key (event-convert-list key))))
	;; In Emacs 22, the function `event-modifiers' cannot return the
	;; correct value until the symbol is parsed.
	(key-binding (vector key))
	;; It is necessary to call a function `event-basic-type' after
	;; `event-modifiers' because `event-basic-type' uses the symbol
	;; property `event-symbol-elements' added by `event-modifiers'
	;; when event is given as a symbol.
	(let ((modifiers (event-modifiers key))
	      (key-code (event-basic-type key)))
	  (if (integerp key-code)
	      (setq key-code (char-to-string key-code)
		    modifiers (reverse modifiers)))
	  (setq key (append modifiers (list key-code))))
	(if handle
	    (add-to-list symbol key)
	  (set symbol (delete key (symbol-value symbol)))))
      (setq keys-list (cdr keys-list)))
    (symbol-value symbol))) ; Return value

(defun scim-define-common-key (key handle)
  "Specify which key events SCIM anytime takes over. If HANDLE
is non-nil, SCIM handles the key events given by KEY. When KEY is
given as an array, it doesn't indicate key sequence, but multiple
definitions of single keystroke.
 It is necessary to call a function `scim-update-key-bindings' or
restart scim-mode so that this settings may become effective."
  (scim-define-key 'scim-common-function-key-list key handle))

(defun scim-define-preedit-key (key handle)
  "Specify which key events SCIM takes over when preediting. If
HANDLE is non-nil, SCIM handles the key events given by KEY. When
KEY is given as an array, it doesn't indicate key sequence, but
multiple definitions of single keystroke.
 It is necessary to call a function `scim-update-key-bindings' or
restart scim-mode so that this settings may become effective."
  (scim-define-key 'scim-preedit-function-key-list key handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control display
(defun scim-update-mode-line ()
  (force-mode-line-update)
  scim-mode) ; Return value

(defun scim-set-cursor-color ()
  (if scim-cursor-color
      (let ((color (cond ((stringp scim-cursor-color)
			  (if scim-imcontext-status
			      scim-cursor-color
			    (frame-parameter nil 'foreground-color)))
			 ((consp scim-cursor-color)
			  (if scim-imcontext-status
			      (car scim-cursor-color)
			    (cdr scim-cursor-color))))))
;#	(if scim-debug (scim-message "set cursor color: %s" color))
	(set-cursor-color color))))

(defun scim-reset-imcontext-statuses ()
  "Reset entirely the variables which keep the IMContext statuses
of each buffer in order to correct impropriety of the cursor color.
This function might be invoked just after using SCIM GUI Setup Utility."
  (if scim-mode-local
      (let ((buffers (buffer-list)))
	(save-current-buffer
	  (while buffers
	    (set-buffer (car buffers))
	    (if (local-variable-p 'scim-imcontext-status)
		(setq scim-imcontext-status nil))
	    (setq buffers (cdr buffers)))))
    (setq-default scim-imcontext-status nil))
  (scim-set-cursor-color))

;(defun scim-title-bar-height ()
;  "Return the pixel hight of title bar of selected frame."
;  (let* ((window-id (frame-parameter nil 'outer-window-id))
;	 (line (shell-command-to-string
;		(concat "xprop -id " window-id " _NET_FRAME_EXTENTS"))))
;    (string-to-number (substring line
;				 (string-match "[0-9]+,[ 0-9]+$" line)
;				 (string-match ",[ 0-9]+$" line)))))

(defun scim-get-frame-extents ()
  "Return the pixel width of frame edges as (left right top bottom).
Here, `top' also indicates the hight of frame title bar."
;#  (if scim-debug (scim-message "get frame extents"))
  (let* ((window-id (frame-parameter nil 'outer-window-id))
	 (line (shell-command-to-string
		(concat "xprop -id " window-id " _NET_FRAME_EXTENTS"))))
    (mapcar 'string-to-number
	    (split-string (substring line (string-match "[0-9]" line) -1)
			  ","))))

(defun scim-save-frame-extents ()
  (setq scim-frame-extents (if (frame-parameter nil 'parent-id)
			       ;; no display effect
			       (scim-get-frame-extents)
			     ;; with Compiz fusion effects
			     '(0 0 0 0))))

(defun scim-frame-header-height ()
  "Return the total of pixel height of menu-bar and tool-bar.
The value that this function returns is not so accurate."
  (- (frame-pixel-height)
     (* (frame-height) (frame-char-height))
     scim-adjust-window-y-offset))

(defun scim-real-frame-header-height ()
  "Return the total of pixel height of menu-bar and tool-bar.
The value that this function returns is very exact, but this function
is quite slower than `scim-frame-header-height'."
;#  (if scim-debug (scim-message "get frame header height"))
  (let* ((window-id (frame-parameter nil 'window-id))
	 (line (shell-command-to-string
		(concat "xwininfo -id " window-id
			" | grep 'Relative upper-left Y:'"))))
    (string-to-number (substring line
				 (string-match "[0-9]+$" line)))))

(defun scim-set-window-y-offset ()
  (setq scim-adjust-window-y-offset
	(or (if scim-adjust-window-y-position
		(let* ((scim-adjust-window-y-offset 0)
		       (gap (- (scim-frame-header-height)
			       (scim-real-frame-header-height))))
		  (if (< gap (frame-char-height)) gap)))
	    0)))

(defun scim-compute-pixel-position ()
  "Return the screen pxel position of point as (X . Y).
Its values show the coordinates of lower left corner of the character."
;#;  (if scim-debug (scim-message "current-buffer: %s  pos: %d" (current-buffer) (point)))
  (let* ((x-y (or (posn-x-y (posn-at-point))
		  '(0 . 0))))
;#;    (if scim-debug (scim-message "(x . y): %s" x-y))
    (cons (+ (frame-parameter nil 'left)
	     (car scim-frame-extents)
	     (car (window-inside-pixel-edges))
	     (car x-y))
	  (+ (frame-parameter nil 'top)
	     (nth 2 scim-frame-extents)
	     (scim-frame-header-height)
	     (car (cdr (window-pixel-edges)))
	     (cdr x-y)
	     (frame-char-height)))))

(defun scim-get-gnome-font-size ()
  "Return the pixel size of application font in the GNOME desktop
environment. It is necessary to set the screen resolution (dots per
inch) and to be able to use a shell command `gconftool-2'. If not,
this function returns zero."
  (if (string= (shell-command-to-string "which gconftool-2") "")
      0
    (let ((font (shell-command-to-string
		 "gconftool-2 -g /desktop/gnome/interface/font_name"))
	  (dpi (shell-command-to-string
		"gconftool-2 -g /desktop/gnome/font_rendering/dpi")))
      (/ (* (string-to-number
	     (substring font (string-match "[0-9]+$" font) -1))
	    (string-to-number dpi))
	 72))))

(defun scim-set-window-x-offset ()
  (setq scim-adjust-window-x-offset
	(cond ((eq scim-adjust-window-x-position 'gnome)
	       (+ (scim-get-gnome-font-size) 4))
	      ((integerp scim-adjust-window-x-position)
	       scim-adjust-window-x-position)
	      (t 0))))

(defun scim-get-active-window-id ()
  "Return the number of the window-system window which is foreground,
i.e. input focus is in this window."
  (if scim-focus-observation-infrequent
      (string-to-number
       (shell-command-to-string
	"xwininfo -root -children -int | grep -v '\"scim-panel-gtk\"\\|\"tomoe\"\\|\"nagisa\"\\|(has no name)' | grep ' children:$' -A 1 | tail -n 1"))
    (let* ((line (shell-command-to-string "xprop -root _NET_ACTIVE_WINDOW"))
	   (pos (string-match "[0-9a-fA-F]+$" line)))
      (if pos
	  (scim-hexstr-to-number (substring line pos -1))
	(scim-message "Active window ID cannot be obtained by `xprop'. Instead, `xwininfo' is used.")
	(setq scim-focus-observation-infrequent t)
	(scim-get-active-window-id)))))

(defun scim-config-file-timestamp ()
  (let ((time (nth 5 (file-attributes scim-config-file))))
    (+ (* (car time) 65536) (cadr time))))

(defun scim-check-frame-focus (&optional focus-in)
  (let ((window-id (string-to-number
		    (frame-parameter nil 'outer-window-id)))
	(active-win (scim-get-active-window-id))
	(stat-toggled (or (not scim-frame-focus) focus-in)))
    (when (eq (eq window-id active-win) stat-toggled)
      (if stat-toggled
	  (when (and (not scim-frame-focus)
		     scim-config-last-modtime
		     (> (scim-config-file-timestamp) scim-config-last-modtime))
;#	    (if scim-debug (scim-message "SCIM's settings changed"))
	    (scim-reset-imcontext-statuses))
	(setq scim-config-last-modtime (scim-config-file-timestamp)))
      (setq scim-frame-focus stat-toggled)
;#      (if scim-debug (scim-message "change focus"))
      (if scim-use-kana-ro-key
	  (scim-update-kana-ro-key (not scim-mode-map-prev-disabled)))
      (when (stringp scim-imcontext-id)
	(scim-change-focus scim-frame-focus)
	(when scim-frame-focus
	  (scim-set-cursor-color)
	  (scim-save-frame-extents)
	  (scim-set-window-x-offset)
	  (scim-set-window-y-offset))))))

(defun scim-start-focus-observation ()
  (let ((cycle (if scim-focus-observation-infrequent
		   scim-focus-update-interval-long
		 scim-focus-update-interval)))
    (setq scim-focus-update-timer
	  (run-at-time cycle cycle 'scim-check-frame-focus))))

(defun scim-cancel-focus-update-timer ()
  (if scim-focus-update-timer
      (cancel-timer scim-focus-update-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulate preedit buffer
(defun scim-check-rgb-color (string)
  (and (eq (length string) 7)
       (string-match "^#[0-9A-Fa-f]+$" string)))

(defun scim-select-face-by-attr (attr)
  (cdr (assoc attr
	      '(("underline" . scim-preedit-underline-face)
		("highlight" . scim-preedit-highlight-face)
		("reverse" . scim-preedit-reverse-face)))))

(defun scim-remove-preedit ()
  (remove-hook 'before-change-functions 'scim-before-change-function t)
  (unless (string= scim-preedit-prev-string "")
    (let ((pos scim-preedit-point)
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t))
      (delete-region pos (+ pos (length scim-preedit-prev-string)))
      (goto-char pos)))
  (mapc 'delete-overlay scim-preedit-overlays)
  (if scim-cursor-type-saved
      (set-frame-parameter nil 'cursor-type scim-cursor-type-saved))
  (setq scim-preedit-prev-string ""
	scim-preedit-overlays nil
	scim-cursor-type-saved nil)
  (set-marker scim-preedit-point nil)
  (scim-enable-undo)
  (scim-disable-preedit-keymap))

(defun scim-cleanup-preedit ()
  (scim-remove-preedit)
;#  (if scim-debug (scim-message "cleanup preedit"))
  (setq scim-preedit-update nil
	scim-preedit-shown ""
	scim-preedit-string ""
	scim-preedit-curpos 0
	scim-preedit-attributes nil
	scim-committed-string ""))

(defun scim-redraw-preedit ()
  (setq scim-preedit-update nil)
  (let ((str scim-preedit-string)
	(attrs scim-preedit-attributes))
    (if (or (string= scim-preedit-shown "FALSE")
	    (string= str ""))
	;; IMContext is empty or invisible
	(scim-cleanup-preedit)
      ;; IMContext contains preedit string
      (if scim-preedit-buffer
	  (scim-remove-preedit)
	(scim-set-window-y-offset)
	(unless scim-surrounding-text-modified
	  (setq scim-preedit-default-attr nil)))
      ;; Put String
      (scim-enable-preedit-keymap)
      (scim-disable-undo)
      (set-marker scim-preedit-point (point))
;#;      (if scim-debug (scim-message "current cursor position: %d" scim-preedit-point))
      (condition-case err
	  (insert str)
	(text-read-only
	 (scim-message "Failed to insert preediting text %s" err)
	 (scim-cleanup-preedit)
	 (scim-reset-imcontext)
	 (setq str ""
	       scim-string-insertion-failed t)))
      (unless (string= str "")
	(setq scim-preedit-prev-string str)
	;; Set attributes
;#;	(if scim-debug (scim-message "attributes: %s" attrs))
	(let* ((max (length str))
	       (ol (make-overlay scim-preedit-point
				 (+ scim-preedit-point max)))
	       (flat-attr nil))
	  (overlay-put ol 'face 'scim-preedit-default-face)
	  (overlay-put ol 'priority 0)
	  (setq scim-preedit-overlays (list ol))
	  (while attrs
	    (let* ((beg (string-to-number (car attrs)))
		   (end (string-to-number
			 (car (setq attrs (cdr attrs)))))
		   (type (car (setq attrs (cdr attrs))))
		   (value (car (setq attrs (cdr attrs))))
		   fc pr)
;#;	      (if scim-debug (scim-message "beg: %d  end: %d  type: %s  val: %s" begin end type value))
	      (setq attrs (cdr attrs))
	      (if (cond ((and (string= type "foreground")
			      (scim-check-rgb-color value))
			 (setq fc (list :foreground value)
			       pr 50))
			((and (string= type "background")
			      (scim-check-rgb-color value))
			 (setq fc (list :background value)
			       pr 50))
			((and (string= type "decoreate")
			      (setq fc (scim-select-face-by-attr value)))
			 (setq pr 100)))
		  (let ((ol (make-overlay (+ scim-preedit-point beg)
					  (+ scim-preedit-point end))))
		    (overlay-put ol 'face fc)
		    (overlay-put ol 'priority pr)
		    (setq scim-preedit-overlays
			  (cons ol scim-preedit-overlays))
		    (setq flat-attr (if (and (listp flat-attr)
					     (eq beg 0) (eq end max))
					(cons fc flat-attr)
				      t)))
		(scim-message "Unable to set attribute \"%s %s\"." type value))))
	  (add-hook 'before-change-functions 'scim-before-change-function nil t)
	  (setq flat-attr (or flat-attr
			      'none)
		scim-preedit-default-attr (or scim-preedit-default-attr
					      flat-attr))
;#	  (if scim-debug (scim-message "default attr: %s" scim-preedit-default-attr))
;#	  (if scim-debug (scim-message "current attr: %s" flat-attr))
	  (if (or (eq flat-attr t)
		  (not (equal flat-attr scim-preedit-default-attr)))
	      ;; When conversion candidate is shown
	      (progn
		(when scim-cursor-type-for-candidate
		  (setq scim-cursor-type-saved (frame-parameter nil 'cursor-type))
		  (set-frame-parameter nil 'cursor-type scim-cursor-type-for-candidate))
		(if scim-put-cursor-on-candidate
		    (goto-char (+ scim-preedit-point scim-preedit-curpos)))
		(scim-set-cursor-location))
	    ;; When the string is preedited or prediction window is drawn
	    (goto-char (+ scim-preedit-point scim-preedit-curpos))
	    (if (car scim-prediction-window-position)
		(setq scim-preedit-curpos 0))
	    (if (cdr scim-prediction-window-position)
		(scim-set-cursor-location)
	      (let ((scim-adjust-window-x-offset 0))
		(scim-set-cursor-location))))
	  )))))

(defun scim-do-update-preedit ()
  (when scim-preedit-update
;#    (if scim-debug (scim-message "preedit-update  win-buf: %s  cur-buf: %s  cmd-buf: %s  str: \"%s\"" (window-buffer) (current-buffer) scim-preedit-buffer scim-preedit-string))
    (scim-redraw-preedit)
    (scim-preedit-updated)))

(defun scim-abort-preedit ()
  (when (eq (current-buffer) scim-preedit-buffer)
    (let ((str scim-preedit-prev-string)
	  (pos (copy-marker scim-preedit-point)))
      (save-excursion
	(scim-cleanup-preedit)
	(unless scim-clear-preedit-when-unexpected-event
	  (goto-char pos)
	  (insert-before-markers str)))
      (scim-reset-imcontext))))

(defun scim-before-change-function (&optional beg end)
;#  (if scim-debug (scim-message "change buffer (beg:%s  end:%s)" beg end))
;#  (if scim-debug (scim-message "cursor positon: %s" (point)))
  (if (listp scim-buffer-undo-list) ; Undo enabled?
      (scim-abort-preedit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage buffer switching
(defun scim-check-current-buffer ()
;#;  (if scim-debug (scim-message "check current buffer"))
  (with-current-buffer (window-buffer)
    (if (or buffer-read-only scim-mode-map-disabled)
	(unless scim-mode-map-prev-disabled
	  (scim-disable-keymap-internal))
      (if scim-mode-map-prev-disabled
	  (scim-enable-keymap-internal)))
    (let ((buffer (current-buffer)))
      (unless (eq buffer scim-last-command-buffer)
	;; Focus out if buffer is switched to another
;#	(if scim-debug (scim-message "buffer was changed from %s to %s" scim-last-command-buffer buffer))
	(when scim-last-command-buffer
	  (with-current-buffer scim-last-command-buffer
	    (when (stringp scim-imcontext-id)
	      (if scim-frame-focus (scim-change-focus nil))
	      (if scim-preedit-buffer
		  ;; Cleenup preedit if focus change become timeout
		  (scim-abort-preedit)))))
	(setq scim-last-command-buffer buffer)
	;; Focus in if window is active
	(when (stringp scim-imcontext-id)
	  (scim-check-frame-focus t)))
      ;; Switch IMContext between global and local
      (when (eq (local-variable-p 'scim-imcontext-id)
		(not scim-mode-local))
	(scim-deregister-imcontext)
	(unless scim-mode-local
	  (scim-check-frame-focus t))
	(setq scim-last-command-buffer buffer))
      ;; Check whether the buffer has registered
      (unless (or scim-imcontext-id
		  (and (not (minibufferp buffer))
		       (eq (aref (buffer-name buffer) 0) ?\ ))) ; Buffer invisible?
;#	(if scim-debug (scim-message "new buffer was detected: %s" buffer))
	(scim-register-imcontext)))))

(defun scim-kill-buffer-function ()
  (if (local-variable-p 'scim-imcontext-id)
      (scim-deregister-imcontext)
    (if (eq scim-last-command-buffer (current-buffer))
	(setq scim-last-command-buffer nil))))

(defun scim-exit-minibuffer-function ()
  (if (and scim-imcontext-temporary-for-minibuffer
	   scim-mode-local)
      (scim-deregister-imcontext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with agent through an UNIX domain socket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disconnect
(defun scim-bridge-disconnect ()
  (let ((proc scim-bridge-socket))
    (setq scim-bridge-socket nil)
    (if (processp proc)
	(delete-process proc)
;#      (if scim-debug (scim-message "process is already deleted."))
      )))

(defun scim-bridge-process-sentinel (proc stat)
;#  (if scim-debug (scim-message "process: %s  status: %s" proc (substring stat 0 -1)))
  (scim-mode-quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connect
(defun scim-bridge-connect-using-script ()
  (let* ((path scim-bridge-socket-script-path)
	 (script (concat path
			 (if (and (> (length path) 0)
				  (string= (substring path -1) "/"))
			     "" "/")
			 scim-bridge-socket-script-name)))
    (unless (and (file-exists-p script)
		 (file-executable-p script)
		 (save-current-buffer
		   (let* ((buf (find-file script))
			  (str (buffer-string)))
		     (kill-buffer buf)
		     (string= str scim-bridge-socket-script))))
      (unless (file-directory-p path)
	(make-directory path))
;#      (if scim-debug (scim-message "write the script file: %s" script))
      (with-temp-file script (insert scim-bridge-socket-script))
      (set-file-modes script ?\700))
    (start-process scim-bridge-name
		   scim-tmp-buffer-name
		   script
		   scim-bridge-socket-path)))

(defun scim-bridge-connect-internal ()
  (unless (file-exists-p scim-bridge-socket-path)
    (scim-message "launch scim-bridge...")
    (call-process-shell-command scim-bridge-name nil 0 nil "--noexit")
    (let ((i 30))
      (while (and (not (file-exists-p scim-bridge-socket-path))
		  (> (setq i (1- i)) 0))
	(sleep-for 0.1))
      (if (= i 0) (error "scim-bridge: %s" "socket not found!"))))
  (if (and (not scim-bridge-socket-use-script)
	   (featurep 'make-network-process))
      (make-network-process
       :name scim-bridge-name
       :service scim-bridge-socket-path
       :buffer scim-tmp-buffer-name
       :family 'local :server nil :noquery t)
    (scim-bridge-connect-using-script)))

(defun scim-bridge-connect ()
  (if (and (processp scim-bridge-socket)
	   (not (assq (process-status scim-bridge-socket) '(open run))))
      (scim-bridge-disconnect))
  (unless (and (processp scim-bridge-socket)
	       (assq (process-status scim-bridge-socket) '(open run)))
    (let ((proc
	   (condition-case err
	       (scim-bridge-connect-internal)
	     (file-error ; Connection refused by agent
	      (scim-message "%s" err)
	      (shell-command (concat "killall " scim-bridge-name))
	      (scim-bridge-connect-internal)))))
;#      (if scim-debug (scim-message "process: %s  status: %s" proc (process-status proc)))
      (setq scim-bridge-socket proc)
      ;; `process-kill-without-query' is an obsolete function (as of Emacs 22.1)
;      (process-kill-without-query proc)
      (set-process-query-on-exit-flag proc nil)
      (set-process-coding-system proc 'utf-8 'utf-8)
      (set-process-sentinel proc 'scim-bridge-process-sentinel))
    (with-current-buffer scim-tmp-buffer-name
      (setq scim-tmp-buffer (current-buffer))
;#      (if scim-debug (scim-message "temp buffer: %s" scim-tmp-buffer))
      (unless scim-debug (buffer-disable-undo))
      (erase-buffer)
      ;; `make-local-hook' is an obsolete function (as of Emacs 21.1)
;      (make-local-hook 'after-change-functions)
      (add-hook 'after-change-functions
		'scim-bridge-receive-commands nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communicate with agent
(defun scim-bridge-receive-commands (beg &optional end lng)
  (when (eq (current-buffer) scim-tmp-buffer)
    (save-current-buffer (accept-process-output nil 0 1))
    (let* ((i scim-bridge-timeout)
	   (repl (buffer-substring-no-properties beg (point-max))))
      (while (and (> (setq i (1- i)) 0)
		  (or (string= repl "")
		      (not (string= (substring repl -1) "\n"))))
	(save-current-buffer (accept-process-output nil 0 1))
	(setq repl (buffer-substring-no-properties beg (point-max))))
      (erase-buffer)
;#      (if scim-debug (scim-message "recv:\n%s" repl))
;#      (if scim-debug (scim-message "(received in %sms)" (- scim-bridge-timeout i)))
      (if (string= repl "")
	  (scim-message "Data reception became timeout.")
	(if scim-last-command-buffer
	    (with-current-buffer scim-last-command-buffer
	      (let ((inhibit-modification-hooks nil))
;#		(if scim-debug (scim-message "before-change-functions: %s" before-change-functions))
		(scim-parse-reply (scim-split-commands repl)))
	      (if inhibit-modification-hooks
		  (run-hooks 'post-command-hook))
	      )))
      )))

(defun scim-bridge-send-command (cmd)
  (with-current-buffer scim-tmp-buffer
    (remove-hook 'after-change-functions 'scim-bridge-receive-commands t)
    (erase-buffer)
;#    (if scim-debug (scim-message "process: %s  status: %s" scim-bridge-socket (process-status scim-bridge-socket)))
;#    (if scim-debug (scim-message "send: %S" cmd))
    (unwind-protect
	(progn
	  (process-send-string scim-bridge-socket (concat cmd "\n"))
	  (if scim-bridge-wait-reply (scim-bridge-receive-commands 1)))
      (add-hook 'after-change-functions
		'scim-bridge-receive-commands nil t))))

(defun scim-bridge-send-only (cmd)
  (let ((scim-bridge-wait-reply nil))
    (scim-bridge-send-command cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Send command to agent
(defun scim-register-imcontext ()
  (unless scim-imcontext-id
    (when scim-mode-local
      (make-local-variable 'scim-imcontext-id)
      (put 'scim-imcontext-id 'permanent-local t)
      (make-local-variable 'scim-imcontext-status)
      (put 'scim-imcontext-status 'permanent-local t))
    (setq scim-imcontext-id 'RQ)
    (scim-bridge-send-command "register_imcontext")))

(defun scim-deregister-imcontext () ;(id)
  (if (stringp scim-imcontext-id)
      (progn
	(if scim-frame-focus (scim-change-focus nil))
	(scim-bridge-send-command
	 (concat "deregister_imcontext " scim-imcontext-id)))
    (if (local-variable-p 'scim-imcontext-id)
	(progn
	  (kill-local-variable 'scim-imcontext-id)
	  (kill-local-variable 'scim-imcontext-status))
      (setq-default scim-imcontext-id nil)
      (setq-default scim-imcontext-status nil))
    (if (eq scim-last-command-buffer (current-buffer))
	(setq scim-last-command-buffer nil))))

(defun scim-reset-imcontext () ;(id)
;#;  (if scim-debug (scim-message "buffer: %s" (current-buffer)))
  (scim-bridge-send-command
   (concat "reset_imcontext " scim-imcontext-id)))

(defun scim-set-preedit-mode () ;(id mode)
  (scim-bridge-send-command
   (concat "set_preedit_mode " scim-imcontext-id " embedded")))

(defun scim-change-focus (focus-in) ;(id focus-in)
  (scim-bridge-send-command
   (concat "change_focus " scim-imcontext-id
	   (if focus-in " true" " false"))))

(defun scim-set-cursor-location () ;(id x y)
  (let* ((pixpos (save-excursion
		   (goto-char (+ scim-preedit-point scim-preedit-curpos))
		   (scim-compute-pixel-position)))
	 (x (number-to-string
	     (max (- (car pixpos) scim-adjust-window-x-offset) 1)))
	 (y (number-to-string (cdr pixpos))))
;#    (if scim-debug (scim-message "cursor position: (%s, %s)" x y))
    (scim-bridge-send-only
     (concat "set_cursor_location " scim-imcontext-id " " x " " y))))

(defun scim-handle-key-event (key-code key-pressed modifiers) ;(id key-code key-pressed &rest modifiers)
  (scim-bridge-send-command
   (scim-construct-command
    (append (list "handle_key_event"
		  scim-imcontext-id key-code key-pressed)
	    modifiers))))

(defun scim-preedit-updated ()
  (scim-bridge-send-only "preedit_updated"))

(defun scim-string-commited ()
  (scim-bridge-send-only "string_commited"))

(defun scim-surrounding-text-gotten (retval cursor-position string)
  (scim-bridge-send-command
   (scim-construct-command
    (cons "surrounding_text_gotten"
	  (if retval
	      (list "true" (number-to-string cursor-position) string)
	    (list "false"))))))

(defun scim-surrounding-text-deleted (retval)
  (scim-bridge-send-command
   (concat "surrounding_text_deleted " (if retval "true" "false"))))

(defun scim-surrounding-text-replaced (retval)
  (scim-bridge-send-command
   (concat "surrounding_text_replaced " (if retval "true" "false"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process commands from agent to clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scim-imengine-status-changed (id enabled)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-imcontext-status (equal enabled "TRUE"))
    (scim-set-cursor-color)))

(defun scim-preedit-mode-changed ()
  t)

(defun scim-imcontext-registered (id)
  ;; Initialize IMContext
;#  (if scim-debug (scim-message "imcontext registered (id: %s  buf: %s)" id (if scim-mode-local (current-buffer) "global")))
  (scim-cancel-focus-update-timer)
  (setq scim-imcontext-id id
	scim-preedit-prev-string ""
	scim-preedit-overlays nil)
  (scim-cleanup-preedit)
  (scim-set-preedit-mode)
  (scim-check-frame-focus t) ; Focus in
  (scim-start-focus-observation))

(defun scim-imcontext-deregister ()
;#  (if scim-debug (scim-message "imcontext deregistered (id: %s  buf: %s)" scim-imcontext-id (if (local-variable-p 'scim-imcontext-id) (current-buffer) "global")))
  (if (local-variable-p 'scim-imcontext-id)
      (progn
	(kill-local-variable 'scim-imcontext-id)
	(kill-local-variable 'scim-imcontext-status))
    (setq-default scim-imcontext-id nil)
    (setq-default scim-imcontext-status nil))
  (setq scim-last-command-buffer nil))

(defun scim-imcontext-reseted (id)
  t)

(defun scim-cursor-location-changed ()
  t)

(defun scim-key-event-handled (consumed)
  (if (or (string= consumed "true")
	  (null scim-last-command-event))
      ;; If key event is handled
      (when scim-last-command-event
	;; Send cursor location for displaying SCIM-Ruby history window
	(when (and (string= scim-preedit-prev-string "")
		   (string= scim-preedit-string ""))
	  (let ((scim-preedit-point (point))
		(scim-adjust-window-x-offset 0))
	    (scim-set-cursor-location)))
	(setq scim-last-command-event nil))
    ;; If key event is ignored
    (scim-do-update-preedit)
    (let* ((vec (vector scim-last-command-event))
	   (event (or (and (boundp 'local-function-key-map)
			   (lookup-key local-function-key-map vec))
		      (lookup-key function-key-map vec)))
	   keybind)
      (setq event (or (and (arrayp event)
			   (car (listify-key-sequence event)))
		      scim-last-command-event))
      (let ((minor-mode-overriding-map-alist
	     (cons '(scim-mode) minor-mode-overriding-map-alist))
	    (scim-mode-map-alist nil))
	(setq keybind (key-binding (vector event)))
	(if (and (null keybind)
		 (integerp event))
	    ;; Reset the 25th bit corresponding to the shift key
	    (setq event (logand event (lognot ?\x2000000))
		  keybind (key-binding (vector event)))))
;#      (if scim-debug (scim-message "event: --> %s --> %s" scim-last-command-event event))
      (setq scim-last-command-event nil
	    last-command-event event
	    this-command keybind)
;#      (if scim-debug (scim-message "execute command: %s" keybind))
      (cond ((not (commandp keybind))
	     (scim-message "%s is undefined"
			   (single-key-description (aref vec 0))))
	    ((eq keybind 'self-insert-command)
	     (scim-insert-and-modify-undo-list (char-to-string event)))
	    (t (command-execute keybind)))
      )))

(defun scim-update-preedit (id)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-preedit-update t)))

(defun scim-set-preedit-string (id string)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-preedit-string string)))

(defun scim-set-preedit-attributes (id &rest attrs)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-preedit-attributes attrs)))

(defun scim-set-preedit-cursor-position (id position)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
;#    (if scim-debug (scim-message "cursor position: %s" position))
    (setq scim-preedit-curpos (string-to-number position))))

(defun scim-set-preedit-shown (id shown)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-preedit-shown shown)))

(defun scim-set-commit-string (id string)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
;#    (if scim-debug (scim-message "commit string: %s" string))
    (setq scim-committed-string string)))

(defun scim-commit-string (id)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (if buffer-read-only
	(scim-message "Buffer is read-only: %S" (current-buffer))
      (scim-remove-preedit)
      (condition-case err
	  (progn
	    (if scim-undo-by-committed-string
		(insert scim-committed-string)
	      (scim-insert-and-modify-undo-list scim-committed-string))
	    (unless this-command (undo-boundary)))
	(text-read-only
	 (scim-message "Failed to commit string %s" err)
	 (setq scim-string-insertion-failed t)))
      (setq scim-surrounding-text-modified t)
      (scim-redraw-preedit))))

(defun scim-forward-key-event (id key-code key-pressed &rest modifiers)
  (let ((event (scim-encode-event key-code modifiers)))
    (if event
	(if (string= key-pressed "TRUE")
	    (setq unread-command-events (cons event
					      unread-command-events)))
      (if (not (string= id scim-imcontext-id))
	  (scim-message "IMContext ID (%s) is mismatched." id)
	(scim-handle-key-event key-code key-pressed modifiers))
      )))

(defun scim-get-surrounding-text (id before-max after-max)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (let* ((len-before (scim-twos-complement before-max))
	   (len-after (scim-twos-complement after-max))
	   (end-before (if scim-preedit-buffer
			   scim-preedit-point
			 (point)))
	   (beg-after (if scim-preedit-buffer
			  (+ end-before (length scim-preedit-prev-string))
			end-before))
	   (beg-before (if (>= len-before 0)
			   (max (- end-before len-before) (point-min))
			 (line-beginning-position)))
	   (end-after (if (>= len-after 0)
			  (min (+ beg-after len-after) (point-max))
			(line-end-position)))
	   (str-before (buffer-substring-no-properties beg-before end-before))
	   (str-after (buffer-substring-no-properties beg-after end-after))
	   (string (concat str-before str-after))
	   (cursor (length str-before))
	   (retval (> (length string) 0)))
;#      (if scim-debug (scim-message "val: %s  str: %s  pos: %d" retval string cursor))
      (scim-surrounding-text-gotten retval cursor string))))

(defun scim-delete-surrounding-text (id offset length)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (if buffer-read-only
	(scim-message "Buffer is read-only: %S" (current-buffer))
;#      (if scim-debug (scim-message "delete surrounding text"))
      (scim-remove-preedit)
      (let* ((pos (point))
	     (beg (+ pos (scim-twos-complement offset)))
	     (end (+ beg (string-to-number length)))
	     (retval t))
	(condition-case err
	    (delete-region beg end)
	  (text-read-only
	   (scim-message "Failed to delete surrounding text %s" err)
	   (setq retval nil
		 scim-string-insertion-failed t)))
	(setq scim-surrounding-text-modified t)
	(scim-redraw-preedit)
	(scim-surrounding-text-deleted retval)))))

(defun scim-replace-surrounding-text (id corsor-index string)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (if buffer-read-only
	(scim-message "Buffer is read-only: %S" (current-buffer))
;#      (if scim-debug (scim-message "replace surrounding text"))
      (scim-remove-preedit)
      (let* ((pos (point))
	     (beg (- pos (scim-twos-complement corsor-index)))
	     (end (+ beg (length string)))
	     (retval t))
	(condition-case err
	    (progn
	      (delete-region beg end)
	      (goto-char beg)
	      (insert string))
	  (text-read-only
	   (scim-message "Failed to replace surrounding text %s" err)
	   (setq retval nil
		 scim-string-insertion-failed t)))
	(goto-char pos)
	(setq scim-surrounding-text-modified t)
	(scim-redraw-preedit)
	(scim-surrounding-text-replaced retval)))))

(defun scim-focus-changed ()
  t)

(defun scim-beep (id)
  (ding t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execute commands replied from agent
(defun scim-parse-reply (cmdlist)
;#  (if scim-debug (scim-message "this-command: %s" this-command))
;#  (if scim-debug (scim-message "inhibit-modification-hooks: %s" inhibit-modification-hooks))
  (while cmdlist
    (let* ((args (car cmdlist))
	   (cmd (cdr (assoc (car args) scim-reply-alist))))
      (if cmd
	  (progn
;#	    (if scim-debug (scim-message "execute: %s" args))
	    (apply cmd (cdr args)))
	(scim-message "Unknown command received from agent: %s" (car args))
	))
    (setq cmdlist (cdr cmdlist)))
  (scim-do-update-preedit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process key events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scim-wait-following-key-event (prev-event key-code modifiers)
  (let ((event (read-event nil nil scim-key-release-delay)))
    (when event
      (if (or (eq event prev-event)
	      (not (eq (key-binding (vector event)) 'scim-handle-event)))
	  (setq unread-command-events (cons event unread-command-events))
	(let ((scim-key-release-delay nil))
	  (scim-dispatch-key-event event))))
    (scim-handle-key-event key-code "false" modifiers)))

(defun scim-dispatch-key-event (event)
  (let* ((elist (scim-decode-event event))
	 (key-code (car elist))
	 (modifiers (cdr elist)))
    (if (numberp key-code) (scim-check-current-buffer))
;#;    (if scim-debug (scim-message "event: %s" elist))
;#    (if scim-debug (scim-message "event: %s" event))
    (when (member "kana_ro" modifiers)
      (setq event (event-convert-list
		   (append (event-modifiers event) (list key-code))))
;#      (if scim-debug (scim-message "event: --> %s" event))
      (unless (eq (key-binding (vector event)) 'scim-handle-event)
	(setq key-code nil
	      unread-command-events (cons event unread-command-events))))
    (when key-code
      (setq scim-last-command-event event
	    scim-surrounding-text-modified nil)
      (if (and (stringp scim-imcontext-id)
	       (numberp key-code))
	  ;; Send a key event to agent
	  (let ((scim-string-insertion-failed nil))
	    (setq key-code (number-to-string key-code))
	    (scim-handle-key-event key-code "true" modifiers)
	    (if (and scim-key-release-delay
		     (not scim-string-insertion-failed))
		(scim-wait-following-key-event event key-code modifiers)
	      (scim-handle-key-event key-code "false" modifiers)))
	;; IMContext is not registered or key event is not recognized
	(scim-key-event-handled "false"))))
  (unless (memq 'scim-check-current-buffer
		(default-value 'post-command-hook))
;#    (if scim-debug (scim-message "`post-command-hook' was reset. now add the hook again."))
    (add-hook 'post-command-hook 'scim-check-current-buffer)))

(defun scim-handle-event (&optional arg)
  (interactive "*p")
  (scim-dispatch-key-event last-command-event)
  (scim-cancel-focus-update-timer)
  (scim-start-focus-observation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scim-mode-on ()
  (interactive)
  (if (not window-system)
      (scim-mode-quit)
    (if scim-bridge-socket (scim-mode-off)) ; Restart scim-mode
    (unwind-protect
	(scim-bridge-connect)
      (if (not (and scim-bridge-socket
		    (memq (process-status scim-bridge-socket)
			  '(open run))))
	  ;; Connection failed
	  (scim-mode-quit)
	;; Connection was established
	;; Cleenup variables
	(let ((buffers (buffer-list)))
	  (save-current-buffer
	    (while buffers
	      (set-buffer (car buffers))
	      (kill-local-variable 'scim-imcontext-id)
	      (kill-local-variable 'scim-imcontext-status)
	      (kill-local-variable 'scim-mode-map-prev-disabled)
	      (setq buffers (cdr buffers)))))
	(setq-default scim-imcontext-id nil)
	(setq-default scim-imcontext-status nil)
	(setq scim-last-command-buffer nil
	      scim-preedit-default-attr nil
	      scim-config-last-modtime nil
	      scim-focus-observation-infrequent nil)
	;; Initialize key binding
	(scim-update-key-bindings)
	(when (and (fboundp 'add-to-ordered-list)
		   (boundp 'emulation-mode-map-alists))
	  (add-to-ordered-list
	   'emulation-mode-map-alists 'scim-mode-map-alist 50))
	;; Turn on minor mode
	(add-hook 'minibuffer-exit-hook 'scim-exit-minibuffer-function)
	(mapc (lambda (hook)
		(add-hook hook 'scim-disable-keymap))
	      scim-incompatible-mode-hooks)
	(add-hook 'post-command-hook 'scim-check-current-buffer)
;#	(if scim-debug (scim-message "post-command-hook: %s" post-command-hook))
	(add-hook 'kill-buffer-hook 'scim-kill-buffer-function)
	(add-hook 'kill-emacs-hook 'scim-mode-off)
	(setq-default scim-mode t)))
    (scim-update-mode-line)))

(defun scim-mode-quit ()
  (remove-hook 'kill-emacs-hook 'scim-mode-off)
  (remove-hook 'kill-buffer-hook 'scim-kill-buffer-function)
  (remove-hook 'post-command-hook 'scim-check-current-buffer)
  (mapc (lambda (hook)
	  (remove-hook hook 'scim-disable-keymap))
	scim-incompatible-mode-hooks)
  (remove-hook 'minibuffer-exit-hook 'scim-exit-minibuffer-function)
  (kill-buffer scim-tmp-buffer)
  (scim-cancel-focus-update-timer)
  (scim-cleanup-preedit)
  (when (and (fboundp 'add-to-ordered-list)
	     (boundp 'emulation-mode-map-alists))
    (setq emulation-mode-map-alists
	  (delq 'scim-mode-map-alist emulation-mode-map-alists)))
  (when scim-cursor-color
    (set-cursor-color (frame-parameter nil 'foreground-color)))
  (scim-update-kana-ro-key nil)
  (scim-bridge-disconnect)
  (setq-default scim-mode nil)
  (scim-update-mode-line))

(defun scim-mode-off ()
  (interactive)
  (when (and (stringp scim-imcontext-id)
	     scim-frame-focus)
    (scim-change-focus nil)
    (setq scim-frame-focus nil))
  ;; Deregister IMContext IDs
  (let ((buffers (buffer-list)))
    (save-current-buffer
      (while buffers
	(set-buffer (car buffers))
	(when (local-variable-p 'scim-imcontext-id)
	  (setq scim-last-command-buffer (current-buffer))
	  (scim-deregister-imcontext))
	(setq buffers (cdr buffers)))))
  (scim-deregister-imcontext)
  ;; Turn off minor mode
  (scim-mode-quit))

(defun scim-update-mode ()
  (if scim-mode
      (scim-mode-on)
    (scim-mode-off)))

(defun scim-mode (&optional arg)
  "Start/Stop Scim conversion system."
  (interactive "P")
  (if (not (or window-system scim-mode))
      (prog1 nil
	(scim-message "scim-mode needs Emacs to run as X application."))
    (setq-default scim-mode
		  (if (null arg)
		      (not scim-mode)
		    (> (prefix-numeric-value arg) 0)))
    (scim-update-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup minor-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor-mode-alist
(unless (assq 'scim-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(scim-mode scim-mode-line-string)
	      minor-mode-alist)))
;; minor-mode-map-alist
(unless (assq 'scim-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'scim-mode scim-mode-map)
	      minor-mode-map-alist)))

(provide 'scim-bridge)

;;;
;;; scim-bridge.el ends here
