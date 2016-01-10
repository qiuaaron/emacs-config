;;; scim-bridge-zh-si.el

;; Copyright (C) 2008 S. Irie
;; Copyright (C) 2008 Andy Stewart

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Input Method, i18n

(defconst scim-bridge-zh-si-version "0.7.1")

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

;; scim-bridge.el is SCIM-Bridge client for GNU Emacs. It is,
;; however, not part of official SCIM-Bridge.

;; This program changes the documentation strings of the variables
;; and functions defined in scim-bridge.el into the equivalents
;; which are written in Simplified Chinese.

;;
;; Installation:
;;
;; First, save this file (scim-bridge-zh-si.el) and scim-bridge.el
;; in a directory listed in load-path, and then byte-compile them.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'scim-bridge-zh-si)
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
;;   (require 'scim-bridge-zh-si)
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
;;         * Add/modify translations
;;         * Version 0.7.1
;;
;; 2008-10-08  S. Irie
;;         * First release
;;         * Version 0.7.0.1
;;
;; 2008-10-04  Andy Stewart
;;         * Translated from scim-bridge-en.el
;;         * Version 0.7.0

;; ToDo:

;;; Code:

(require 'scim-bridge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Apply translations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(scim-set-group-doc
 'scim
 "智能通用输入法平台")

(scim-set-variable-doc
 'scim-mode
 "切换 scim-mode.
直接设置这个变量没有效果;
使用 \\[customize] 或 函数 `scim-mode'.")

;; Basic settings
(scim-set-group-doc
 'scim-basic
 "操作设置, 例如模式设置和键盘设置")

(scim-set-variable-doc
 'scim-mode-local
 "如果值为非空 (non-nil), 输入法内容会对每一个缓存进行注册以使
各个缓存可以单独切换输入法. 否则, 输入法进行全局切换.")

(scim-set-variable-doc
 'scim-imcontext-temporary-for-minibuffer
 "如果为值为非空 (non-nil), 当在 minibuffer 输入内容时总是关闭 SCIM 的输入状态.
这个选项只有当选项 `scim-mode-local' 激活 (non-nil) 时才有效.")

(scim-set-variable-doc
 'scim-common-function-key-list
 "这个列表(list)指示SCIM在直接插入模式和预编辑模式接管的按键.
你可以使用函数 `scim-define-common-key' 添加或删除其元素.
注意: 不要在这个选项设置前缀按键, 例如 ESC 或 C-X.
如果你要这么做, Emacs可能会变得不可操作.")

(scim-set-variable-doc
 'scim-preedit-function-key-list
 "这个列表(list)指示SCIM在预编辑区域存在时接管的按键.
你可以使用函数 `scim-define-preedit-key' 添加或删除其元素.")

(scim-set-variable-doc
 'scim-use-kana-ro-key
 "如果你用jp-106键盘和日本kana输入法(Japanese kana typing method),
开启这个选项(non-nil)以使不按shift按键输入 kanna 字符 `ろ'.
这个选项可以使用 `xmodmap' 命令临时的修改 X-windows 系统的键盘设置.")

(scim-set-variable-doc
 'scim-key-release-delay
 "如果你用jp-106键盘和基于SCIM-Anthy的日本thumb shift输入法 (Japanese
thumb shift typing method). 设置按键的时间延迟 (单位: 秒).
设置一个小于SCIM-Anthy中 \"同时按下按键的时间设置\"的值."
 '(choice (const :tag "没有延迟" nil)
          (number :tag "延迟 (单位: 秒)"
                  :value 0.1)))

(scim-set-variable-doc
 'scim-undo-by-committed-string
 "如果这个值为空 (nil),
撤销执行并使一些短的转换字符串 (committed string) 连在一齐或划分长的
转换字符串(不操过 20 个字符). 否则, 每一个转换字符串执行撤销.")

(scim-set-variable-doc
 'scim-clear-preedit-when-unexpected-event
 "当一个意想不到的事件发生在预编辑时, 预编辑区域被清空,
并设置这个值为非空 (non-nil).
意想不到的事件是指, 比如, 字符串被鼠标粘帖了.")

;; Appearance
(scim-set-group-doc
 'scim-appearance
 "外观, 候选窗口, 等等.")

(scim-set-face-doc
 'scim-preedit-default-face
 "这个外观显示整个预编辑区域.")

(scim-set-face-doc
 'scim-preedit-underline-face
 "整个外观对应于SCIM图形设置工具文本中的下划线(`Underline')属性.")

(scim-set-face-doc
 'scim-preedit-highlight-face
 "整个外观对应于SCIM图形设置工具文本中的高亮(`Highlight')属性.")

(scim-set-face-doc
 'scim-preedit-reverse-face
 "整个外观对应于SCIM图形设置工具文本中的背景 (`Reverse')属性.")

(scim-set-variable-doc
 'scim-cursor-color
 "如果这个值是一个字符串, 它指示出当 SCIM 开启时光标所应用的颜色.
如果这个值为一个单元 (cons cell), 它的第一个元素 (car) 和 剩下的元素 (cdr) 分别指示
SCIM 开启 和 关闭 时的光标颜色. 如果这个值为空 (nil) 意味着不控制光标的颜色."
 ;; ** Don't translate `:value' properties!! **
 '(choice (const :tag "空的 (nil)" nil)
          (color :tag "红色" :format "红色 (%{sample%})\n" :value "red")
          (color :tag "蓝色" :format "蓝色 (%{sample%})\n" :value "blue")
          (color :tag "绿色" :format "绿色 (%{sample%})\n" :value "green")
          (color :tag "其他" :value "red")
          (cons  :tag "其他 (开 . 关)"
                 (color :format "开: %v (%{sample%})  " :value "red")
                 (color :format "关: %v (%{sample%})\n" :value "blue"))))

(scim-set-variable-doc
 'scim-cursor-type-for-candidate
 "这个选项指定预编辑窗口显示候选变换时光标的形状.
如果这个指为空 (nil) 意味者光标形状没有改变."
 '(choice (const :tag "默认 (nil)" nil)
          (const :tag "方形" box)
          (const :tag "条状" bar)
          (cons :tag "条状 (指定宽度)"
                (const :format "" bar)
                (integer :tag "宽度" :value 1))
          (const :tag "不可见的" (bar . 0))))

(scim-set-variable-doc
 'scim-put-cursor-on-candidate
 "如果这个值为非空 (non-nil) 时, 当预编辑区域显示候选转换时, 光标方在
选择段的位置, 否则光标方在预编辑区域的顶端.")

(scim-set-variable-doc
 'scim-adjust-window-x-position
 "这个选项指定是否对候选窗口的位置进行调整, 候选窗口可能按纵向排列.
如果这个值为 'gnome', 会使用 GNOME 桌面环境的字体大小进行调整.
否则, 如果这个值是整数, 指示从正常位置处的进行调整的像素数量.
这个值不适用于候选窗口总是显示的输入法, 比如 SCIM-pinyin (中文拼音输入法).
因为当光标在屏幕底部时, 窗口会隐藏光标."
 '(choice (const :tag "使用 GNOME 的字体大小" gnome)
          (integer :tag "指定像素数量" :value 24)
          (const :tag "关闭" nil)))

(scim-set-variable-doc
 'scim-adjust-window-y-position
 "如果这个值为非空 (non-nil), 候选窗口纵向位置会相对于命令 `xwininfo'
得到光标的底部进行调整. 否则, 不进行调整. 因此窗口可能会在精确位置的下面一点.")

(scim-set-variable-doc
 'scim-prediction-window-position
 "(只用于日本输入法) 这个值以 (POS . ADJ) 的形式给出.
如果 POS 的值为非空 (non-nil), 预想的窗口会显示在预编辑区域的开头.
如果 ADJ 的值为非空 (non-nil), 横向调整, 调整值和
`scim-adjust-window-x-position' 相同."
 '(cons
   (choice :tag "位置"
           (const :tag "预编辑区域的末尾" nil)
           (const :tag "预编辑区域的开头" t))
   (choice :tag "调整"
           (const :tag "和转换窗口一样" t)
           (const :tag "关闭" nil))))

(scim-set-variable-doc
 'scim-mode-line-string
 "这个变量指定一个当 scim-mode 激活时显示在 mode-line 上的描述
scim-mode 的字符串.")

;; Advanced settings
(scim-set-group-doc
 'scim-expert
 "高级设置")

(scim-set-variable-doc
 'scim-focus-update-interval
 "窗口更新周期的时间间隔, 单位是秒.")

(scim-set-variable-doc
 'scim-focus-update-interval-long
 "当窗口不能用命令 `xgrop' 处理时用 `xwininfo' 替代,
但是不是用 `scim-focus-update-interval' 而是用这个值作为一个更新周期.")

(scim-set-variable-doc
 'scim-kana-ro-x-keysym
 "当日本 kana-RO 按键使用时, 这个选项指定事件对应于 X window 系统给出的
替代的按键系统符号. 这个程序设置替代按键系统为了从 yen-mark 按键中区别反斜线
符号按键.")

(scim-set-variable-doc
 'scim-kana-ro-key-symbol
 "当日本 kana-RO 按键使用时, 这个选项指定事件对应于 `scim-kana-ro-x-keysym'
给出的替代的按键系统符号. 这个程序设置替代按键系统为了从 yen-mark 按键中区别
反斜线符号按键."
 '(choice (symbol)
          (const :tag "空的" nil)))

(scim-set-variable-doc
 'scim-bridge-timeout
 "如果没有特殊的原因不要改变这个设置.")

(scim-set-variable-doc
 'scim-bridge-wait-reply
 "如果没有特殊的原因不要改变这个设置.")

(scim-set-variable-doc
 'scim-bridge-socket-use-script
 "如果这个值是非空的 (non-nil), 使用用户脚本替代 UNIX 域名套接字
内置的函数 `make-network-process'.
当 Emacs 版本是 22 或 以后的版本时, 这个脚本通常不是必需的.")

(scim-set-variable-doc
 'scim-bridge-socket-script-path
 "套接字脚本的目的目录.
如果目录不存在, 自动创建一个.")

(scim-set-variable-doc
 'scim-bridge-socket-script-name
 "套接字脚本的文件名称.")

(scim-set-variable-doc
 'scim-bridge-socket-script
 "套接字脚本的真正内容.
第一个命令行参数必需为套接字(socket)名字.")

(scim-set-variable-doc
 'scim-config-file
 "SCIM 配置文件的名称, 用于探测 SCIM 的设置改变.")

(scim-set-variable-doc
 'scim-meta-key-exists
 "如果键盘存在 mata 按键修改, 设置这个值为 t.
当自动判定不能很好工作时, 请在 `scim-bridge.el' 加载前手动设置这个值.")

(scim-set-variable-doc
 'scim-tmp-buffer-name
 "这个是用来和代理 (scim-agent) 进行通信的缓存的名字.")

(scim-set-variable-doc
 'scim-incompatible-mode-hooks
 "当这个勾子 (hooks) 运行, scim-mode-map 变得无效的")

;; Functions
(scim-set-function-doc
 'scim-set-group-doc
 "改变组的文档字符串.
如果字符串 (STRING) 为空 (empty or nil), 文档字符串保留原来的.")

(scim-set-function-doc
 'scim-set-variable-doc
 "改变变量的文档字符串.
如果字符串 (STRING) 为空 (empty or nil), 文档字符串保留原来的.
如果自定义类型 (CUSTOM-TYPE) 为非空 (non-nil), 它设置为变量 (VARIABLE)的
`custom-type' 属性, 符合 `defcustom' 中的 `:type' 关键字.")

(scim-set-function-doc
 'scim-set-face-doc
 "改变外观的文档字符串.
如果字符串 (STRING) 为空 (empty or nil), 文档字符串保留原来的.")

(scim-set-function-doc
 'scim-set-function-doc
 "改变函数的文档字符串.
如果字符串 (STRING) 为空 (empty or nil), 文档字符串保留原来的.")

(scim-set-function-doc
 'scim-define-common-key
 "SCIM在预编辑时接管的特殊的按键事件.
如果操作 (HANDLE) 为非空 (non-nil), SCIM根据给出的按键 (KEY) 进行按键事件操作.
当给出的按键是一个数组时, 它不会指示按键序列, 但是对单一按键进行多重定义.
当调用函数 `scim-update-key-bindings' 或 重新启动 scim-mode 以使设置生效时
这个值是必需的.")

(scim-set-function-doc
 'scim-define-preedit-key
 "SCIM在预编辑时接管的特殊的按键事件.
如果操作 (HANDLE) 为非空 (non-nil), SCIM根据给出的按键 (KEY) 进行按键事件操作.
当给出的按键是一个数组时, 它不会指示按键序列, 但是对单一按键进行多重定义.
当调用函数 `scim-update-key-bindings' 或 重新启动 scim-mode 以使设置生效时
这个值是必需的.")

(scim-set-function-doc
 'scim-reset-imcontext-statuses
 "重新设置所有缓存 (BUFFER) 的输入状态以改正不适当的光标颜色.
这个函数可能会在使用 SCIM 图形设置工具后调用.")

(scim-set-function-doc
 'scim-get-frame-extents
 "返回框架(frame)边缘的像素宽度 (left right top bottom).
这里, `top' 是指框架标题栏的高度.")

(scim-set-function-doc
 'scim-frame-header-height
 "返回菜单栏和工具栏的总的高度.
这个函数返回的值不是那么精确.")

(scim-set-function-doc
 'scim-real-frame-header-height
 "返回菜单栏和工具栏的总的高度.
这个函数返回的值非常精确, 但是这个函数比 `scim-frame-header-height' 要慢很多.")

(scim-set-function-doc
 'scim-compute-pixel-position
 "返回屏幕上点的像素位置 (X . Y).
这个值显示字符左上角的坐标.")

(scim-set-function-doc
 'scim-get-gnome-font-size
 "返回GNOME环境中应用字体的像素大小.
当设置屏幕分辨率(点/英寸)时这个值是必需的, 使用命令 `gconftool-2'.
不然的话, 这个函数返回零 (zero).")

(scim-set-function-doc
 'scim-get-active-window-id
 "返回窗口系统中前台窗口的标识数字,
也就是输入聚焦的窗口.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define functions useful only for Simplified Chinese
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings

;; Functions

;;  (No such functions)

(provide 'scim-bridge-zh-si)

;;;
;;; scim-bridge-zh-si.el ends here
