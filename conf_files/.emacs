(menu-bar-mode -1)
(setq inhibit-startup-message t)
;;gdb和term退出后自动关闭窗口
(defun kill-buffer-when-exit ()
"Close assotiated buffer when a process exited"
(let ((current-process (ignore-errors (get-buffer-process (current-buffer)))))
(when current-process
(set-process-sentinel current-process
(lambda (watch-process change-state)
(when (string-match "\\(finished\\|exited\\)" change-state)
(kill-buffer (process-buffer watch-process))))))))
(add-hook 'gdb-mode-hook 'kill-buffer-when-exit)
(add-hook 'term-mode-hook 'kill-buffer-when-exit)
;;(require 'multi-shell)
;;(setq multi-shell-command "/bin/bash")
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'ido)
(ido-mode t)
(require 'swbuff)
(global-set-key "\M-p" 'swbuff-switch-to-previous-buffer) (global-set-key "\M-n" 'swbuff-switch-to-next-buffer) (setq swbuff-exclude-buffer-regexps '("^ " "\\*.*\\*")) (setq swbuff-status-window-layout 'scroll) (setq swbuff-clear-delay 1) (setq swbuff-separator "|") (setq swbuff-window-min-text-height 1)
(transient-mark-mode t);;高亮显示要拷贝的区域
(global-set-key "\C-xg" 'goto-line)
(require 'linum+)
;;(format "%%%dd|  " 5)
;;        =>"%2d|"
;;(set-face-foreground 'linum "yellow")
(set-face-background 'linum "grey1")
;;(set-face-background 'fringe "red")
(global-linum-mode 1)
;;以下两句的意思是:用组合键替代alt-x
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward);;对于重名buffer，标志出其上级目录名
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\M-h" "->")
;;(global-set-key "\C-x\C-a" "&& ")
(global-set-key "\C-x\C-o" "|| ")
(global-set-key "\C-c\C-s" "<= ")
;;(global-set-key "\C-c\C-b" ">= ")
(global-set-key "\C-c\C-t" "< ")
(global-set-key "\C-c\C-l" "> ")
(global-set-key "\M-o" 'next-multiframe-window)
(global-set-key "\C-o" 'previous-multiframe-window)
(global-set-key "\M-m" 'delete-other-windows)
;(global-set-key "\M-0" 'delete-window &optional WINDOW)

;;(global-set-key "\M-k" 'delete-window)
(global-set-key "\M-i" 'split-window-horizontally)
(global-set-key "\M-r" 'undo)
(global-set-key "\M-k" 'backward-kill-word)
(require 'edit-server)
(edit-server-start)
(require 'redo)
(global-set-key (kbd "C-.") 'redo)
(setq default-major-mode 'text-mode);一打开就起用 text 模式。  
(global-font-lock-mode t);语法高亮  
(auto-image-file-mode t);打开图片显示功能  
(fset 'yes-or-no-p 'y-or-n-p);以 y/n代表 yes/no  
(column-number-mode t);显示列号
(show-paren-mode t);显示括号匹配
(display-time-mode 1);显示时间，格式如下
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(tool-bar-mode nil);去掉那个大大的工具栏
(scroll-bar-mode nil);去掉滚动条
(setq mouse-yank-at-point t);支持中键粘贴。
(setq frame-title-format "%b");在标题栏提示你目前在什么位置。
(setq-default make-backup-files nil);不要生成备份文件
;;(setq frame-title-format;设置标题栏显示文件的完整路径名
;;'("%S" (buffer-file-name "%f"
;;(dired-directory dired-directory "%b"))))

(require 'yasnippet);;快速补全代码
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

(load "auctex.el" nil t t);Latex
;;(load "preview-latex.el" nil t t)
;;(require 'tex-mik)
 ;; 不使用 TAB 来缩进
 (setq TeX-auto-untabify t) 
 ;; 在工具栏显示编辑按钮
 (add-hook 'LaTeX-mode-hook #'LaTeX-install-toolbar) 
 ;; 不要默认指定主文件
 (setq TeX-auto-save nil)
 (setq TeX-parse-self t)
 (setq-default TeX-master nil)

(setq TeX-output-view-style (quote (("^pdf$" "." "xpdf %o %(outpage)"))));; xpdf

(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

(add-hook 'LaTeX-mode-hook
(lambda()
(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t));;xelatex
(setq TeX-command-default "XeLaTeX")))
(require 'color-theme)
(eval-after-load "color-theme"
'(progn
	(color-theme-initialize)
;;	(color-theme-charcoal-black)))
	(color-theme-ld-dark)))
;;	(color-theme-clarity)))
;;	(color-theme-aalto-light)))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(load-home-init-file t t))
;;启用minibuffer，好像是默认设置吧
(minibuffer-electric-default-mode 1)
;;启用部分补全功能，如输入M-x q r r相当于M-x query-replace-regexp
;;(partial-completion-mode 1)
;;在minibuffer里启用自动补全函数和变量
(icomplete-mode 1)

;;设定句子结尾，主要是针对中文设置
(setq sentence-end "\\([¡££¡£¿]\\|¡­¡­\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;允许emacs和外部其他程序的粘贴，使用x剪切板
(setq x-select-enable-clipboard t) 

;设置窗口启动大小
(setq default-frame-alist
'((height . 25) (width . 80) (menu-bar-lines . 80) (tool-bar-lines . 80)))

(setq semanticdb-project-roots 
	  (list
        (expand-file-name "/")))

;;自定义自动补齐命令，如果在单词中间就补齐，否则就是tab。
;;(load "cedet.el")

(defun my-indent-or-complete ()
   (interactive)
   (if (looking-at "\\>")
       (hippie-expand nil)
     (indent-for-tab-command)))
(global-set-key "\C-i" 'my-indent-or-complete)
(autoload 'senator-try-expand-semantic "senator")
(setq hippie-expand-try-functions-list
      '(
	senator-try-expand-semantic
	try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-expand-list
	try-expand-list-all-buffers
	try-expand-line
        try-expand-line-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
	))
;;自动插入匹配的括号
;;(setq skeleton-pair t)


;;             C++模式
(add-hook 'c++-mode-hook 'hs-minor-mode)

(defun my-c++-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
    (?` ?` _ "''")			;;去除？即可使括号之间不留空格
    (?\( _")")
    (?\[ _"]")
    (?{ \n > _ \n ?} >)))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
(add-hook 'c++-mode-hook 'my-c++-mode-auto-pair)
(add-hook 'common-lisp-mode-hook 'my-c++-mode-auto-pair)
(add-hook 'emacs-lisp-mode-hook 'my-c++-mode-auto-pair)

;;C++模式策略
(add-hook 'c++-mode-hook
    '(lambda ()
       (require 'xcscope)
       (require 'ecb-autoloads)
       (c-set-style "Stroustrup")	      ;;采用GNU缩进风格
       (which-function-mode)
       (c-toggle-auto-state)  ;;自动开始新行
       (c-toggle-auto-hungry-state))) ;;饥饿的删除键

;(add-hook 'c-mode-common-hook
;  (lambda ()
;  (define-key c-mode-base-map [(f3)] 'my-indent-or-complete)))
;  (define-key c-mode-base-map [(meta ?/)] 'my-indent-or-complete)
;;  (define-key c-mode-base-map [(f2)] 'semantic-ia-complete-symbol-menu)
;;  (define-key c-mode-base-map [(f5)] 'compile)
;;  '(compile-command "gcc");;使用make，代替make -k编译
;;))
;;(global-set-key [(f4)] 'speedbar)

;;自动插入匹配的括号
;;             C模式
;;(add-hook 'c-mode-hook 'hs-minor-mode)
;;(defun my-c-mode-auto-pair ()
;;  (interactive)
;;  (make-local-variable 'skeleton-pair-alist)
;;  (setq skeleton-pair-alist  '(
;;    (?` ?` _ "''")
;;    (?\( _")")
;;    (?\[ _"]")
;;    (?{ \n > _ \n ?} >)))
;;  (setq skeleton-pair t)
;;  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;;  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;;  (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
;;  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
;;  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe))
;;(add-hook 'c-mode-hook 'my-c-mode-auto-pair)

;;C模式策略
(add-hook 'c-mode-hook 'c++-mode)
;;(add-hook 'c-mode-hook
;;    '(lambda ()
;;       (require 'xcscope)
;;       (require 'ecb-autoloads)
;;       (c-set-style "Stroustrup")
;;       (c-toggle-auto-state)   ;;自动开始新行
;;       (c-toggle-auto-hungry-state))) ;;饥饿的删除键

;;cc模式策略
(add-hook 'cc-mode-hook 'c++-mode)
;;(add-hook 'cc-mode-hook
;;    '(lambda ()
;;       (require 'xcscope)
;;       (require 'ecb-autoloads)
;;       (c-set-style "Stroustrup")	      ;;采用GNU缩进风格
;;       (c-toggle-auto-state)  ;;自动开始新行
;;       (c-toggle-auto-hungry-state))) ;;饥饿的删除键

;;(hs-mirror-mode)
;;(setq mslk-c++-key (make-keymap))
;;(local-set-key "\C-i" 'mslk-c++-key)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(add-hook 'java-mode-hook (function cscope:hook)) 
;;(global-set-key "\M-i" 'dabbrev-completion)
;;(global-set-key "\M-i" 'indent-for-tab-command)
(global-set-key [(f3)] 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
 ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;;; ### JDEE ###
;;; --- Java 开发环境
(add-hook 'jde-mode-hook
          (lambda ()
            (setq jde-jdk-registry (quote (("1.6.0_05" . "/usr/lib/j2sdk1.6-sun/")))) ;版本合路径设置
            (setq jde-complete-function 'jde-complete-menu)                           ;补全方式
            (setq jde-complete-add-space-after-method t) ;在方法补全后加空格
            (setq jde-enable-abbrev-mode t)              ;加载abbrev模式
            (java-mode-indent-annotations-setup)         ;缩进注释模式
            ))

;;; ### Xrefactory ###
;;; --- Java & C 重构环境
(defvar xref-current-project nil)       ;设定当前的工程
(defvar xref-key-binding 'none)         ;设定当前的按键邦定
(setq exec-path (cons (expand-file-name "~/MyEmacs/Site-Lisp/Packages/xref") exec-path))
(setq load-path (cons (expand-file-name "~/MyEmacs/Site-Lisp/Packages/xref/emacs") load-path))
(load "xrefactory")
(setq xref-auto-update-tags-before-push t)                  ;自动刷新Tags
(setq xref-completion-inserts-parenthesis t)                ;自动插入圆括号
(setq xref-save-files-and-update-tags-after-refactoring t)  ;重构后自动刷新Tags
(setq xref-save-files-and-update-tags-before-refactoring t) ;重构前自动刷新Tags
(setq xref-files-encoding 'euc-cn)                          ;设置文件编码, 支持中文

(global-set-key [f1] 'shell)
(global-set-key [f2] 'gdb)
(load-library "hideshow")
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(global-set-key "\C-\\" 'hs-show-all)
(global-set-key "\C-c\C-\\" 'hs-hide-all)


;;gdb调试配置
(add-to-list 'load-path "~/.emacs.d/gdb")
(require 'gdb-settings)
(global-set-key [s-f3] 'gdb-frame-assembler-buffer)      ;;show assembler

;; (add-hook 'gdb-mode-hook '(lambda ()
;;         (gud-tooltip-mode 1)
;;         (gdb-many-windows t)
;;         ;;(tabbar-mode nil)
;;         (tool-bar-mode t)
;;         (fullscreen)
;;         (menu-bar-mode nil)
;;         (gdb-show-changed-values t)
;;     )
;; )
;; 
;; (setq gdb-many-windows t)
;; ;;(load-library "multi-gud.el")
;; (load-library "multi-gdb-ui.el")
;; (defun gdb-run ()
;; "If gdb isn't running; run gdb, else call gud-go."                         
;;  (interactive) 
;;     (sr-speedbar-close)
;;     (gdb (gud-query-cmdline 'gdba))
;; )
;; (defun gdb-or-gud-go2 ()                                                      
;;   "If gdb isn't running; run gdb, else call gud-go."                         
;;   (interactive)    
;;   ;(sr-speedbar-close)                                                          
;;   (if (and gud-comint-buffer                                                 
;;            (buffer-name gud-comint-buffer)                                   
;;            (get-buffer-process gud-comint-buffer)                            
;;            (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba)))
;;       (gud-call (if gdb-active-process "continue" "run") "") ;;then part                
;;     ;;else part
;;     (gdb-run)
;;   );;end:if
;; ) 

;; (defun gdb-or-gud-go ()                                                      
;;   "If gdb isn't running; run gdb, else call gud-go."                         
;;   (interactive)                                                       
;;   (if (and gud-comint-buffer                                                 
;;            (buffer-name gud-comint-buffer)                                   
;;            (get-buffer-process gud-comint-buffer)                            
;;            (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba))) ;;if part
;;       ;;then part 
;;       (funcall (lambda ()
;;             (gud-call (if gdb-active-process "continue" "run") "")
;;             (speedbar-update-contents)
;;         )
;;       )           
;;     ;;else part
;;     (funcall(lambda ()
;;         (sr-speedbar-close)
;;         (gdb (gud-query-cmdline 'gdba)))
;;     )
;;   );;end:if
;; )      
;; (defun gud-break-remove ()                                                   
;;   "Set/clear breakpoin."                                                     
;;   (interactive)                                                              
;;   (save-excursion                                                            
;;     (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)               
;;         (gud-remove nil)                                                     
;;       (gud-break nil))))     
                                                      
;; (defun gud-kill ()                                                           
;;   "Kill gdb process."                                                        
;;   (interactive)                                                              
;;   (with-current-buffer gud-comint-buffer (comint-skip-input))                
;;   (kill-process (get-buffer-process gud-comint-buffer))) 

;; (defun gdb-key-settings ()                                                      
;;     "If gdb isn't running; run gdb, else call gud-go."                         
;;     (interactive)
;;         (global-set-key [f5] 'gdb-or-gud-go)    ;;F5:
;;         (global-set-key [S-f5] 'gud-kill)       ;;shift+f5
;;         (global-set-key [(f8)] 'gud-until)      ;;F8 
;;         (global-set-key "\C-cw" 'gud-watch)  ;;watch
;;         (global-set-key "\C-xc" 'gud-break-remove) ;;break
;;         (global-set-key [f4] 'gud-next)        ;;next
;;         (global-set-key [f3] 'gud-step)        ;;step
;;         (global-set-key [S-f12] 'gud-finish)    ;;shift+f12
;;         (global-set-key [(f12)] 'speedbar-update-contents)      ;;F12
;; 	(global-set-key [s-f3] 'gdb-frame-assembler-buffer)      ;;F12
	    
;; )
;; (add-hook 'c-mode-common-hook 'gdb-key-settings)
;; (add-hook 'go-mode-hook 'gdb-key-settings)
;; (provide 'gdb-settings)


;; (global-set-key [(f6)] 'gdb)
;; (add-hook 'gdb-mode-hook '(lambda ()
;;     (setq gud-mode t)
;;     (setq gud-tooltip-mode t)
;;     (global-set-key [(f3)] 'gud-step)
;;     (global-set-key [(f4)] 'gud-next)
;;     (global-set-key [(f5)] 'gud-go)
;;     (global-set-key "\M- " 'gdb-toggle-breakpoint)))
;; (add-hook 'gud-mode-hook gdb-mode)

(setq split-height-threshold 0)
(setq split-width-threshold nil)
;;编译
;; F9 调用 compile 并设置编译命令
;; C-F9 则保存所有文件并编译(无提示)
(defun defineCompileCmd()
  (interactive)
;;   (setq boost_path 
;; 	)
;; ;;"E:/workspace/C++/lib/boost/boost1.46")
;;   (setq ace_src_path 
;; 	)
;; ;;"E:/workspace/C++/lib/ace/ace6.0.4/ACE_wrappers")
;;   (setq ace_include_path 
;; 	)
;; ;;"E:/workspace/C++/lib/ace/ace6.0.4/ace/include")
;;   (setq ace_mingw_lib_d 
;; 	)
;; ;;"E:/workspace/C++/lib/ace/ace6.0.4/ace/lib_gcc/lib_d")
   (setq addition_include_path 
 	)
   (setq addition_lib 
 	)
;; ;;"E:/workspace/C++/lib/ace/ace6.0.4/ace/lib_gcc/lib")
  (setq compile_file_name (substring (buffer-name (current-buffer)) 0 (string-match "[.]"  (buffer-name (current-buffer)))))
;;  (setq compile_file_name (concat compile_file_name ""))
  (setq compile-command 
(concat  "gcc -g "
" utils.o "
" stack.o "
;;   addition_include_path
;;   boost_path
;; " -I"
;; ace_src_path
;; " -I"
;; ace_include_path
;; " -L"
;; ace_mingw_lib
;; " -lACE " 

(buffer-name (current-buffer))
" -o "
compile_file_name
;; " && "
;; compile_file_name
)))

(defun shrink-compile-window()
  "shrink compile window, avoid compile window occupy 1/2 hight of whole window"
  (interactive)
  ;;(select-window (get-buffer-window "*compilation*"))
  (setq compiled_buffer_name (buffer-name (current-buffer)))
  (switch-to-buffer-other-window "*compilation*")
  (if (< (/ (frame-height) 3) (window-height))
      (shrink-window (/ (window-height) 2)))
  (switch-to-buffer-other-window compiled_buffer_name)
  )
;;C-F9保存当前所有未保存的buffer并编译当前buffer
(global-set-key [C-f9] '(lambda()
 "Save buffers and start compile"
 (interactive)
 (save-some-buffers t)
 (defineCompileCmd)
 (compile compile-command)
 (shrink-compile-window)
 )
) 
;;F9调用compile编译当前buffer
(global-set-key [f9] '(lambda ()
(interactive)
(defineCompileCmd)
(compile compile-command)
(shrink-compile-window)
))

(custom-set-faces
 '(default ((t (:stipple nil :background "black" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant italic :weight normal :height 128 :width normal :foundry "microsoft" :family "Consolas")))))

;;选择区域的前／背景
(set-face-foreground 'region "grey1")
(set-face-background 'region "wheat")
;;二次选择区域的前／背景
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")
;;(set-default-font "Courier New-13")

;; ;; 设置字体的函数
;; (defun my-default-font ()
;;   (interactive)
;; ;;  (set-default-font "Courier Std-11")
;; ;;  (set-fontset-font (frame-parameter nil "font)
;; ;;                    "han "("AR PL UMing CN"."unicode-bmp"))
;; )

;; ;; ;; 有关界面和字体的配置
;; ;; (add-hook "after-make-frame-functions
;; ;;           (lambda (new-frame)
;; ;;             (select-frame new-frame)
;; ;;             (tool-bar-mode 0)
;; ;;             (scroll-bar-mode 0)
;; ;;             (my-default-font)

;; ;; ))