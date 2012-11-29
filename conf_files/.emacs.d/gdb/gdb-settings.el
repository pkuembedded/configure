;;------------------------------------------------------------------------------
;;GUD设置
;;------------------------------------------------------------------------------ 
(add-hook 'gdb-mode-hook '(lambda ()
							(gud-tooltip-mode 1)
							(gdb-many-windows t)
							;;(gdb-use-separate-io-buffer 1)
							;;(gdb-display-assembler-buffer 1)
							(gdb-show-changed-values t)
							;;(tabbar-mode nil)
							;;(tool-bar-mode t)
							;;(fullscreen)
							;;(menu-bar-mode nil)
							))

;;(add-hook 'gud-mode-hook 'gdb-mode)

;;------------------------------------------------------------------------------
;;GUD快捷键设置
;;------------------------------------------------------------------------------
(require 'gud) ;;(require 'gdb-mi) ;

;;(setq gdb-many-windows t)
;;(setq gud-tooltip-mode 1)
;;(setq gdb-show-changed-values t)

;;(setq need-restory-nextide-panel (sr-speedbar-exist-p)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
;;开始调试/继续     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(defun gud-self-run-or-go ()                                                      
  "If gdb isn't running; run gdb, else call gud-go."                         
  (interactive)                                                       
  (if (and gud-comint-buffer                                                 
		   (buffer-name gud-comint-buffer)                                   
		   (get-buffer-process gud-comint-buffer)                         
		   (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdbmi)))
	  
	  (funcall (lambda ()
				 (gud-call (if gdb-active-process "continue" "run") "") ))           
    
    (funcall(lambda ()
;;			  (setq need-restory-nextide-panel (sr-speedbar-exist-p))
;;			  (sr-speedbar-close)
			  (gdb (gud-query-cmdline 'gdb)) ))))

(defun debug-go ()
;;  (sr-speedbar-close)
  ;;(ecb-hide-ecb-windows)
  (gdb (gud-query-cmdline 'gdb))  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
;;结束调试 ,并且当speedbar切换到files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                          
(defun gud-self-kill ()                                                           
  "Kill gdb process." 
  (interactive)
  
  (with-current-buffer gud-comint-buffer (comint-skip-input))                
  (kill-process (get-buffer-process gud-comint-buffer))
  
  (sleep-for 2)
;;  (sr-speedbar-close)
  (kill-buffer "*Buffer List*")
  (kill-buffer (buffer-name gud-comint-buffer))
  (delete-other-windows)
  (if need-restory-nextide-panel (sr-speedbar-open))
;;  (speedbar-change-initial-expansion-list "files")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
;;断点开关       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                        
(defun debug-break-toggle ()                                                   
  "Set/clear breakpoin."                                                     
  (interactive)                                                              
  (save-excursion                                                            
    (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint) 
    	(gud-remove nil)
	  (gud-break nil))))     



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
;;结束调试2(有bug)     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
(defun debug-quit ()                                                           
  "finish."                                                        
  (interactive)
  (kill-buffer "*Buffer List*")
  ;;(gud-stop-subjob)  
  (gud-basic-call "quit"))      ;;(gud-finish))                                               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
;;运行到光标处      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
(defun debug-until ()                                                           
  "run."                                                        
  (interactive)                                                              
  ;;(setq line (line-number-at-pos (posn-point end)))
  (gud-call (concat "until " (number-to-string (line-number-at-pos)))))
;;  (speedbar-update-contents))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
;;查看变量
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun gud-self-watch ()
  (interactive)
;;  (sr-speedbar-open)
  (gud-watch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
;;恢复gud布局     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun gud-restore ()
  (interactive)
;;  (sr-speedbar-close)
  (gdb-restore-windows))
;;  (sr-speedbar-open))

(defun gdb-key-settings2 ()                               
  "If gdb isn't running; run gdb, else call gud-go."                         
  (interactive)
  (global-set-key (kbd "C-c `") 'gdb-restore-windows)
  (global-set-key (kbd "C-c w") 'gud-self-watch)        ;;C-x w 查看变量-watch
  (global-set-key (kbd "C-c e") 'gud-print)        ;;C-x e 输出变量-echo
  
  (global-set-key [f5] 'gud-go)    ;;F5:启动GUD/GUD-go ;;F5 开始调试/go        (global-set-key [f5] 'gud-self-run-or-go)
  (global-set-key [S-f5] 'gud-self-kill)
  ;;(global-set-key [f8] 'debug-until)      ;;F8 运行到光标处
  ;;(global-set-key [C-f8] 'gud-pstar)
  ;;(global-set-key "\C-xc" 'debug-break-toggle) ;;F9 断点开关
  (global-set-key "\M- " 'debug-break-toggle) ;;F9 断点开关
  (global-set-key [f4] 'gud-next)
  (global-set-key [C-f10] 'gud-until);;S-F10 运行到光标处
  (global-set-key [S-f10] 'gud-jump)	;;S-F10 跳转到光标处，下次从光标处开始运行
  (global-set-key [f3] 'gud-step)
  (global-set-key [C-f12] 'gud-finish)

  ;;(global-set-key [S-f11] 'debug-kill)    ;;Shift+F11
  ;;(global-set-key [f12] 'speedbar-update-contents)      ;;F12 刷新speedbar(让监视点的数据刷新为最新)    
  )

(defun gdb-key-settings-go ()
  (define-key go-mode-map (kbd "C-c `") 'gdb-restore-windows)
  (define-key go-mode-map (kbd "C-c w") 'gud-self-watch)        ;;C-x w 查看变量-watch
  (define-key go-mode-map (kbd "C-c e") 'gud-print)        ;;C-x e 输出变量-echo
  
  (define-key go-mode-map [f5] 'gud-go)    ;;F5:启动GUD/GUD-go ;;F5 开始调试/go        (global-set-key [f5] 'gud-self-run-or-go)
  (define-key go-mode-map [S-f5] 'gud-self-kill)
  ;;(define-key go-mode-map [f8] 'debug-until)      ;;F8 运行到光标处
  ;;(define-key go-mode-map [C-f8] 'gud-pstar)
  ;;(define-key go-mode-map "\C-xc" 'debug-break-toggle) ;;F9 断点开关
  (define-key go-mode-map "\M- " 'debug-break-toggle) ;;F9 断点开关
  (define-key go-mode-map [f4] 'gud-next)
  (define-key go-mode-map [C-f10] 'gud-until);;S-F10 运行到光标处
  (define-key go-mode-map [S-f10] 'gud-jump)	;;S-F10 跳转到光标处，下次从光标处开始运行
  (define-key go-mode-map [f3] 'gud-step)
  (define-key go-mode-map [C-f12] 'gud-finish)

  ;;(define-key go-mode-map [S-f11] 'debug-kill)    ;;Shift+F11
  ;;(define-key go-mode-map [f12] 'speedbar-update-contents)      ;;F12 刷新speedbar(让监视点的数据刷新为最新)    
  )

(defun gdb-key-settings-cc ()                               
  (global-set-key (kbd "C-c `") 'gud-go)
  (global-set-key (kbd "C-c w") 'gud-self-watch)        ;;C-x w 查看变量-watch
  (global-set-key (kbd "C-c e") 'gud-print)        ;;C-x e 输出变量-echo
  
  (global-set-key [f5] 'gud-go)    ;;F5:启动GUD/GUD-go ;;F5 开始调试/go        (global-set-key [f5] 'gud-self-run-or-go)
  (global-set-key [S-f5] 'gud-self-kill)
  ;;(define-key c-mode-base-map [f8] 'debug-until)      ;;F8 运行到光标处
  ;;(define-key c-mode-base-map [C-f8] 'gud-pstar)
  ;;(define-key c-mode-base-map "\C-xc" 'debug-break-toggle) ;;F9 断点开关
  (global-set-key "\M- " 'debug-break-toggle) ;;F9 断点开关
  (global-set-key [f4] 'gud-next)
  (global-set-key [C-f10] 'gud-until);;S-F10 运行到光标处
  (global-set-key [S-f10] 'gud-jump)	;;S-F10 跳转到光标处，下次从光标处开始运行
  (global-set-key [f3] 'gud-step)
  (global-set-key [C-f12] 'gud-finish)

  ;;(define-key c-mode-base-map [S-f11] 'debug-kill)    ;;Shift+F11
  ;;(define-key c-mode-base-map [f12] 'speedbar-update-contents)      ;;F12 刷新speedbar(让监视点的数据刷新为最新)    
  )

(defun gdb-key-settings-gud ()                               
  (global-set-key (kbd "C-c `") 'gdb-restore-windows)
  (global-set-key (kbd "C-c w") 'gud-self-watch)        ;;C-x w 查看变量-watch
  (global-set-key (kbd "C-c e") 'gud-print)        ;;C-x e 输出变量-echo
  
  (global-set-key [f5] 'gud-go)    ;;F5:启动GUD/GUD-go ;;F5 开始调试/go        (global-set-key [f5] 'gud-self-run-or-go)
  (global-set-key [S-f5] 'gud-self-kill)
										;(define-key gud-mode-map [f8] 'debug-until)      ;;F8 运行到光标处
										;(define-key gud-mode-map [C-f8] 'gud-pstar)
  ;;(define-key gud-mode-map "\C-xc" 'debug-break-toggle) ;;F9 断点开关
  (global-set-key "\M- " 'debug-break-toggle) ;;F9 断点开关
  (global-set-key [f4] 'gud-next)
  (global-set-key [C-f10] 'gud-until);;S-F10 运行到光标处
  (global-set-key [S-f10] 'gud-jump)	;;S-F10 跳转到光标处，下次从光标处开始运行
  (global-set-key [f3] 'gud-step)
  (global-set-key [C-f12] 'gud-finish)

  ;;(define-key gud-mode-map [S-f11] 'debug-kill)    ;;Shift+F11
  ;;(define-key gud-mode-map [f12] 'speedbar-update-contents)      ;;F12 刷新speedbar(让监视点的数据刷新为最新)    
  )
;;------------------------------------------------------------------------------
;;GUD再下列mode下将会开启
;;------------------------------------------------------------------------------
(add-hook 'c-mode-common-hook 'gdb-key-settings-cc)
(add-hook 'go-mode-hook 'gdb-key-settings-go)
(add-hook 'gud-mode-hook 'gdb-key-settings-gud)

(provide 'gdb-settings)
