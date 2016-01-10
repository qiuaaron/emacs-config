;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define the file header of verilog source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-verilog-header ()
  "Insert a standard Verilog file header."
  (interactive)
  (let ((start (point)))
  (insert "\
////////////////////////////////////////////////////////////////////////////////
//     普源精电科技有限公司版权所有(2004-2010) 
////////////////////////////////////////////////////////////////////////////////
//  源文件名:  <filename>
//  功能描述:  
//  作   者 : <AUTHOR>
//  版   本 : 1.0
//  完成日期: <credate>
//  修改历史: 历史修改记录      
//  <作者>    <修改时间>      <版本>        <修改描述> 
//  <AUTHOR>     <moddate>          1.0               创建此版本
////////////////////////////////////////////////////////////////////////////////
")
  (goto-char start)
  (search-forward "<filename>")
  (replace-match (buffer-name) t t)
  (search-forward "<author>") (replace-match "" t t)
  (insert "邱小勇")
  (insert "  <" "qiuxiaoyong" "@" "rigol.com" ">")
  (search-forward "<credate>") (replace-match "" t t)
  (verilog-insert-date)
  (search-forward "<author>") (replace-match "" t t)
  (insert "邱小勇")
  (search-forward "<moddate>") (replace-match "" t t)
  (verilog-insert-date)
  (search-forward "<copydate>") (replace-match "" t t)
  (verilog-insert-year)
    (search-forward "<modhist>") (replace-match "" t t)
    (verilog-insert-date)
    (insert " : created")
    (goto-char start)
    (let (string)
      (setq string (read-string "title: "))
      (search-forward "<title>")
      (replace-match string t t)
      (setq string (read-string "project: " verilog-project))
      (setq verilog-project string)
      (search-forward "<project>")
      (replace-match string t t)
      ;(setq string (read-string "Company: " verilog-company))
      (setq string "Rigol Ltd" )
      (setq verilog-company string)
      (search-forward "<company>")
      (replace-match string t t)
      (search-forward "<company>")
      (replace-match string t t)
      (search-forward "<company>")
      (replace-match string t t)
      (search-backward "<description>")
      (replace-match "" t t)
      )
    )
  )



