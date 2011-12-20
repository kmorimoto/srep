;;; srep.el --- the smart text repeater

;; Copyright (C) 2011 Morimoto, Ken <ken.m.pp1@gmail.com>

(defconst srep-version-number "0.1.1 (2011-12-20)"
  "srep.el version number.")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;;
;; srep - Emacs で Excel のオートフィル機能のようなことをする
;;
;; [インストール方法]
;;
;;   ロード可能なディレクトリに srep.el を置き、.emacs などで
;;   (require 'srep) としてください。
;;
;; [起動方法]
;;
;;   M-x srep で起動します。
;;
;; [リピート関数の追加方法]
;;
;;   独自にリピート関数 (アナライザとリピータ) を実装して
;;   srep-repeat-functions に登録することで、繰り返し要素を
;;   追加することができます。
;;
;;   決まった単語を繰り返すだけであれば、マクロ srep-define-word-repeater を使って
;;   簡単にリピート関数を定義できます。
;;
;;   リピート関数を追加するには以下のようにします。
;;
;;   (add-to-list 'srep-repeat-functions
;;                '(srep-ana:my-analyzer srep-rep:my-repeater))
;;
;; [リピート関数の仕様]
;;
;;   * アナライザ
;;     (srep-ana:NAME 解析対象文字列)
;;       => (繰り返し可能な部分文字列を数値化した値
;;           繰り返し可能な部分文字列
;;           繰り返し可能な部分文字列の位置
;;           解析対象文字列)
;;          あるいは nil
;;
;;     [例] (srep-ana:integer "foo_034_bar")
;;           => (34 "034" 4 "foo_034_bar")
;;
;;   * リピータ
;;     (srep-rep:NAME 繰り返しの値 入力文字列例)
;;       => 出力文字列
;;
;;     [例] (srep-rep:integer 7 "034")
;;           => "007"
;;

;;; Change Log:

;;
;; 2011-12-20  Morimoto, Ken  <ken.m.pp1@gmail.com>
;;
;;   * リリース 0.1.1
;;
;;   * srep-repeat-functions の構文エラーを修正。
;;
;;   * YYYY-MM-DD 形式のリピート関数がオーバーフローで正常動作しないバグを修正。
;;
;; 2011-12-20  Morimoto, Ken  <ken.m.pp1@gmail.com>
;;
;;   * リリース 0.1.0
;;

;;; Code:

(defvar srep-delimiter-history ()
  "History of delimiter.")

(defvar srep-times-history ()
  "History of times.")

;; 繰り返し関数群
;; アナライザとリピータのペアをリストとして保持。
;; 先頭に近いアナライザほど優先的に使用される。
(defvar srep-repeat-functions
  '((srep-ana:yyyy-mm-dd srep-rep:yyyy-mm-dd)
    (srep-ana:dayofweek-ja srep-rep:dayofweek-ja)
    (srep-ana:dayofweek-ja-short srep-rep:dayofweek-ja-short)
    (srep-ana:dayofweek-ja-short2 srep-rep:dayofweek-ja-short2)
    (srep-ana:dayofweek srep-rep:dayofweek)
    (srep-ana:dayofweek-short srep-rep:dayofweek-short)
    (srep-ana:month srep-rep:month)
    (srep-ana:month-short srep-rep:month-short)
    (srep-ana:hexadecimal srep-rep:hexadecimal)
    (srep-ana:integer srep-rep:integer))
  "Functions for analyzing and repeating string.")

;; srep 本体
(defun srep (beg end prefix)
  "Execute srep. \"srep\" is a Smart text REPeater."
  (interactive "r\nP")
  (if (and transient-mark-mode (not mark-active))
      (message "The mark is not active.")
    (let* ((delimiter (if prefix
			  (or (car srep-delimiter-history) "\n")
			(read-string "delimiter: " nil 'srep-delimiter-history "")))
	   (orgstr (buffer-substring-no-properties beg end))
	   (backward (equal (point) beg))
	   (post-delimiter (srep--post-delimiter-p orgstr delimiter backward))
	   (rep-table (srep--create-repeat-table (srep--split orgstr delimiter backward)))
	   (count 0)
	   end times-str times)
      (while (not end)
	(setq times-str (if (and prefix (> (prefix-numeric-value prefix) 0))
			    (concat (number-to-string (prefix-numeric-value prefix)) "!")
			  (srep--read-times)))
	(setq times (string-to-number times-str))
	(setq end (or (<= times 0)
		      (equal (substring times-str -1) "!"))) ; 最後に ! が付いていたらこれで終了
	(setq mark-active nil)
	(while (> times 0)
	  (unless post-delimiter
	    (insert delimiter)
	    (when backward (goto-char beg)))
	  (insert (srep--repeat rep-table count))
	  (when backward (goto-char beg))
	  (when post-delimiter
	    (insert delimiter)
	    (when backward (goto-char beg)))
	  (setq count (1+ count))
	  (setq times (1- times)))))))

;; 繰り返しの終端にデリミタがあるか
;;   str        基となる文字列
;;   delimiter  デリミタ
;;   backward   文頭に向かって繰り返すなら t
;;   => 終端にデリミタがあるかどうか。
(defun srep--post-delimiter-p (str delimiter backward)
  (let ((delimiter-len (length delimiter)))
    (when (< delimiter-len (length str))
      (if backward
	  (equal (substring str 0 delimiter-len) delimiter)
	(equal (substring str (- delimiter-len)) delimiter)))))

;; リピート回数をユーザに尋ねる
;; 結果を文字列化して返す。処理後に srep を終了する場合は最後に "!" が付いている。
(defun srep--read-times ()
  (let* ((input (read-string "How many times? (default 1, end with '!' to quit): "
			     nil 'srep-times-history ""))
	 (num (if (equal input "") 1 (truncate (string-to-number input))))
	 (result (number-to-string num)))
    (when (or (<= num 0) (equal input "")) ; 終了指定とデフォルト指定は履歴に残さない
      (setq srep-times-history (cdr srep-times-history)))
    (if (and (> (length input) 0) (equal (substring input -1) "!"))
	(concat result "!")
      result)))

;; 文字列とデリミタから繰り返しの基となる文字列群を抽出
;;   str        基となる文字列
;;   delimiter  デリミタ
;;   backward   逆方向かどうか
;;   => 文字列群のリスト。
(defun srep--split (str delimiter backward)
  (let ((elements (split-string str (regexp-quote delimiter))))
    (when backward (setq elements (reverse elements)))
    (when (and (not (equal str "")) (equal delimiter ""))
      (setq elements (cdr elements))) ; 空文字列で区切ると必ず最初に余分な空文字列の要素が付くので削除
    (let ((element-num (length elements)))
      (when (equal (nth (1- element-num) elements) "")
	(setcdr (nthcdr (- element-num 2) elements) nil))) ; 選択範囲の最後のデリミタは無視 (最後に空文字列の要素が付く)
    elements))

;; 繰り返しテーブルの作成
;;   inputs  入力文字列群のリスト
;;   => 繰り返しテーブル。srep--repeat で使用する。
;;        ((文字列 文字列 ...)
;;         (リピータ 数列関数 (数列関数のパラメータ) サンプル文字列)
;;         (文字列 文字列 ...)
;;         (リピータ 数列関数 (数列関数のパラメータ) サンプル文字列)
;;         :
;;         :)
;;      数列関数: 第 n 項の値を求める関数。
;;      数列関数のパラメータ: 初項、公差、公比など。
;;      サンプル文字列: 繰り返し基文字列のサンプル。出力書式の参考にする。
;;
;; [例]
;;   inputs: ("aaa_00_bbb_01"
;;            "ccc_01_ddd_02"
;;            "eee_02_fff_04")
;;
;;   => (("aaa_" "ccc_" "eee_")
;;       (srep-rep:integer srep--arithmetic-sequence-nth (3.0 1.0) "00")
;;       ("_bbb_" "_ddd_" "_fff_")
;;       (srep-rep:integer srep--geometric-sequence-nth (8.0 2.0) "01")
;;       ("" "" ""))
(defun srep--create-repeat-table (inputs)
  (let (table
	(funcs srep-repeat-functions)
	found)
    (while (and funcs (not found))
      (let ((strs inputs)
	    (analyzer (nth 0 (car funcs)))
	    (repeater (nth 1 (car funcs)))
	    results)
	(setq funcs (cdr funcs))
	(while strs
	  (let ((str (car strs)) result)
	    (setq strs (cdr strs))
	    (setq result (funcall analyzer str)) ; 文字列解析
	    (if result
		(setq results (cons result results))
	      (setq strs nil)
	      (setq results nil))))
	(when results
	  (setq results (reverse results)) ; cons で追加していたので反転
	  ;; アナライザを全文字列に適用できた
	  (let ((exp (srep--numerical-sequence-p (srep--make-nth-list results 0))))
	    (when exp
	      ;; 数列だった
	      (let (pres matches posts)
		(setq found t)
		(setq matches (append (list repeater) exp (list (nth 1 (car results)))))
		(while results
		  ;; マッチした部分文字列の前後をリスト化
		  (let* ((result (car results)) ; アナラアイザの戻り値
			 (matchstr (nth 1 result)) ; アナライザが認識した部分文字列
			 (matchpos (nth 2 result)) ; アナライザが認識した部分文字列の位置
			 (orgstr (nth 3 result))) ; 解析対象の文字列全体
		    (setq results (cdr results))
		    (setq pres (append pres (list (substring orgstr 0 matchpos))))
		    (setq posts (append posts (list (substring orgstr (+ matchpos (length matchstr))))))))
		;; マッチしなかった部分を再帰的に解析
		(setq table (append (srep--create-repeat-table pres)
				    (list matches)
				    (srep--create-repeat-table posts)))))))))
    (or table (list inputs))))

;; リスト群の nth をリスト化
;;   lists  リストのリスト
;;   n      何番目の要素を抽出するか
;;   => 要素を抽出して作成したリスト。
(defun srep--make-nth-list (lists n)
  (let ((tmp lists) seq)
    (while tmp
      (setq seq (cons (nth n (car tmp)) seq))
      (setq tmp (cdr tmp)))
    (reverse seq)))

;; 繰り返し
;;   table  繰り返しテーブル
;;   n      第 n 項
;;   => 第 n 項の文字列。
(defun srep--repeat (table n)
  (let ((tbl table) str)
    (while tbl
      (let ((word (car tbl)))
	(setq tbl (cdr tbl))
	(if (stringp (car word))
	    (setq str (concat str (nth (% n (length word)) word)))
	  (let ((repeater (nth 0 word))
		(index (funcall (nth 1 word) (nth 2 word) n)) ; 第 n 項の値
		(sample (nth 3 word)))
	    (setq str (concat str (funcall repeater index sample)))))))
    str))

;; 規則性のある数列かどうか
;;   seq  数値のリスト
;;   => 規則性のある数列なら数列情報。そうでないなら nil。
(defun srep--numerical-sequence-p (seq)
  (cond ((and (>= (length seq) 3) ; 3 項未満の場合は等比数列と見なさない
	      (srep--geometric-sequence-p seq)))
	((srep--arithmetic-sequence-p seq))))

;; 等差数列かどうか
;;   seq  数値のリスト
;;   => 等差数列ならば数列情報。そうでないなら nil。
;;        (数列関数 (初項 公差))
(defun srep--arithmetic-sequence-p (seq)
  (let ((len (length seq)))
    (cond ((= len 0) nil)
	  ((= len 1) (list 'srep--arithmetic-sequence-nth
			   (list (float (car seq)) 0.0)))
	  (t
	   (let ((i 2)
		 (diff (- (float (nth 1 seq)) (float (nth 0 seq)))))
	     (while (and (< i len) diff)
	       (if (/= (+ (nth (1- i) seq) diff) (nth i seq))
		   (setq diff nil)
		 (setq i (1+ i))))
	     (when diff
	       (list 'srep--arithmetic-sequence-nth
		     (list (+ (nth (1- len) seq) diff) diff))))))))

;; 等差数列の n 項の値を求める
;;   param  初項と公差のリスト
;;   n      第 n 項
;;   => 第 n 項の値。
(defun srep--arithmetic-sequence-nth (param n)
  (let ((start (nth 0 param))
	(diff (nth 1 param)))
    (+ start (* diff n))))

;; 等比数列かどうか
;;   seq  数値のリスト
;;   => 等比数列ならば数列情報。そうでないなら nil。
;;        (数列関数 (初項 公比))
(defun srep--geometric-sequence-p (seq)
  (let ((len (length seq)))
    (cond ((= len 0) nil)
	  ((= len 1) (list 'srep--geometric-sequence-nth
			   (list (float (car seq)) 1.0)))
	  (t
	   (let ((i 2)
		 (ratio (cond
			 ((= 0 (nth 1 seq)) 0.0)
			 ((= 0 (nth 0 seq)) nil)
			 (t (/ (float (nth 1 seq)) (float (nth 0 seq)))))))
	     (while (and (< i len) ratio)
	       (if (/= (* (nth (1- i) seq) ratio) (nth i seq))
		   (setq ratio nil)
		 (setq i (1+ i))))
	     (when ratio
	       (list 'srep--geometric-sequence-nth
		     (list (* (nth (1- len) seq) ratio) ratio))))))))

;; 等比数列の n 項の値を求める
;;   param  初項と公比のリスト
;;   n      第 n 項
;;   => 第 n 項の値。
(defun srep--geometric-sequence-nth (param n)
  (let ((start (nth 0 param))
	(ratio (nth 1 param)))
    (* start (expt ratio n))))

;; 決まった単語を繰り返すだけのリピート関数を定義するマクロ
;;   name       名前
;;   word-list  単語リスト
;;
;; このマクロによって、アナライザとリピータが定義される。
;; (NAME の部分は name で指定したものに置き換わる。)
;;   - アナライザ => srep-ana:NAME
;;   - リピータ   => srep-rep:NAME
(defmacro srep-define-word-repeater (name word-list)
  "Define analyzer and repeater. (\"srep-ana:NAME\" and \"srep-rep:NAME\")"
  (let* ((name-str (symbol-name name))
	 (analyzer-name (intern (concat "srep-ana:" name-str)))
	 (repeater-name (intern (concat "srep-rep:" name-str)))
	 (table-name (intern (concat "srep-tbl:" name-str))))
    `(progn
       (defconst ,table-name ,word-list)
       (defun ,analyzer-name (str)
	 (srep-ana:generic-word str ,table-name))
       (defun ,repeater-name (n sample)
	 (srep-rep:generic-word n sample ,table-name)))))

;; 決まった単語を繰り返すだけのリピート関数のアナライザ
;;   str        入力文字列
;;   word-list  単語リスト
;;   => 一般的なアナライザの戻り値と同じ。
(defun srep-ana:generic-word (str word-list)
  (let ((pos (string-match (srep--generate-word-analyzer-regexp word-list) str)))
    (when pos
      (let ((match (match-string 0 str)))
	(list (srep--word-to-index match word-list) match pos str)))))

;; 決まった単語を繰り返すだけのリピート関数のリピータ
;;   n          第 n 項
;;   sample     サンプル文字列
;;   word-list  単語リスト
;;   => 一般的なリピータの戻り値と同じ。
(defun srep-rep:generic-word (n sample word-list)
  (let ((word (nth (mod (truncate n) (length word-list)) word-list)))
    (cond ((not (string-match "[a-z]" sample)) (upcase word))
	  ((not (string-match "[A-Z]" sample)) (downcase word))
	  (t (capitalize word)))))

;; 単語リストの中から単語を探す (大文字小文字の区別なし)
;;   word       探したい単語
;;   word-list  単語リスト
;;   => 見つかった場合は要素番号、見つからなかった場合は nil。
(defun srep--word-to-index (word word-list)
  (let ((rest word-list)
	(word-d (downcase word))
	found)
    (while (and rest (not found))
      (if (equal word-d (downcase (car rest)))
	  (setq found t)
	(setq rest (cdr rest))))
    (if found
	(- (length word-list) (length rest))
      nil)))

;; 単語リストのどれかを検索する正規表現を生成
;;   word-list  単語リスト
;;   => 正規表現。
;;
;; [例]
;;   word-list: ("abc" "def" "ghi")
;;
;;   => "\\b\\(Ghi\\|GHI\\|ghi\\|Def\\|DEF\\|def\\|Abc\\|ABC\\|abc\\)\\b"
(defun srep--generate-word-analyzer-regexp (word-list)
  (let ((rest word-list) case-words)
    (while rest
      (let ((word (car rest)))
	(setq rest (cdr rest))
	(setq case-words (cons (downcase word) case-words))
	(setq case-words (cons (upcase word) case-words))
	(setq case-words (cons (capitalize word) case-words))))
    (concat "\\b\\(" (mapconcat '(lambda (x) x) case-words "\\|") "\\)\\b")))

;; リピート関数 (アナライザ): 整数
(defun srep-ana:integer (str)
  (let ((pos (string-match "[+-]?[0-9]+" str)))
    (when pos
      (let ((match (match-string 0 str)))
	(list (string-to-number match) match pos str)))))

;; リピート関数 (リピータ): 整数
(defun srep-rep:integer (n sample)
  (let* ((pos (string-match "^[+-]?\\(0.+\\)" sample))
	 (digit (if pos (length (match-string 1 sample)) 0))
	 (sign (cond ((< n 0) "-")
		     ((equal (substring sample 0 1) "+") "+")
		     (t ""))))
    (concat sign (format (format "%%0%dd" digit) (abs n)))))

;; リピート関数 (アナライザ): 16 進数
(defun srep-ana:hexadecimal (str)
  (let ((pos (string-match "\\(^\\|[^A-Za-z0-9]\\)\\(0[xX]\\([0-9A-Fa-f]+\\)\\)\\($\\|[^A-Za-z0-9]\\)" str)))
    (when pos
      (list (string-to-number (match-string 3 str) 16)
	    (match-string 2 str)
	    (match-beginning 2)
	    str))))

;; リピート関数 (リピータ): 16 進数
(defun srep-rep:hexadecimal (n sample)
  (let* ((pos (string-match "^0[xX]\\(0.+\\)" sample))
	 (digit (if pos (length (match-string 1 sample)) 0))
	 (upper (string-match "[A-F]" sample)))
    (concat (substring sample 0 2) ; "0x" or "0X"
	    (format (format "%%0%d%s" digit (if upper "X" "x")) n))))

;; リピート関数 (アナライザ): 日付 YYYY-MM-DD
(defun srep-ana:yyyy-mm-dd (str)
  (let ((pos (string-match "\\(^\\|[^0-9]\\)\\(\\(\\(19[789]\\|20[0123]\\)[0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)\\)\\($\\|[^0-9]\\)" str)))
    (when pos
      (let* ((match (match-string 2 str))
	     (year (string-to-number (match-string 3 str)))
	     (month (string-to-number (match-string 5 str)))
	     (day (string-to-number (match-string 6 str)))
	     (en-time (encode-time 0 0 0 day month year 0)))
	(list (+ (* (float (nth 0 en-time)) 65536.0) (float (nth 1 en-time))) match pos str)))))

;; リピート関数 (リピータ): 日付 YYYY-MM-DD
(defun srep-rep:yyyy-mm-dd (n sample)
  (let* ((upper (truncate (/ n 65536.0)))
	 (lower (truncate (- n (* upper 65536.0))))
	 (de-time (decode-time (list upper lower))))
    (format "%04d-%02d-%02d" (nth 5 de-time) (nth 4 de-time) (nth 3 de-time))))

;;-----------------------------------------------------------------------------
;; 単語リピート関数

(srep-define-word-repeater
 dayofweek
 '("sunday" "monday" "tuesday" "wednesday" "thursday" "friday" "saturday"))

(srep-define-word-repeater
 dayofweek-short
 '("sun" "mon" "tue" "wed" "thu" "fri" "sat"))

(srep-define-word-repeater
 month
 '("january" "february" "march" "april" "may" "june"
   "july" "august" "september" "october" "november" "december"))

(srep-define-word-repeater
 month-short
 '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

(srep-define-word-repeater
 dayofweek-ja
 '("日曜日" "月曜日" "火曜日" "水曜日" "木曜日" "金曜日" "土曜日"))

(srep-define-word-repeater
 dayofweek-ja-short
 '("日" "月" "火" "水" "木" "金" "土"))

(srep-define-word-repeater
 dayofweek-ja-short2
 '("日曜" "月曜" "火曜" "水曜" "木曜" "金曜" "土曜"))

;;-----------------------------------------------------------------------------
;; テスト

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      ;; ----------------------------------------
      (desc "srep--split")
      (expect '("")
	(srep--split "" ""))
      (expect '("a" "b" "c")
	(srep--split "abc" ""))
      (expect '("a" "b" "c")
	(srep--split "a,b,c" ","))
      (expect '("a" "b" "c")
	(srep--split "a,b,c," ","))
      (expect '("" "a" "b" "c")
	(srep--split ",a,b,c," ","))
      (expect '("a" "b" "c")
	(srep--split "a, b, c" ", "))
      ;; ----------------------------------------
      (desc "srep--make-nth-list")
      (expect '(0 1 2)
	(srep--make-nth-list '((9 0 9) (9 1 9) (9 2 9)) 1))
      ;; ----------------------------------------
      (desc "srep--arithmetic-sequence-p")
      (expect nil ; 空リスト
	(srep--arithmetic-sequence-p '()))
      (expect '(srep--arithmetic-sequence-nth (1.0 0.0)) ; 単項
	(srep--arithmetic-sequence-p '(1)))
      (expect '(srep--arithmetic-sequence-nth (3.0 1.0)) ; 2 項
	(srep--arithmetic-sequence-p '(1 2)))
      (expect '(srep--arithmetic-sequence-nth (4.0 1.0)) ; 3 項以上
	(srep--arithmetic-sequence-p '(1 2 3)))
      (expect '(srep--arithmetic-sequence-nth (0.0 -1.0)) ; 公差が負
	(srep--arithmetic-sequence-p '(3 2 1)))
      (expect nil ; 等差数列ではない
	(srep--arithmetic-sequence-p '(1 2 4)))
      ;; ----------------------------------------
      (desc "srep--geometric-sequence-p")
      (expect nil ; 空リスト
	(srep--geometric-sequence-p '()))
      (expect '(srep--geometric-sequence-nth (2.0 1.0)) ; 単項
	(srep--geometric-sequence-p '(2)))
      (expect '(srep--geometric-sequence-nth (8.0 2.0)) ; 2 項
	(srep--geometric-sequence-p '(2 4)))
      (expect '(srep--geometric-sequence-nth (16.0 2.0)) ; 3 項以上
	(srep--geometric-sequence-p '(2 4 8)))
      (expect '(srep--geometric-sequence-nth (-1.0 -1.0)) ; 公比が負
	(srep--geometric-sequence-p '(1 -1 1)))
      (expect nil ; 等比数列ではない
	(srep--geometric-sequence-p '(1 2 3)))
      ;; ----------------------------------------
      (desc "srep-ana:integer")
      (expect '(1234 "1234" 0 "1234hoge") ; 先頭に正数
	(srep-ana:integer "1234hoge"))
      (expect '(1234 "+1234" 0 "+1234hoge") ; 先頭に符号付き正数
	(srep-ana:integer "+1234hoge"))
      (expect '(-1234 "-1234" 0 "-1234hoge") ; 先頭に負数
	(srep-ana:integer "-1234hoge"))
      (expect '(1234 "1234" 4 "hoge1234hoge") ; 途中に正数
	(srep-ana:integer "hoge1234hoge"))
      (expect '(1234 "+1234" 4 "hoge+1234hoge") ; 途中に符号付き正数
	(srep-ana:integer "hoge+1234hoge"))
      (expect '(-1234 "-1234" 4 "hoge-1234hoge") ; 途中に負数
	(srep-ana:integer "hoge-1234hoge"))
      (expect '(1234 "1234" 4 "hoge1234") ; 最後に正数
	(srep-ana:integer "hoge1234"))
      (expect '(1234 "+1234" 4 "hoge+1234") ; 途中に符号付き正数
	(srep-ana:integer "hoge+1234"))
      (expect '(-1234 "-1234" 4 "hoge-1234") ; 途中に負数
	(srep-ana:integer "hoge-1234"))
      (expect nil ; 数字が含まれない
	(srep-ana:integer "hoge"))
      ;; ----------------------------------------
      (desc "srep-rep:integer")
      (expect "1234" ; 正数
	(srep-rep:integer 1234 "0"))
      (expect "-1234" ; 負数
	(srep-rep:integer -1234 "0"))
      (expect "+1234" ; 正数、サンプルに符号付き (+)
	(srep-rep:integer 1234 "+1"))
      (expect "1234" ; 正数、サンプルに符号付き (-)
	(srep-rep:integer 1234 "-1"))
      (expect "-1234" ; 負数、サンプルに符号付き (+)
	(srep-rep:integer -1234 "+1"))
      (expect "-1234" ; 負数、サンプルに符号付き (-)
	(srep-rep:integer -1234 "-1"))
      (expect "01234" ; 正数、サンプルに桁指定付き
	(srep-rep:integer 1234 "00001"))
      (expect "-01234" ; 負数、サンプルに桁指定付き
	(srep-rep:integer -1234 "00001"))
      (expect "+01234" ; 正数、サンプルに符号 (+)、桁指定付き
	(srep-rep:integer 1234 "+00001"))
      (expect "01234" ; 正数、サンプルに符号 (-)、桁指定付き
	(srep-rep:integer 1234 "-00001"))
      (expect "-01234" ; 負数、サンプルに符号 (+)、桁指定付き
	(srep-rep:integer -1234 "+00001"))
      (expect "-01234" ; 負数、サンプルに符号 (-)、桁指定付き
	(srep-rep:integer -1234 "-00001")))))

(provide 'srep)

;;; srep.el ends here
