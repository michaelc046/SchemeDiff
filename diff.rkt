;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname diff) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))

; ------------------------------------------------------------------------------
; Simplified "diff" in Scheme (line-based)
;
; This program emulates the core idea of the Unix diff utility, but only with:
;   - 'd  (delete) : delete a line from the first text
;   - 'a  (add)    : add a line from the second text
; (no 'c change operation: it is represented as a sequence of deletes + adds)
;
; INPUT:
;   A text is represented as a list of strings, one string per line.
;
; OUTPUT:
;   A list of differences. Each difference is a 4-element list:
;     (list index1 action index2 line-string)
;   where:
;     index1 = position in the first text (1-based when referring to a deleted line)
;     action = 'd or 'a
;     index2 = position in the second text (1-based when referring to an inserted line)
;     line-string = the affected line

; Index convention:
; i = number of lines already consumed from text1
; j = number of lines already consumed from text2
; Therefore, the current line in text1 has index (+ i 1),
; and the current line in text2 has index (+ j 1).

; CASE 1: both texts are finished -> no more differences
; CASE 2: t1 is empty, t2 still has lines -> ADD all remaining lines of t2 
; CASE 3: t2 empty, t1 still has lines -> DELETE all remaining lines of t1
; CASE 4: current lines match -> no action, consume both lines
; CASE 5: current lines differ -> two possible choices:
;   (A) delete current line from t1
;   (B) add current line from t2
;Ir compute both candidate edit scripts and choose the shorter one (fewer ops).     
(define diff
  (lambda (t1 t2)
    (diff-rec t1 0 t2 0)
    ))

; PARAMETERS:
;   t1 : remaining lines of text1 (list of strings)
;   i  : number of lines already consumed from original text1
;   t2 : remaining lines of text2 (list of strings)
;   j  : number of lines already consumed from original text2

(define diff-rec ; val: list 
  (lambda (t1 i t2 j) ;t1,t2: list of strings i,j: index
    (cond    
      ((and (null? t1) (null? t2)) '() ) ;CASE 1
      (null? t1) (cons (list i 'a (+ j 1) (car t2)) (diff-rec t1 i (cdr t2) (+ j 1)))) ;CASE 2
      (null? t2) (cons (list (+ i 1) 'd j (car t1)) (diff-rec (cdr t1) (+ i 1) t2 j))) ;CASE 3
      ((string=? (car t1) (car t2)) (diff-rec (cdr t1) (+ i 1) (cdr t2) (+ j 1))) ;CASE 4
      (else (shorter+ 
             (cons (list (+ i 1) 'd j (car t1)) ;CASE 5A
                   (diff-rec (cdr t1) (+ i 1) t2 j)) 
             (cons (list i 'a (+ j 1) (car t2)) ;CASE 5B
                   (diff-rec t1 i (cdr t2) (+ j 1))) ))
      )
    ))

;shorter+: selects the "best" diff script between two candidates.
(define shorter+ ;val: list
  (lambda (t1 t2) ;u v : string
    (let ( (m (length t1)) (n (length t2)) )
      (cond ( (> m n) t2) ; m < n?
            ( (> n m) t1) ; n < m?
            (else t1)
            )
      )
    ))



(diff

 (list
  ""
  "This program computes the longest common subsequence."
  "The algorithm is recursive."
  "It compares characters one by one."
  "The result is returned as a string."
  "End of the program.")
 (list
  ""
  "This program computes the longest common subsequence."
  "The algorithm is recursive."
  "It compares strings line by line."
  "The result is returned as a list."
  "End of the program."))


#|
(diff

 (list  ; lcs_v1
  ""
  ";; Longest Common Subsequence (LCS)"
  ";; Algoritmo ricorsivo"
  ""
  "(define lcs      ; val:  stringa"
  "  (lambda (u v)  ; u, v: stringhe"
  "    (cond ((or (= (string-length u) 0) (= (string-length v) 0))"
  "           (string ))  ; stringa vuota"
  "          ((char=? (string-ref u 0) (string-ref v 0))"
  "           (string-append"
  "---            (string (string-ref u 0)) (lcs (substring u 1) (substring v 1))))"
  "          (else"
  "           (longer (lcs (substring u 1) v) (lcs u (substring v 1))))"
  "          )))"
  ""
  "(define longer   ; val:  stringa"
  "  (lambda (u v)  ; u, v: stringhe"
  "    (let ((m (string-length u)) (n (string-length v)))"
  "      (if (< m n)"
  "          v"
  "          u))"
  "    ))"
  ""
  )     ; lcs_v1

 (list  ; lcs_v2
  ""
  ";; Longest Common Subsequence (LCS)"
  ";; Algoritmo ricorsivo"
  ""
  "(define lcs      ; val:  stringa"
  "  (lambda (u v)  ; u, v: stringhe"
  "    (cond ((or (= (string-length u) 0) (= (string-length v) 0))"
  "           (string ))  ; stringa vuota"
  "          ((char=? (string-ref u 0) (string-ref v 0))"
  "           (string-append"
  " ---           (substring u 0 1) (lcs (substring u 1) (substring v 1))))"
  "          (else"
  "           (longer (lcs (substring u 1) v) (lcs u (substring v 1))))"
  "          )))"
  ""
  ";;  Stringa piu' lunga"
  ""
  "(define longer   ; val:  stringa"
  "  (lambda (u v)  ; u, v: stringhe"
  "    (let ((m (string-length u)) (n (string-length v)))"
  "      (cond ((< m n) v)"
  "            ((> m n) u)"
  "            ((= (random 2) 0) v)"
  "            (else u)))"
  "    ))"
  ""
  )     ; lcs_v2

 )  ; diff|#