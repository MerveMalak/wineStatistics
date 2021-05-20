#lang scheme
(require 2htdp/batch-io)
(define file (read-words/line "wine.data.txt"))
(define (split-list lst) ;Thirteen data of a wine are kept as a single string. Returns a list with thirteen data separated
  (if (null? lst)'()
      (cons (string-split (car(car lst)) ",") (split-list (cdr lst)))))
(define (string-to-number lst) ;Converts string data to numeric value
  (if (null? lst)'()
      (cons (string->number  (car lst)) (string-to-number (cdr lst)))))
(define (wine-list-edit lst) ; returns edited wine list
  (if (null? lst)'()
      (cons (string-to-number (car lst)) (wine-list-edit (cdr lst)))))
(define wine-list (wine-list-edit (split-list file))) ;wine-list 
(define attribute-names '("Alcohol" "Malic acid" "Ash" "Alcalinity of ash"  ; attribute names is assigned to the assigment-names
                          "Magnesium" "Total phenols" "Flavanoids"
                          "Nonflavanoid phenols" "Proanthoyanins" "Color intensity"
                          "Hue" "OD280/OD315 of diluted wines" "Proline"))
(define position ;returns element at the specific index
  (lambda (lst n count)
    (if (= count n)
        (car lst)
        (position (cdr lst) n (+ count 1)))))
(define (attribute-list lst n) ;retrieves the specified attribute's value in each wine returns a list
  (if (null? lst) '()
      (cons (position (car lst) n 0) (attribute-list (cdr lst) n ))))
(define (mean lst sum total) ;returns mean of the attribute
  (if (null? lst)
      (exact->inexact (/ sum total))
      (mean (cdr lst) (+ sum (car lst)) (+ 1 total))))
(define (median lst) ;returns median of the attribute
  (let ((sort-lst (sort lst <)) ;the list must be sorted
        (len (length lst)))
    (if (even? len)
        (exact->inexact (/ (+ (position sort-lst (/ len 2) 1) (position sort-lst (+ (/ len 2) 1) 1)) 2))
        (exact->inexact (position sort-lst (/ (+ len 1) 2) 1)))))
(define (x-mean-square n mean-lst) ;returns square of x-mean
  (sqr (- n mean-lst)))
(define (variance lst sum total mean-lst) ;returns variance of the attribute
  (if (null? lst)
      (exact->inexact (/ sum total))
      (variance (cdr lst) (+ sum (x-mean-square (car lst) mean-lst)) (+ total 1) mean-lst)))
(define first-n-element ;Returns the list elements from the first to the specified element as a list.
  (lambda (n lst)
    (if (= n 0)
        '()
        (cons (car lst) (first-n-element (- n 1) (cdr lst))))))
(define interleave 
  (lambda (lst1 lst2)
    (if (null? lst1) lst2
        (cons (car lst1) (interleave lst2 (cdr lst1))))))
(define shuffle ;returns shuffle list
  (lambda (lst)
    (let ((half (quotient (+ (length lst) 1) 2)))
      (interleave (first-n-element half lst)
                  (list-tail lst half)))))
(define (percent-of-list n lst) ;Calculates the specified percentage
  (inexact->exact (round (* n (length lst)))))
(define train-set (first-n-element  (percent-of-list 0.8 wine-list) (shuffle wine-list))) ;train set
(define test-set (list-tail (shuffle wine-list) (percent-of-list 0.8 wine-list))) ;test set

(define accuracy? ;checks for accuracy
  (lambda (n)
    (if (= (+ (random 3) 1) n) #t
        #f)))
(define (total-accuracy lst sum total) ;return total accuracy / number of test set
  (cond ((null? lst) (exact->inexact (/ sum total)))
        (else
         (if (accuracy? (position (car lst) 1 1))
             (total-accuracy (cdr lst) (+ sum 1) (+ total 1))
             (total-accuracy (cdr lst) sum (+ total 1))))))

(define (print-attribute lst1 lst2 n count) ;Prints the mean median and variance values of each attribute
  (cond ((= n count) (newline))
        (else
       (print (car lst1)) (newline)
                           (print 'mean-->)
                           (print (mean (attribute-list lst2 count) 0 0))
                           (newline)
                           (print 'median-->)
                           (print (median (attribute-list lst2 count)))
                           (newline)
                           (print 'variance-->)
                           (print (variance (attribute-list lst2 count) 0 0 (mean (attribute-list lst2 count) 0 0)))
                           (newline)
                           (print-attribute (cdr lst1) lst2 n (+ count 1)))))
(define (program)
       (print-attribute attribute-names wine-list 14 1)
       (print "the total accuracy by comparing the results with the ground-truth labels: ")
       (print (total-accuracy test-set 0 0)))
(program)

                         


