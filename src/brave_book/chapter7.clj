(ns chapter7)

; Use the list function, quoting, and read-string to create a list that, when 
; evaluated, prints your first name and your favorite sci-fi movie.

(eval (read-string "(list 'Roelof  'Star 'Wars)"))

; Create an infix function that takes a list like (1 + 3 * 4 - 5) and transforms 
; it into the lists that Clojure needs in order to correctly evaluate the 
; expression using operator precedence rules.

(defn infix [expr]
  (let [first (first expr)
        second (nth expr 1)
        thirth (nth expr 2)
        fourth (nth expr 3)
        fifth (nth expr 4)
        sixth (nth expr 5)
        seventh (nth expr 6)]
    (list sixth (list second (list fourth thirth fifth) first) seventh)))

(def priorities {'+ 1, '- 1, '* 2, '/ 2})

(defn infix2 [[first-number operator second-number & r]]
  (if (empty? r)
    (list operator
          (if (list? first-number)
            (infix2 first-number)
            first-number)
          (if (list? second-number)
            (infix2 second-number)
            second-number))
    (if (< (get priorities operator) (get priorities (first r)))
      (list operator first-number (infix2 (conj r second-number)))
      (infix2 (conj r (list operator first-number second-number))))))



(infix2 '(10 + 2 * 3))

(infix2 '(2 * 3 + 10))

(infix2 '(3 * (2 + (5 - 3) / 2)))


; (* 3 (2 + (5 - 3) / 2))
; (* 3 (+ 2 (/ (- 5 3) 2)))