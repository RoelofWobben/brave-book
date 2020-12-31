(ns chapter3)

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn my-reduce
  ([f initial coll]
   (loop [result initial
          remaining coll]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (my-reduce f head tail)))

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
     (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(hit asym-hobbit-body-parts)


; Write a function that takes a number and adds 100 to it.

(defn add-100 [n]
  (+ n 100))

; Write a function, dec-maker, that works exactly like the function inc-maker
; except with subtraction:

(defn dec-maker
  "Create a custom decrementor"
  [dec-by]
  #(- % dec-by))

(def dec9 (dec-maker 9))

(dec9 10)

; Write a function, mapset, that works like map except the return value is a set:

(defn mapset [f coll] (set (map f coll)))

; Create a function that’s similar to symmetrize-body-parts except that it has
; to work with weird space aliens with radial symmetry. Instead of two eyes, 
; arms, legs, and so on, they have five.

(defn my-better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (take 5 (repeat [part (matching-part part)]) )))
          []
          asym-body-parts))

; Create a function that generalizes symmetrize-body-parts and the function you 
; created in Exercise 5. The new function should take a collection of body parts
; and the number of matching body parts to add

(defn generalized-better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts num]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (take num (repeat [part (matching-part part)]) )))
          []
          asym-body-parts))

