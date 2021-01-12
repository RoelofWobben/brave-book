(ns chapter4)

(def filename "suspects.csv")

(slurp filename)

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))


; Turn the result of your glitter filter into a list of names.

(defn glitter-filter->names [minimun-glitter records]
  (map :name (glitter-filter minimun-glitter records)))

; Write a function, append, which will append a new suspect to your list of 
; suspects.

 (defn append[suspect record]
   conj suspect record)

; Write a function, validate, which will check that :name and :glitter-index are
; present when you append. The validate function should accept two arguments: 
; a map of keywords to validating functions, similar to conversions, and the
; record to be validated.

(defn validate
  [validators suspect]
    (every?  #((get validators %) (get suspect %)) (keys validators)))

(defn validate2
  [validators suspect]
  (reduce-kv (fn[acc k v]
               (and acc                        ;; The previous keys were validated
                    (contains? suspect k)  ;; the suspect contains the key in question
                    (v (suspect k))))            ;; the value of that key in the suspect validates according to the validator function
             true validators)) 

 
(defn validate-name [name]
   (and (string? name) (> (count name) 3)))

(defn validate-glitter[index]
  (and (number? index)) (< index 20))

(validate2 {:name validate-name, :glitter-index validate-glitter } {:name "Roelof" :glitter-index 10})

; Write a function that will take your list of maps and convert it back to a CSV
; string. You’ll need to use the clojure.string/join function. 


(defn maps->csv [coll-of-maps]
  (let [header (-> coll-of-maps first keys)                               ;; we take the keys to use as a csv header 
        to-vals (apply juxt header)                                       ;; a function that will extract the map values in the same order as header
        lines (map (fn[m] (->> m                                          ;; we take m
                              to-vals                                     ;; extract the values
                              (clojure.string/join ", "))) coll-of-maps)  ;; and join them as strings
        header-string (->> header (map name) (clojure.string/join ", "))] ;; header string
    (str header-string "\n" (clojure.string/join "\n" lines))))           ;; the whole thing we want

(maps->csv [{:name "Edward" :glitter-index 10} {:name "Bella" :glitter-index 0}])
