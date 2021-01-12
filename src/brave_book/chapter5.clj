(ns chapter5)


; You used (comp :intelligence :attributes) to create a function that returns a 
; character’s intelligence. Create a new function, attr, that you can call like
;  (attr :intelligence) and that does the same thing.


(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(defn attr [attribute]
  (get (get character :attributes) attribute))

(attr :strength)

; Implement the comp function.

(defn my-comp
  ([] identity)
  ([f] f)
  ([f g]
   (fn
     ([] (f (g)))
     ([x] (f (g x))))))


; Implement the assoc-in function. Hint: use the assoc function and define 
; its parameters as [m [k & ks] v].

(defn my-assoc-in [m [k & ks] v]
  (assoc m k (my-assoc-in (get m k) ks v)))


(def users [{:name "James" :age 26}  {:name "John" :age 43}])

(assoc-in users [1 :age] 44)

; Look up and use the update-in function.

(def p {:name "James" :age 26})

(update-in p [:age] inc)

; Implement update-in.

(def m {:1 {:value 0, :active false}, :2 {:value 0, :active false}, :3 {:value 0, :active false}})


(defn my-update-in [m ks f & args]
  (let [up (fn up [m ks f args]
             (let [k (first ks)
                   r (rest ks)]
               (if (empty? r)
                 (assoc m k (apply f (get m k) args))
                 (assoc m k (up (get m k) r f args)))))]
    (up m ks f args)))


