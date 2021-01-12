(ns brave-book.chapter8)

; Write the macro when-valid so that it behaves similarly to when. Here is an
;  example of calling it:

; (when-valid order-details order-details-validations
; (println "It's a success!")
; (render :success))

; When the data is valid, the println and render forms should be evaluated, and
; when-valid should return nil if the data is invalid.


(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))


(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))


(defmacro when-valid
  "Handle validation more concisely"
  [to-validate validations & body]
  `(let [errors-name# (validate ~to-validate ~validations)]
     (when (empty? errors-name#)
       ~@body)))

(def order-details
  {:name ""
   :email "mitchard.blimmons@gmail.com"})

(def order-details-validations
  {:name
   ["Please enter a name" not-empty]

   :email
   ["Please enter an email address" not-empty

    "Your email address doesn't look like an email address"
    #(or (empty? %) (re-seq #"@" %))]})


(when-valid order-details order-details-validations
            (println "Line 1")
            (println "Line 2")
            (println "Line 3"))


; You saw that and is implemented as a macro. Implement or as a macro.

(defmacro or
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (or ~@next)))))

(or
 (do (println "Hello")
     :success)
 :fail)


; In Chapter 5 you created a series of functions (c-int, c-str, c-dex) to read 
; an RPG character’s attributes. Write a macro that defines an arbitrary number 
; of attribute-retrieving functions using one macro call. Here’s how you would call it:


(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})


(defmacro defattrs
  [& assignments]
  `(do
     ~@(map
        (fn [[retr attr]] `(def ~retr (comp ~attr :attributes)))
        (partition 2 assignments))))


(macroexpand '(defattrs c-int :intelligence
                c-str :strength
                c-dex :dexterity))







