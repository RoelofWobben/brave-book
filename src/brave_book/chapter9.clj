(ns chapter9)


; Write a function that takes a string as an argument and searches for it on 
; Bing and Google using the slurp function. Your function should return the HTML
; of the first page returned by the search.

(defn search-clojure-docs [search-term]
  (let [data (future (slurp  (str "https://clojuredocs.org/search?q=" search-term)))
        data-2 (future (str "https://www.ecosia.org/search?q=" search-term))]
    [@data @data-2]))


(search-clojure-docs "slurp")

; Update your function so it takes a second argument consisting of the search 
; engines to use.
(def urls {:ecosia "https://www.ecosia.org/search?q="})

(defn search-with-search-engine [search-term search-engines]
  (mapv deref (mapv  #(future (slurp  (str (get urls %) search-term)))
                     search-engines)))

(defn search-with-search-engine2 [search-term search-engines]
  (->> search-engines
       (mapv  #(future (slurp  (str (get urls %) search-term))))
       (mapv deref)))


(search-with-search-engine "slurp" [:ecosia])

; Create a new function that takes a search term and search engines as arguments,
; and returns a vector of the URLs from the first page of search results from
; each search engine

;  I could not figure out how to get urls out of the html so I skip this one 



