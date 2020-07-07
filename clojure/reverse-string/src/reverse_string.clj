(ns reverse-string
  ; (:require [clojure.string :as str])
  )

; This is the easy/cheating way
; (defn reverse-string [s] ;; <- arglist goes here
;   (str/reverse s)
; )

; This is the real way :p
(defn reverse-string [s]
  (loop [content s 
         reversed ""]
    (let [[head & tails] content]
      (if (= tails nil)
        (str head reversed)
        (recur tails (str head reversed))
        )
      )
    )
  )
