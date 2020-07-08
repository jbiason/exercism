(ns bob)

(defn shout?
  [x]
  (let [filtered (filter #(Character/isLetter %) x)]
    (if (empty? filtered)
      false
      (every? #(Character/isUpperCase %) filtered)
      )
    )
  )

(defn trim
  [x]
  (remove #(Character/isWhitespace %) x)
  )

(defn question?
  [x]
  (= (last (trim x)) \?)
  )

(defn silence?
  [x]
  (empty? (trim x))
  )

(defn forceful-question?
  [x]
  (and (shout? x) (question? x))
  )

(defn response-for 
  [s]
  (cond
    (silence? s) "Fine. Be that way!" 
    (forceful-question? s) "Calm down, I know what I'm doing!"
    (shout? s) "Whoa, chill out!"
    (question? s) "Sure."
    :else "Whatever."
    )
  )
