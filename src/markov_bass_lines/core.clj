(ns markov-bass-lines.core)

(def transcribed-bass-lines (list '(on off on tie on off on tie)
                                  '(on tie off on tie off on on)))

(defn trigrams [bassline]
  (if (> 3 (count bassline))
    ()
    (cons (hash-map (str (first bassline) " "
                         (second bassline))
                (nth bassline 2))
          (trigrams (rest bassline)))))

(defn sum-merge [a b]
  (+ a b))

(defn -main []
  (let [first-freq (frequencies (trigrams (first transcribed-bass-lines)))
        second-freq (frequencies (trigrams (second transcribed-bass-lines)))]
    (merge-with sum-merge first-freq second-freq)))


; (all-trigrams transcribed-bass-lines)

; epic win
; markov-bass-lines.core=> (frequencies '(("on off" "on") ("on tie" "off") ("on off" "on")))
; {("on off" "on") 2, ("on tie" "off") 1}

