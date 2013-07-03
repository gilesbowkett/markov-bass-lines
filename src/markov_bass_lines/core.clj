(ns markov-bass-lines.core)

(def transcribed-bass-lines (list '(on off on tie on off on tie)
                                  '(on tie off on tie off on on)))

(defn trigrams [bassline]
  (if (> 3 (count bassline))
    ()
    (cons {(list (first bassline)
                     (second bassline))
                (nth bassline 2)}
          (trigrams (rest bassline)))))

(defn -main []
  (map trigrams transcribed-bass-lines))

; epic win
; markov-bass-lines.core=> (frequencies '(("on off" "on") ("on tie" "off") ("on off" "on")))
; {("on off" "on") 2, ("on tie" "off") 1}

