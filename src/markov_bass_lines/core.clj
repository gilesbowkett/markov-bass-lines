(ns markov-bass-lines.core)

(def transcribed-bass-lines (list '(on off on tie on off on tie)
                                  '(on tie off on tie off on on)))

(defn trigrams [bassline]
  (if (> 3 (count bassline))
    ()
    (cons (list (list (first bassline)
                      (second bassline))
                (nth bassline 2))
          (trigrams (rest bassline)))))

(defn -main []
  (println (first transcribed-bass-lines))
  (println (trigrams (first transcribed-bass-lines)))
  (println (second transcribed-bass-lines))
  (println (trigrams (second transcribed-bass-lines))))

