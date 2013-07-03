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

(defn get-trigram [bass-line]
  (println bass-line)
  (println (trigrams bass-line)))

(defn -main []
  (map get-trigram transcribed-bass-lines))

