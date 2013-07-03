(ns markov-bass-lines.core)

(def transcribed-bass-lines (list '(on off on tie on off on tie)
                                  '(on tie off on tie off on on)))

(def trigram (take 3 (first transcribed-bass-lines)))
(def bi-gram (take 2 trigram))
(def next-note (nth trigram 2))
(def bi-gram-next-note {bi-gram next-note})

(defn -main []
  (println (bi-gram-next-note '(on off))))

