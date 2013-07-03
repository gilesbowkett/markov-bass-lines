(ns markov-bass-lines.core)

(def trigram (take 3 (seq '(on off on tie on off on tie))))
(def bi-gram (take 2 trigram))
(def next-note (nth trigram 2))
(def bi-gram-next-note {bi-gram next-note})

(defn -main []
  (println (bi-gram-next-note '(on off))))

