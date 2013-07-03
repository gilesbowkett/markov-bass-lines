(ns markov-bass-lines.core)

(def transcribed-bass-lines (list '(on off on tie on off on tie)
                                  '(on tie off on tie off on on)))

(defn tri-gram [bass-lines] (take 3 (first bass-lines)))

(def bi-gram (take 2 (tri-gram transcribed-bass-lines)))
(def next-note (nth (tri-gram transcribed-bass-lines) 2))
(def bi-gram-next-note {bi-gram next-note})

(defn -main []
  (println (bi-gram-next-note '(on off))))

