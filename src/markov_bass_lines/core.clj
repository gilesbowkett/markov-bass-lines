(ns markov-bass-lines.core)

(def transcribed-bass-lines (list '(on off on tie on off on tie)
                                  '(on tie off on tie off on on)))

; much shorter version from https://gist.github.com/gilesbowkett/5915632
(defn freqs []
  (apply merge-with + (map (comp frequencies (partial partition 3 1)) transcribed-bass-lines)))

; ???

(defn -main []
  (freqs))


; markov-bass-lines.core=> (frequencies '(("on off" "on") ("on tie" "off") ("on off" "on")))
; {("on off" "on") 2, ("on tie" "off") 1}

