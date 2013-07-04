(ns markov-bass-lines.core
  (:require [clojure.math.numeric-tower :as math]))

(def titles-and-basslines
  '((NTH_110_A_FunkBass
     (on tie off on on tie off off on tie on on off on off on on tie on on off on off on on on on on tie off on off))

    (NTH_110_G_NiceFunkBass
     (on off off on off off on off off off off off off off off on on off on on off off on off off off off off off off off off))

    (NTH_110_G_SlinkyBass
     (on tie on off on tie off on off on tie on on on tie off on tie on off on tie off on on on on off on tie off off))

    (NTH_120_G_DiscoFunkBass
     (on tie on tie on tie on tie on tie on on tie off on off on tie on tie on tie on tie on tie on on tie off on off))

    (NTH_120_G_SpaceDiscoBass
     (on tie off off off off off on on tie off off off off off on on on on tie off on tie off on tie off on on tie on tie))

    (NTH_125_G_SubFunkBass
     (on tie off off on tie off off on tie off off on on off on tie on on off on tie on tie on tie on on tie on tie off))

    (NTH_130_C#_ElectricDiscoBass
     (on tie on off off on off on on tie off off off off off on on tie on tie off on off on on tie off off off off off off))

    (NTH_130_C#_ElectricMenaceBass
     (on tie off on off off on off on off on off on on off on on tie off on off off on off on off on off on off on off))

    (NTH_130_C#_SmoothBass
     (on tie tie tie off off on off on tie off off off off on tie on tie on tie off off on tie on tie on tie off off off off))

    (NTH_130_G_SawFunkBass
     (on tie off off on tie off off on tie on on off on on on on tie on tie on tie off on tie off off off off off off off))))

(def basslines (map (fn [title-and-notes]
                        (first (rest title-and-notes)))
                    titles-and-basslines))

(defn n-gram-freqs
      "get n-grams of arbitrary size, and their frequencies"
      [n]
  (apply merge-with
   +
   (map (comp frequencies
              (partial partition n 1))
        basslines)))

; start on the downbeat. true for every one of the corpus basslines, and
; probably every funk bassline since the dawn of time.
(def first-note 'on)

(defn translate-frequencies-into-probabilities []
  ; the problem: get from here --> {(tie tie) 1, (tie on) 3}
  ;                    to here --> {tie {tie 0.25, on 0.75}}

  ;              and from here --> {(off on off) 22, (off on on) 16, (off on tie) 19}
  ;                    to here --> {"off on" [{off 0.39, on 0.28, tie 0.33}

  "oops")

(defn determine-probabilities [tokens-and-frequencies]
  (map (fn [t-and-f]
           (let [freq (last t-and-f)
                       tokens (first t-and-f)]
             {tokens (/ freq
                        (apply +
                               (vals tokens-and-frequencies)))}))
       tokens-and-frequencies))

(defn relevant-n-grams-only [tokens-and-frequencies target-token]
  (filter (fn [t-and-f]
            (= target-token
               (first (first t-and-f))))
          tokens-and-frequencies))

; markov-bass-lines.core=> (determine-probabilities (relevant-n-grams-only '{(tie tie) 1, (tie on) 3, (foo bar) 1} 'tie))
; ({(tie tie) 1/4} {(tie on) 3/4})

; the next thing is find the lowest common denominator of this map, multiply each of the tokens by that number, and
; put them all in a big list. then just pull a random element from that list; voila, Markov. then you add more
; sophistication later, maybe.

(defn denominators [probability-map]
  (distinct (map (fn [fraction] (denominator (first (vals fraction)))) probability-map)))

(defn denominator-for-markov-list [probability-map]
  (let [denoms (denominators probability-map)]
    (if (= 1 (count denoms))
      (first denoms)
      (reduce math/lcm denoms))))

(defn normalize-markov-elements [token-fraction-pair list-denom]
  (let [possible-choice (second (first (first token-fraction-pair)))
        num (last (last token-fraction-pair))]
    (repeat (* list-denom num) possible-choice)))

(defn make-markov-list [probability-map]
  (flatten (map (fn [t-f-pair]
                    (normalize-markov-elements t-f-pair
                                               (denominator-for-markov-list probability-map)))
                probability-map)))

; now randomly select an element from the above list
(defn second-note []
  (rand-nth
    (make-markov-list
      (determine-probabilities
        (relevant-n-grams-only (n-gram-freqs 2)
                               first-note)))))

; this does not work for shit
  (let [freqs (n-gram-freqs 2)
        choices '((on off) ; this list should be dynamically generated
                  (on on)  ; e.g., (choices (freqs first-note))
                  (on tie))]
    (freqs (first choices))))

; gotta get my dev on

(def fg "you have to quit the repl with control-D first")

(def example (determine-probabilities (relevant-n-grams-only (n-gram-freqs 2) 'on)))

; lein bs

(defn -main [])

