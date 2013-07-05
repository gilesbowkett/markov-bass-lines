(ns markov-bass-lines.core
  (:require [clojure.math.numeric-tower :as math])
  (:use overtone.live))

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

(defn denominators [probability-map]
  (distinct (map (fn [fraction]
                     (denominator (first (vals fraction))))
                 probability-map)))

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
(defn choose-markov-element [prev-element]
  ; probably for efficiency I should move some of this up a bit, later. the only
  ; part that actually needs to run every time is rand-nth. and of course it has
  ; to scoop up the prev elements and collect/recycle them. I guess the argument
  ; to n-gram-freqs will change too. hmm
  (rand-nth
    (make-markov-list
      (determine-probabilities
        (relevant-n-grams-only (n-gram-freqs 2)
                               prev-element)))))

(defn lz-sq-markov [prev-element]
    (lazy-seq
      (let [new-element (choose-markov-element prev-element)]
        (cons new-element
              (lz-sq-markov new-element)))))

(defn basic-bass-sequence []
  ; probably for elegance some of this logic should roll up into lz-sq-markov
  (concat [first-note]
          (take 32 (lz-sq-markov first-note))))

; scavenged hoover

(defsynth hoover [freq 220 amp 10 lgu 0.1 lgd 1 gate 1]
   (let [pwm (lin-lin (sin-osc:kr (vec (repeatedly 3 #(ranged-rand 2 4)))) -1 1 0.125 0.875)
         freq (lag-ud freq lgu lgd)
         freq (*
                freq
                (lin-exp (sin-osc:kr
                               (vec (repeatedly 3 #(ranged-rand 2.9 3.1)))
                               (vec (repeatedly 3 #(ranged-rand 0 (* 2 Math/PI))))
                               ) -1 1 0.995 1.005))
         mix (*
               0.1
               (apply +
                      (*
                        (lin-lin (lf-saw (* [0.25 0.5 1] freq) 1) -1 1 0 1)
                        (- 1 (lf-pulse:ar (* freq [0.5 1 2]) 0 pwm)))))
         ;bass
         mix (+ mix (lf-par (* 0.25 freq) 0))
         mix (mul-add mix 0.1 0)
         ;eq
         mix (b-peak-eq mix 6000 1 3)
         mix (b-peak-eq mix 3500 1 6)
         ;chorus
         mix (+ mix
               (* 0.5 (comb-c mix 1/200
                           (lin-lin (sin-osc:kr 3 [(* 0.5 Math/PI) (* 1.5 Math/PI)]) -1 1 1/300 1/200)
                           0)))
         env (env-gen (asr) gate)]
     (out 0 (* mix env amp))))

; read token from list to beat of metronome
(defn token-to-midi-action [metro tick lz-sq]
  (let [current-note (first lz-sq)
        next-tick (+ 0.25 tick)]
    (case current-note
      on (hoover)
      off (stop) ; this is fine to demo the bass lines, but it sucks because it doesn't just stop
                 ; the hoover. it stops *everything*. so to integrate with drums you'll need to
                 ; something better. (#FIXME)
      tie ())
    (apply-at (metro next-tick) token-to-midi-action metro next-tick (next lz-sq) [])))

(defn play-infinite-notes []
  (let [midi-flags (lz-sq-markov first-note)
        metro (metronome 110)]
    (token-to-midi-action metro (metro) midi-flags)))

; add drums

; gotta get my dev on

(def fg "you have to quit the repl with control-D first")

; lein bs

(defn -main [])

