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
          (take 31 (lz-sq-markov first-note))))

; scavenged bass synth
; https://github.com/overtone/overtone/blob/master/src/overtone/inst/synth.clj

(definst bass
  [freq 120 t 0.6 amp 0.5]
  (let [env  (env-gen (perc 0.08 t) :action FREE)
        src  (saw [freq (* 0.98 freq) (* 2.015 freq)])
        src  (clip2 (* 1.3 src) 0.8)
        sub  (sin-osc (/ freq 2))
        filt (resonz (rlpf src (* 4.4 freq) 0.09) (* 2.0 freq) 2.9)]
    (* env amp (fold:ar (distort (* 1.3 (+ filt sub))) 0.08))))

(def bass-synth bass)

; foolishness
(definst bubbles
  [bass-freq 80]
  (let [bub (+ bass-freq (* 3 (lf-saw:kr [8 7.23])))
        glis (+ bub (* 24 (lf-saw:kr 0.4 0)))
        freq (midicps glis)
        src (* 0.04 (sin-osc freq))
        zout (comb-n src :decay-time 4)]
    zout))

; infinite:
;   (token-to-midi-action-2 metro (metro) (cycle primitive-bass-line))
; finite:
;   (token-to-midi-action-2 metro (metro) primitive-bass-line)

; moom:
;   (simple-moom metro (metro))

(defn random-minor-pentatonic []
  (rand-nth [:eb3 :gb3 :ab3 :bb3 :db3 :eb3]))

; read token from list to beat of metronome, do some sophisticated shit
(def metro (metronome 110))
(def action-list (basic-bass-sequence))
(def primitive-bass-line (for [action action-list] [(random-minor-pentatonic) action]))
; primitive-bass-line is a straight list, not a loop; use the code which uses "cycle" for an infinite loop
; however that line of code is legit; just create something dynamic to replace the hard-coded :c3 note

; merge in the notes beforehand using a seq comprehension and some kind of
; note-assigning function.
(defn determine-note-duration
  ([subsequent-actions]
    (determine-note-duration subsequent-actions 0))
  ([subsequent-actions duration]
    (if (= (second (first subsequent-actions)) 'tie)
      (determine-note-duration (rest subsequent-actions) (+ duration 0.25))
      (+ duration 0.19))))


(defn token-to-midi-action-2 [metro tick note-action-pairs]
  (let [current-note (first (first note-action-pairs))
        current-action (second (first note-action-pairs))
        next-action (second (second note-action-pairs))
        next-tick (+ 0.25 tick)]
    (if (= current-action 'on)
      (let [duration (determine-note-duration (rest note-action-pairs))
            bass-synth-id (at (metro tick) (bass-synth (note current-note)))]
        (at (metro (+ tick duration)) (ctl bass-synth-id :gate 0))))
    (if (not (empty? note-action-pairs))
      (apply-at (metro next-tick)
                token-to-midi-action-2
                metro next-tick
                (next note-action-pairs) []))))

(def snare (sample (freesound-path 26903)))
(def kick (sample (freesound-path 2086)))

(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.1) :action FREE)]
    (* volume 1 src env)))

(defn weak-hat []
  (hat 0.3))

(defn simple-moom [metro beat-number]

  ; kick
  (at (metro (+ 0 beat-number)) (kick))
  (at (metro (+ 1 beat-number)) (kick))
  (at (metro (+ 2 beat-number)) (kick))
  (at (metro (+ 3 beat-number)) (kick))

  ; snare
  (at (metro (+ 0.75 beat-number)) (snare))
  (at (metro (+ 1.5 beat-number)) (snare))
  (at (metro (+ 2.75 beat-number)) (snare))
  (at (metro (+ 3.5 beat-number)) (snare))

  ; hat
  (at (metro (+ 0.5 beat-number)) (weak-hat))
  (at (metro (+ 1.5 beat-number)) (weak-hat))
  (at (metro (+ 2.5 beat-number)) (weak-hat))
  (at (metro (+ 3.5 beat-number)) (weak-hat))

  (apply-at (metro (+ 4 beat-number)) simple-moom metro (+ 4 beat-number) []))

(defn go []
  ; FIXME: these should be in a let, not global like this
  (def action-list (basic-bass-sequence))
  (def primitive-bass-line (for [action action-list] [(random-minor-pentatonic) action]))
  (simple-moom metro (metro))
  (token-to-midi-action-2 metro (metro) (cycle primitive-bass-line)))

(defn bass-only []
  ; FIXME: these should be in a let, not global like this
  ; FIXME: DRY
  (def action-list (basic-bass-sequence))
  (def primitive-bass-line (for [action action-list] [(random-minor-pentatonic) action]))
  (token-to-midi-action-2 metro (metro) (cycle primitive-bass-line)))

; gotta get my dev on

(def fg "you have to quit the repl with control-D first")

; lein bs

(defn -main [])

; these synths sound great even though the project they're in doesn't even run >.<
; https://github.com/ctford/whelmed/blob/master/src/whelmed/instrument.clj

; FIXME the harder problem is getting the drums to happen at the same time

