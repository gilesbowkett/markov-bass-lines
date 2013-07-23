; namespace and library requires (clojure plumbing)
(ns markov-bass-lines.core
  (:require [clojure.math.numeric-tower :as math])
  (:use overtone.live))

; transcribed by hand, using a Matrix in Reason, from loops by Nick Thayer
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

; actually I don't need the titles
(def basslines (map (fn [title-and-notes]
                        (first (rest title-and-notes)))
                    titles-and-basslines))

; get n-grams of arbitrary size, and their frequencies
(defn n-gram-freqs [n]
  (apply merge-with + (map (comp frequencies
                                 (partial partition n 1))
                           basslines)))

; start on the downbeat. true for every one of the corpus basslines, and
; probably every funk bassline since the dawn of time.
(def first-note 'on)

; find out how probable any given sequence of tokens is
; e.g., (determine-probabilities (n-gram-freqs 2))
; or (determine-probabilities (n-gram-freqs 4))
; which yields ...{(tie tie tie off) 1/290} {(on on tie off) 1/29}...
; one combo ten times more likely than the other
(defn determine-probabilities [tokens-and-frequencies]
  (map (fn [t-and-f]
           (let [freq (last t-and-f)
                 tokens (first t-and-f)]
             {tokens (/ freq
                        (apply + (vals tokens-and-frequencies)))}))
       tokens-and-frequencies))

; filter out irrelevant n-grams, e.g.:
;
; (determine-probabilities (relevant-n-grams-only (n-gram-freqs 2) 'on))
; --> ({(on tie) 62/131} {(on on) 34/131} {(on off) 35/131})
;
; or:
;
; (determine-probabilities (relevant-n-grams-only (n-gram-freqs 8) '(on tie on on)))
; --> ({(on tie on on off on off on) 1/4} {(on tie on on tie on tie off) 1/8}
;      {(on tie on on on tie off on) 1/8} {(on tie on on off on on on) 1/8}
;      {(on tie on on off on tie on) 1/8} {(on tie on on tie off on off) 1/4})

(defn relevant-n-grams-only [tokens-and-frequencies target]
  (filter (fn [t-and-f]
              (= target
                (if (list? target)
                    (take (count target) (first t-and-f))
                    (first (first t-and-f)))))
          tokens-and-frequencies))

; find the denominators in a list like that
(defn denominators [probability-map]
  (distinct (map (fn [tokens-and-fraction]
                     (denominator (first (vals tokens-and-fraction))))
                 probability-map)))

; figure out the lowest common denominator
(defn denominator-for-markov-list [probability-map]
  (let [denoms (denominators probability-map)]
    (if (= 1 (count denoms))
      (first denoms)
      (reduce math/lcm denoms))))

; FIXME: normalize-markov-elements needs to do one thing against a list of two elements
; and another against a list of 8. depending on whether you're looking at an actual Markov
; chain, or just a simple probability table. make-markov-list probably requires similar
; adjustments.

(defn extract-probability [tokens-and-probability]
  (num (last (last tokens-and-probability))))

; markov-bass-lines.core=> (extract-possible-choice {'(tie on) 10/21})
; (on)
; markov-bass-lines.core=> (extract-possible-choice {'(on on tie on off off on tie) 10/21})
; (off off on tie)
(defn extract-possible-choice [tokens-and-probability]
  (let [tokens (first (keys tokens-and-probability))]
    (drop (* (/ 1 2) (count tokens)) tokens)))

; multiply all fractions by a common denominator
(defn normalize-markov-elements [tokens-and-probability list-denom]
  (let [possible-choice (extract-possible-choice tokens-and-probability)
        probability (extract-probability tokens-and-probability)]
    (repeat (* list-denom probability) possible-choice)))

; create a big list to grab random elements from, like the markov
; implementation we did in class
(defn make-markov-list [probability-map]
  (flatten (map (fn [t-f-pair]
                    (normalize-markov-elements t-f-pair
                                               (denominator-for-markov-list probability-map)))
                probability-map)))

; now randomly select an element from the above list
(defn choose-markov-element [previous-element n]
  (rand-nth
    (make-markov-list
      (determine-probabilities
        (relevant-n-grams-only (n-gram-freqs n)
                               previous-element)))))

; create a clojure "lazy sequence" (infinite quasi-list)
; tldr this is how you recurse
(defn lz-sq-markov [previous-element n]
  (lazy-seq
    (let [new-element (choose-markov-element previous-element n)]
      (cons new-element
            (lz-sq-markov new-element n)))))

; grab 32 16th notes, i.e., a two-bar bassline loop
(defn basic-bass-sequence []
  ; probably for elegance some of this logic should roll up into lz-sq-markov
  (concat [first-note]
          (take 31 (lz-sq-markov first-note 2))))
  ; ??? (take 32 (lz-sq-markov first-note 2)) would this work?? FIXME

; the below function fails, because it passes a list (namely first-four-notes)
; to lz-sq-markov, but lz-sq-markov's only equipped to receive a single element.
; I fixed relevant-n-grams-only; current FIXME is for normalize-markov-elements
; and possibly make-markov-list as well.
(defn more-structured-bass-sequence []
  (let [first-four-notes (concat [first-note]
                                 (take 3 (lz-sq-markov first-note 2)))]
    (concat first-four-notes
            (take 12 (lz-sq-markov first-four-notes 4))
            first-four-notes
            (take 12 (lz-sq-markov first-four-notes 4)))))

; scavenged synth (modified)
; http://jvmsoup.com/2012/11/28/hoover-sound-in-overtone/
(defsynth hoover [freq 220 amp 5 lgu 0.1 lgd 1 gate 1]
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

(def bass-synth hoover)



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

; get a random note from the minor pentatonic Eb scale (all black keys)
(defn random-minor-pentatonic []
  (rand-nth [:eb15 :gb15 :ab15 :bb15 :db15 :eb16]))

; now we get serious

; metronome
(def metro (metronome 110))

; translate from "on tie" approach to concept of note duration
(defn determine-note-duration
  ([subsequent-actions]
    (determine-note-duration subsequent-actions 0))
  ([subsequent-actions duration]
    (if (= (second (first subsequent-actions)) 'tie)
      (determine-note-duration (rest subsequent-actions) (+ duration 0.25))
      (+ duration 0.19))))

; schedule notes to play
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

; scavenged drum sounds

; drum sounds (sampled)
(def snare (sample (freesound-path 26903)))
(def kick (sample (freesound-path 2086)))

; drum sound (synthesized)
(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.1) :action FREE)]
    (* volume 1 src env)))

; volume modified
(defn weak-hat []
  (hat 0.3))

; play a typical moombahton beat
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

; drums!
(defn drums []
  (simple-moom metro (metro)))

; bass!
(defn bass []
  (let [primitive-bass-line (for [action (basic-bass-sequence)]
                                 [(random-minor-pentatonic) action])]
    (token-to-midi-action-2 metro (metro) (cycle primitive-bass-line))))

; go!
(defn go []
  (drums)
  (bass))

; nice example
(defn nice-example []
  (let [primitive-bass-line '([:eb16 on]  [:eb15 tie] [:bb15 off] [:bb15 off]
                              [:ab15 on]  [:eb16 off] [:db15 on]  [:eb15 tie]
                              [:gb15 off] [:gb15 off] [:ab15 off] [:db15 off]
                              [:eb15 off] [:bb15 off] [:ab15 on]  [:db15 tie]
                              [:eb16 on]  [:bb15 tie] [:gb15 on]  [:db15 tie]
                              [:ab15 on]  [:db15 tie] [:eb15 off] [:ab15 off]
                              [:db15 off] [:gb15 on]  [:gb15 off] [:db15 on]
                              [:gb15 tie] [:db15 on]  [:db15 off] [:db15 on])]
    (token-to-midi-action-2 metro (metro) (cycle primitive-bass-line))
    (drums)))

; lein bs (clojure plumbing)
(defn -main [])

; these synths sound great even though the project they're in doesn't even run >.<
; https://github.com/ctford/whelmed/blob/master/src/whelmed/instrument.clj

