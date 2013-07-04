(ns markov-bass-lines.core)

(def fg "you have to quit the repl with control-D first")

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
     (on tie off off off off off on on tie off off off off off on on on on tie off on tie off on tie off on on tie on tie ))

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

(def basslines (map (fn [title-and-notes] (first (rest title-and-notes))) titles-and-basslines))

(defn n-gram-freqs "get n-grams of arbitrary size, and their frequencies" [n]
  (apply merge-with
   +
   (map (comp frequencies
              (partial partition n 1))
        basslines)))

(defn -main [])

