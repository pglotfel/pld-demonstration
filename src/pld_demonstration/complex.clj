(ns pld-demonstration.complex
  (:require [clojure.math.numeric-tower :as math]))

(defprotocol IComplex 
  (magnitude ^Double [_])
  (real ^Double [_])
  (imaginary ^Double [_]))

(deftype Complex [^Double r ^Double i]  
  IComplex 
  (magnitude 
    [_]
    (math/sqrt (+ (* r r) (* i i))))  
  (real 
    [_]
    r) 
  (imaginary 
    [_]
    i))

(defn ^Complex complex 
  [^Double real ^Double imaginary]
  (->Complex real imaginary))

(defn ^Complex add-c 
  [^Complex a ^Complex b] 
  (complex (+ (real a) (real b)) (+ (imaginary a) (imaginary b))))

(defn ^Complex mult-c 
  
  [^Complex a ^Complex b] 
  
  (let [^Double r-a (real a)
        ^Double i-a (imaginary a)
        ^Double r-b (real b)
        ^Double i-b (imaginary b)]
    
    (complex (- (* r-a r-b) (* i-a i-b)) (+ (* i-a r-b) (* r-a i-b)))))