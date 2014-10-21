(ns pld-demonstration.core
  (:require [seesaw.core :as s]
            [seesaw.graphics :as g]
            [seesaw.color :as c]
            [clojure.core.async :as async]
            [clojure.math.numeric-tower :as math])
  (:use [pld-demonstration.complex])
  (:import (pld_demonstration.complex Complex)   
           (javax.swing JFrame JLabel)
           (java.awt.image BufferedImage)
           (java.awt Dimension Color Graphics)))

(set! *warn-on-reflection* true)

(defn clear 
  [bi width height]
  (let [act-width (dec width) graphics (g/.getGraphics ^BufferedImage bi)]
    (g/.setColor ^Graphics graphics (Color. 0 0 0))
    (doseq [j (range height)] 
      (g/.drawLine ^Graphics graphics 0 j act-width j))))

(defn cap 
  [^Double a ^Double b]
  (if (> a b)
    b
    a))

(defn calc-pixel-color 
  [iterations max-iterations]
  (if (or (< iterations 10)
          (>= iterations max-iterations))
    (c/color 0 0 0)
    (let [gray (int (/ (* iterations 255) max-iterations))
          r    gray
          g    (cap (int (/ (* 5 ( * gray gray)) 255)) 255)
          b    (cap (int (+ 40 ( / (* 5 (* gray gray)) 255))) 255)]
      (c/color r g b))))

(defn draw-pixel
  [graphics x y color] 
  (g/push ^Graphics graphics
          (g/.setColor ^Graphics graphics color)
          (g/.drawLine ^Graphics graphics x y x y))
  graphics)

;Get an image up

(def image (g/buffered-image 500 500))

(def frame (s/frame :content (s/label :icon image) :width 500 :height 500))

(def repaint-thread (future (loop [] (s/repaint! frame) (Thread/sleep 33) (recur)))) 

(-> frame s/pack! s/show!)

;Typical clojure implementation

(defn mandlebrot  
  [image x y width height & {:keys [iterations image-width image-height] :or {image-width 500 image-height 500 iterations 32}}]
  
  (let [graphics (g/.getGraphics ^BufferedImage image)    
        
        is (doall (range image-width))
        
        js (doall (range image-height))  
        
        mandle-calc (fn this 
                      [c z iters] 
                      (if (or (> (magnitude z) 2.0)
                              (> iters iterations)) 
                        iters
                        (fn [] (this c (add-c c (mult-c z z)) (inc iters)))))]  
    
    (doseq [j js i is] 
      
      (let [c (complex (+ x (* width (/ i image-width))) (+ y (* height (/ j image-height))))]
        
        (draw-pixel graphics i j (calc-pixel-color (trampoline mandle-calc c c 0) iterations))))))

(defn mandlebrot  
  
  [image x y width height & {:keys [iterations image-width image-height] :or {image-width 500 image-height 500 iterations 32}}] 
  
  (let [graphics (g/.getGraphics ^BufferedImage image)  
        
        is (doall (range image-width))
        
        js (doall (range image-height))     
        
        mandle-calc (fn this 
                      [^Complex c ^Complex z ^Long iters] 
                      (if (or (> (magnitude z) 2.0)
                              (> iters iterations)) 
                        iters
                        (fn [] (this c (add-c c (mult-c z z)) (inc iters)))))]   

      (doseq [i (doall (map (fn [js] 
                              (future 
                               (doseq [j js i is]       
                                (let [^Complex c (complex (+ x (* width (/ i image-width))) (+ y (* height (/ j image-height))))]
                                  (draw-pixel graphics i j (calc-pixel-color (trampoline mandle-calc c c 0) iterations))))))      
                            (partition 4 js)))]     
        (deref i))))

;Let's try a new approach

(defn mandlebrot  
  [image x y width height & {:keys [iterations image-width image-height] :or {image-width 500 image-height 500 iterations 32}}] 
  
  (let [graphics (g/.getGraphics ^BufferedImage image)   
        
        cs (doall (for [j (doall (range image-height)) i (doall (range image-width))] 
                    [i j (complex (+ x (* width (/ i image-width))) (+ y (* height (/ j image-height))))]))          
        
        sources (doall (repeatedly 125 async/chan))
        
        completeds? (doall (map async/onto-chan sources (partition 2000 cs)))
        
        mandle-calc (fn this 
                      [^Complex c ^Complex z iters] 
                      (if (or (> (magnitude z) 2.0)
                              (> iters iterations)) 
                        iters
                        (fn [] (this c (add-c c (mult-c z z)) (inc iters))))) 
        
        dests (doall (repeatedly 125 async/chan))
        
        mandle-worker (fn [source dest] 
                        (async/go
                          (loop
                            [val (async/<! source)] 
                            (if val
                              (let [^Complex c (val 2)] 
                                (async/>! dest (assoc val 2 (trampoline mandle-calc c c 0)))
                                (recur (async/<! source)))
                              (async/close! dest)))))
        
        display-worker (fn [source] 
                         (async/go-loop 
                           [val (async/<! source)]
                           (if val 
                             (let [^Long i (val 0)
                                   ^Long j (val 1)
                                   ^Long iters (val 2)]
                               (draw-pixel graphics i j (calc-pixel-color iters iterations))
                               (recur (async/<! source))))))
        
        mandle-chans (doall (map mandle-worker sources dests))
        
        display-chans (doall (map display-worker dests))]
    
    (doseq [i completeds?]
      (async/<!! i))))























;EXTRA############################3


;Agents!

(defn mandlebrot  
  [image x y width height & {:keys [iterations image-width image-height] :or {image-width 500 image-height 500 iterations 32}}]
  
  (let [graphics-agent (agent (g/.getGraphics ^BufferedImage image))
        
        is (doall (range image-width))
        
        js (doall (range image-height))  
        
        mandle-calc (fn this 
                      [c z iters] 
                      (if (or (> (magnitude z) 2.0)
                              (> iters iterations)) 
                        iters
                        (fn [] (this c (add-c c (mult-c z z)) (inc iters)))))]  
    
    (doseq [j js i is] 
      
      (let [c (complex (+ x (* width (/ i image-width))) (+ y (* height (/ j image-height))))]
        
        (send graphics-agent draw-pixel i j (calc-pixel-color (trampoline mandle-calc c c 0) iterations))))))

;Wombo combo! (futures + channels)

(defn mandlebrot  
  [image x y width height & {:keys [iterations image-width image-height] :or {image-width 500 image-height 500 iterations 32}}] 
  
  (let [graphics (g/.getGraphics ^BufferedImage image)      
        
        is (doall (range image-width))
        
        js (doall (range image-height))       
        
        mandle-calc (fn this 
                      [^Complex c ^Complex z ^Long iters] 
                      (if (or (> (magnitude z) 2.0)
                              (> iters iterations)) 
                        iters
                        (fn [] (this c (add-c c (mult-c z z)) (inc iters)))))
        
        display-worker (fn [source] 
                         (async/go 
                           (loop
                             [val (async/<! source)]
                             (if val 
                               (let [^Long i (val 0)
                                     ^Long j (val 1)
                                     ^Long iters (val 2)]
                                 (draw-pixel graphics i j (calc-pixel-color iters iterations))
                                 (recur (async/<! source)))))))
        
        dests (doall (repeatedly 100 async/chan))]    

      (doseq [i (doall (map (fn [js dest] 
                              (future 
                               (doseq [j js i is]       
                                (let [^Complex c (complex (+ x (* width (/ i image-width))) (+ y (* height (/ j image-height))))]
                                                                   
                                  (async/>!! dest [i j (trampoline mandle-calc c c 0)])))
                               (async/close! dest)))                                
                            (partition 5 js)                           
                            dests))])
      
      (doseq [i (doall (map display-worker dests))]
        (async/<!! i))))

