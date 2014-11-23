(ns clojure-tetris.core
  (:require [lanterna.screen :as s])
  (:use clojure-tetris.tetris)
  (:gen-class))

;; TODO:
;; - Implement multi-threading
;; - Refactor!
;; - Score tracking
;; - Next block
;; - Wall kick
;; - Redo gui with OpenGL

;; UI Drawing
(def screen-width (+ COLS 8))
(def screen-height ROWS)
(def scr (s/get-screen :swing {:cols screen-width :rows screen-height}))
(def char-map {0 (str \space)
               1 "#"})

(defn clear-screen  [screen]
  (let [blank  (clojure.string/join (repeat screen-width \space))]
    (doseq  [row  (range screen-height)]
      (s/put-string screen 0 row blank))))

(defn draw-border [screen]
  (doseq [row (range screen-height)]
    (s/put-string screen COLS row "|")))

(defn draw-score
  ([screen]
   (draw-score screen 0))
  ([screen score]
   (s/put-string screen 11 0 "Score:")
   (s/put-string screen 11 1 (str score))))

(defn draw-board
  [screen board]
  (doseq [cell (range (count board))]
    (let [[x y] (position->xy cell)
          value (get char-map (get board cell))]
      (s/put-string screen x y value))))

(defn draw-block
  [screen block]
  (let [coords (block-coords block)]
    (doseq [[x y] coords]
      (s/put-string screen x y (get char-map 1)))))

(defn draw-game-over
  [screen score]
  (clear-screen screen)
  (s/put-string screen 5 4 "GAME OVER")
  (s/put-string screen 5 6 (str "SCORE: " score))
  (s/redraw screen))

(defn draw-all
  [scr board block score]
  (clear-screen scr)
  (draw-border scr)
  (draw-score scr score)
  (draw-board scr board)
  (draw-block scr block)
  (s/redraw scr))

;; INPUT
(defn key-mapping [input]
  (cond
    (= input :left)
    (partial translate -1 0)

    (= input :right)
    (partial translate 1 0)

    (= input :down)
    (partial translate 0 1)

    (= input :up)
    rotate

    :else
    nil))

(def starting-tick-length 600)

(defn -main
  [& args]

  (s/start scr)

  ;; Game Loop:
  (loop [board (blank-board)
         block (get-block)
         score 0
         past-tick (System/currentTimeMillis)
         tick-length starting-tick-length]

    (draw-all scr board block score)

    (let [user-action (key-mapping (s/get-key scr))
          current-tick (System/currentTimeMillis)
          next-tick (if (> (- current-tick past-tick) tick-length)
                      current-tick
                      past-tick)]
      (cond
        (> next-tick past-tick)
        (let [[merged-board new-block] (down-or-merge board block)
              [new-board new-score] (clear-filled-lines merged-board)]
         (recur
           new-board
           new-block
           (+ new-score score)
           next-tick
           (- starting-tick-length (* 10 score))))

        (game-over? board block)
        (draw-game-over scr score)

        user-action
        (recur
          board
          (user-action board block)
          score
          next-tick
          (- starting-tick-length (* 10 score)))

        :else
        (recur
          board
          block
          score
          next-tick
          (- starting-tick-length (* 10 score)))))))

