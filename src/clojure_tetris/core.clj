(ns clojure-tetris.core
  (:require [lanterna.screen :as s])
  (:require [ clojure-tetris.tetris :as t])
  (:gen-class))

;; TODO:
;; - Implement multi-threading
;; - Refactor!
;; - Score tracking
;; - Next block
;; - Wall kick
;; - Redo gui with OpenGL

;; UI Drawing
(def screen-width (+ t/COLS 8))
(def screen-height t/ROWS)
(def scr (s/get-screen :swing {:cols screen-width :rows screen-height}))
(def char-map {0 (str " "), 1 "#"})
(def starting-tick-length 600)

(defn clear-screen [screen]
  (let [blank (apply str (repeat screen-width \space))]
    (doseq [row (range screen-height)]
      (s/put-string screen 0 row blank))))

(defn draw-border [screen]
  (doseq [row (range screen-height)]
    (s/put-string screen t/COLS row "|")))

(defn draw-score
  ([screen]
   (draw-score screen 0))
  ([screen score]
   (s/put-string screen 11 0 "Score:")
   (s/put-string screen 11 1 (str score))))

(defn draw-board
  [screen board]
  (doseq [cell (range (count board))]
    (let [[x y] (t/position->xy cell)
          value (get char-map (get board cell))]
      (s/put-string screen x y value))))

(defn draw-block
  [screen block]
  (let [coords (t/block-coords block)]
    (doseq [[x y] coords]
      (s/put-string screen x y (get char-map 1)))))

(defn draw-game-over
  [screen score]
  (clear-screen screen)
  (s/put-string screen 5 4 "GAME OVER")
  (s/put-string screen 5 6 (str "SCORE: " score))
  (s/redraw screen))

(defn draw-all
  [screen board block score]
  (clear-screen screen)
  (draw-border screen)
  (draw-score screen score)
  (draw-board screen board)
  (draw-block screen block)
  (s/redraw screen))

;; INPUT
(defn key-mapping [input]
  (case input
    :left (partial t/translate -1 0)
    :right (partial t/translate 1 0)
    :down (partial t/translate 0 1)
    :up t/rotate
    nil))

(defn -main
  [& args]

  (s/start scr)

  ;; Game Loop:
  (loop [board (t/blank-board)
         block (t/get-block)
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
        (let [[merged-board new-block] (t/down-or-merge board block)
              [new-board new-score] (t/clear-filled-lines merged-board)]
         (recur
           new-board
           new-block
           (+ new-score score)
           next-tick
           (- starting-tick-length (* 10 score))))

        (t/game-over? board block)
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

