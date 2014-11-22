(ns clojure-tetris.core
  (:require [lanterna.screen :as s])
  (:gen-class))

;; BOARD
(def COLS 10)
(def ROWS 20)

(defn blank-board []
  (vec (take (* COLS ROWS) (repeat 0))))

(defn position->xy [position]
  (let [x (mod position COLS)
        y (int (/ position COLS))]
     [x y]))

;; BLOCKS
(def square-block [[1 1]
                   [1 1]])

(def line-block [1 1 1 1])

(def z-block [[1 1 0]
              [0 1 1]])

(def s-block [[0 1 1]
              [1 1 0]])

(def t-block [[1 1 1]
              [0 1 1]])

(def l-block [[1 0 0]
              [1 1 1]])

(def blocks [square-block line-block z-block s-block t-block l-block])

;; SCREEN
(def screen-width (+ COLS 8))
(def screen-height ROWS)
(def scr (s/get-screen :swing {:cols screen-width :rows screen-height}))
(def char-map {0 (str \space)
               1 "#"})

(defn clear-screen  [screen]
  (let [blank  (apply str  (repeat screen-width \space))]
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

(defn draw-block ;;FIXME: So ugly, needs refactoring
  ([screen block]
   (draw-block screen block [5 0]))
  ([screen block block-xy]
    (let [[x y] block-xy]
      (doseq [row (range (count block))]
        (doseq [cell (range (count (get block row)))]
          (let [value (get char-map (get (get block row) cell))]
            (s/put-string screen (+ x cell) (+ y row) value)))))))


;; Game Loop:
;; 1. Draw the board
;; 2. Get user input
;; 3. Process input, create new board
;; 4. Recur with new board

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

