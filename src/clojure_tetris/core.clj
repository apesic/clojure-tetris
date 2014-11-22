(ns clojure-tetris.core
  (:require [lanterna.screen :as s])
  (:gen-class))

;; BLOCKS
(def square-block [[1 1]
                   [1 1]])

(def line-block [[1]
                 [1]
                 [1]
                 [1]])

(def z-block [[1 1 0]
              [0 1 1]])

(def s-block [[0 1 1]
              [1 1 0]])

(def t-block [[1 1 1]
              [0 1 0]])

(def l-block [[1 0 0]
              [1 1 1]])

(def j-block [[1 1 1]
              [1 0 0]])

(def blocks [square-block line-block z-block s-block t-block l-block j-block])

(defn get-block []
  {:shape (rand-nth blocks), :xy [5 0]})

(def COLS 10)
(def ROWS 20)

(defn map-matrix
  [f matrix]
  (vec (map-indexed  (fn [y row]
    (vec (map-indexed  (fn [x cell] (f cell x y)) row)))
      matrix)))

(defn position->xy [position]
  (let [x (mod position COLS)
        y (int (/ position COLS))]
     [x y]))

(defn xy->position [[x y]]
  (+ x (* y COLS)))

(defn block-coords [block]
  "Return a list of coordinates for each filled cell of block, e.g. ((1 3) (2 3) (2 4) (2 5))"
  (let [matrix (:shape block)
        [x y] (:xy block)
        coord-matrix (map-matrix (fn [cell cell-x cell-y]
                       (vector (* cell (+ cell-x x)) (* cell (+ cell-y y)))) matrix)]
        (filter #(not= '(0 0) %1) (partition 2 (flatten coord-matrix)))))

;; TODO: Refactor to use a single every?
(defn collides?
  [board block]
  (let [block-coords (block-coords block)]
    (not
      (and
        (every?
          (fn [[x y]]
            (< y ROWS))
          block-coords)
        (every?
          (fn [[x y]]
            (zero? (get board (xy->position [x y]))))
          block-coords)
        (every?
          (fn [[x y]]
            (and (>= x 0) (< x COLS)))
          block-coords)))))

(defn translate
  [dx dy board block]
  (let [translated (update-in block [:xy] #(vec (map + [dx dy] %)))]
    (if (collides? board translated)
      block
      translated)))

;;TODO: Refactor using comp
(defn rotate
  [board block]
  (let [rotated (update-in block [:shape]
                           #(vec (apply map vector (map reverse %))))]
    (if (collides? board rotated)
      block
      rotated)))

;; BOARD
(defn blank-board []
  (vec (take (* COLS ROWS) (repeat 0))))

(defn merge-block
  [board block-coords]
  (if-let [[x y] (first block-coords)]
    (recur (assoc board (xy->position [x y]) 1) (rest block-coords))
    board))

(defn down-or-anchor
  [board block]
  (let [test-block (translate 0 1 board block)]
    (if (= block test-block)
      [(merge-block board (block-coords block)) (get-block)]
      [board test-block])))

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

;; TODO: Refactor to take advantage of map-matrix function
(defn draw-block
  [screen block]
  (let [[x y] (:xy block)
        shape (:shape block)]
    (doseq [row (range (count shape))]
      (doseq [cell (range (count (get shape row)))]
        (let [value (get char-map (get-in shape [row cell]))]
          (s/put-string screen (+ x cell) (+ y row) value))))))

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

;; TODO:
;; - Detect full lines
;; - Clear lines
;; - Game over?
;; - Score
;; - Increasing speed

(defn -main
  [& args]
  (s/start scr)

  ;; Game Loop:
  (loop [board (blank-board)
         block (get-block)
         score 0
         past-tick (System/currentTimeMillis)]

    ;; 1. Draw the board
    (clear-screen scr)
    (draw-border scr)
    (draw-score scr score)
    (draw-board scr board)
    (draw-block scr block)
    (s/redraw scr)

    ;; 2. Get user input

    (let [action (key-mapping (s/get-key scr))
          current-tick (System/currentTimeMillis)
          next-tick (if (> (- current-tick past-tick) 600)
                      current-tick
                      past-tick)]
      (if (> next-tick past-tick) (println "Down"))
      (cond
        (> next-tick past-tick)
        (let [[new-board new-block] (down-or-anchor board block)]
         (recur
           new-board
           new-block
           score
           next-tick))
        action
        (recur
          board
          (action board block)
          score
          next-tick)

        :else
        (recur
          board
          block
          score
          next-tick)))

    ;; 3. Process input, create new board
    ;; 4. Recur with new board
    ))

