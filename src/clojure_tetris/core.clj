(ns clojure-tetris.core
  (:require [lanterna.screen :as s])
  (:gen-class))

;; Board setup
(def COLS 10)
(def ROWS 20)

(defn position->xy [position]
  (let [x (mod position COLS)
        y (int (/ position COLS))]
     [x y]))

(defn xy->position [[x y]]
  (+ x (* y COLS)))

(defn blank-board []
  (vec (take (* COLS ROWS) (repeat 0))))

(defn merge-block
  [board block-coords]
  (if-let [[x y] (first block-coords)]
    (recur (assoc board (xy->position [x y]) 1) (rest block-coords))
    board))

(defn clear-filled-lines
  [board]
  (let [cleared-board ((comp
                        (partial vec)
                        (partial apply concat)
                        (partial filter #(some #{0} %1))
                        (partial partition COLS))
                       board)
        lines-cleared (/ (- (count board) (count cleared-board)) COLS)
        new-board (into
                    (vec (take (* COLS lines-cleared) (repeat 0)))
                    cleared-board)]
    [new-board lines-cleared]))

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

(defn map-matrix
  [f matrix]
  (vec (map-indexed  (fn [y row]
    (vec (map-indexed  (fn [x cell] (f cell x y)) row)))
      matrix)))

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
            (and (>= x 0) (< x COLS)))
          block-coords)
        (every?
          (fn [[x y]]
            (zero? (get board (xy->position [x y]))))
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

(defn down-or-merge
  [board block]
  (let [test-block (translate 0 1 board block)]
    (if (= block test-block)
      [(merge-block board (block-coords block)) (get-block)]
      [board test-block])))

(defn game-over?
  [board block]
  (and
    (collides? board block)
    (zero? (get (:xy block) 1))))

;; SCREEN
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


;; TODO:
;; - Next block
;; - Score tracking
;; - Refactor!

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

