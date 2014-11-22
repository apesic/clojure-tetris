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

(defn get-block []
  {:shape (rand-nth blocks), :xy [5 0]})

;; TODO: implement actual collision detection
(defn collides?
  [board block]
  false)

(defn translate
  [dx dy board block]
  (let [translated (update-in block [:xy] #(vec (map + [dx dy] %)))]
    (if (collides? board translated)
      block
      translated)))

(defn rotate
  [board block]
  (let [rotated (update-in block [:shape] #(vec  (apply map vector (map reverse %))))]
    (if (collides? board rotated)
      block
      rotated)))

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

    :else nil))

(defn -main
  [& args]
  (s/start scr)

  ;; Game Loop:
  (loop [board (blank-board)
	 block (get-block)
	 score 0]

    ;; 1. Draw the board
    (clear-screen scr)
    (draw-border scr)
    (draw-score scr score)
    (draw-board scr board)
    (draw-block scr block)
    (s/redraw scr)

    ;; 2. Get user input

    (if-let [action (key-mapping (s/get-key-blocking scr))]
      (recur
	board
	(action board block)
	score
	)
      (recur
	board
	block
	score))
    ;; 3. Process input, create new board
    ;; 4. Recur with new board
    ))

