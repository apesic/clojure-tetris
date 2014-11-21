(ns clojure-tetris.core
  (:require [lanterna.screen :as s])
  (:gen-class))

;; BOARD
(def COLS 10)
(def ROWS 20)

;; SHAPES

;; SCREEN
(def scr (s/get-screen))

(defn clear-screen  [screen]
  (let [blank  (apply str  (repeat COLS \space))]
    (doseq  [row  (range ROWS)]))
      (s/put-string screen 0 row blank))

;; Game Loop:
;; 1. Draw the board
;; 2. Get user input
;; 3. Process input, create new board
;; 4. Recur with new board

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

