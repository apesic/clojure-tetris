(ns clojure-tetris.core-test
  (:require [clojure.test :refer :all]
            [clojure-tetris.core :refer :all]
            [clojure-tetris.tetris :refer :all]))

(deftest test-position->xy
  (testing "Should return [x y] coords for a 10 column grid"
    (is (= (position->xy 15) [5 1]))))

(deftest test-xy->position
  (testing "Should return linear position for [x y] in 10 column grid"
    (is (= (xy->position [5 1]) 15))))

(deftest test-blank-board
  (testing "Should return vector of zeros with length (10*20)"
    (is (= (count (blank-board)) 200))
    (is (every? zero? (blank-board)))))

(deftest test-merge-blank-board
  (testing "Should return new board with block merged in"
    (let [board (blank-board)
          block (block-coords {:shape square-block :xy [5 0]})]
      (is (= (get (merge-block board block) 5) 1))
      (is (= (get (merge-block board block) 6) 1))
      (is (= (get (merge-block board block) 15) 1))
      (is (= (get (merge-block board block) 16) 1)))))

(deftest test-clear-filled-lines
  (testing "Should remove all lines that are filled with 1's and return a vector with the new board and number of lines removed"
    (let [filled-board (vec (take (* COLS ROWS) (repeat 1)))]
      (is (=  [(blank-board) 20](clear-filled-lines filled-board))))))

(deftest test-map-matrix
  (testing "Should apply function to each cell in matrix"
    (is (= [[1 2] [2 3]] (map-matrix #(+ %1 %2 %3) [[1 1] [1 1]])))))

(deftest test-block-coords
  (testing "Should return a list of coord pairs for each filled cell in a block"
    (let [block {:shape l-block :xy [5 0]}]
      (is (= (block-coords block) '((5 0) (5 1) (6 1) (7 1)))))))


(deftest test-collides?
  (testing "Should return true when block collides with board or edges"
    (let [left-collide-block {:shape line-block :xy [-1 0]}
          right-collide-block {:shape line-block :xy [10 0]}
          board-collide-block {:shape line-block :xy [0 16]}
          bottom-collide-block {:shape line-block :xy [0 21]}
          no-collide-block {:shape line-block :xy [5 0]}
          board (vec (concat (vec (take (* 10 19) (repeat 0))) (vec (take 10 (repeat 1)))))]
      (is (collides? board left-collide-block))
      (is (collides? board right-collide-block))
      (is (collides? board board-collide-block))
      (is (collides? board bottom-collide-block))
      (is (not (collides? board no-collide-block))))))

(deftest test-translate
  (testing "Should return a new block with modified :xy if move is valid"
   (let [test-block {:shape line-block :xy [0 15]}
         board (vec (concat (vec (take (* 10 19) (repeat 0))) (vec (take 10 (repeat 1)))))]
     (is (= test-block (translate 0 1 board test-block)))
     (is (= test-block (translate -1 0 board test-block)))
     (is (= {:shape line-block :xy [1 15]} (translate 1 0 board test-block))))))

(def test-rotate
  (testing "Should return a rotated version of block"
    (let [L {:shape l-block :xy [0 5]}
          T {:shape t-block :xy [0 5]}
          S {:shape s-block :xy [0 5]}
          Z {:shape z-block :xy [0 5]}
          J {:shape j-block :xy [0 5]}
          square {:shape square-block :xy [0 5]}
          line {:shape line-block :xy [0 5]}
          board (blank-board)]
      (is (= (rotate board L) {:shape [[0 1] [0 1] [1 1]] :xy [0 5]}))
      (is (= (rotate board T) {:shape [[1 0] [1 1] [1 0]] :xy [0 5]}))
      (is (= (rotate board S) {:shape [[1 0] [1 1] [0 1]] :xy [0 5]}))
      (is (= (rotate board Z) {:shape [[0 1] [1 1] [1 0]] :xy [0 5]}))
      (is (= (rotate board J) {:shape [[1 0] [1 0] [1 1]] :xy [0 5]}))
      (is (= (rotate board square) {:shape [[1 1] [1 1]] :xy [0 5]}))
      (is (= (rotate board line) {:shape [[1 1 1 1]] :xy [0 5]})))))

(def test-down-or-merge
  (testing "Should translate a block down if possible (and return the board and updated block)
           or merge it to the board if not (and return the updated board and a new block)."
   (let [mergable-block {:shape [[1 1 1 1]] :xy [0 19]}
         unmergable-block {:shape [[1 1 1 1]] :xy [0 5]}
         board (blank-board)
         result-board (vec (concat (vec (take (* 10 19) (repeat 0))) [1 1 1 1 0 0 0 0 0 0]))]
     (is (= (first (down-or-merge board mergable-block )) result-board))
     (is (= (down-or-merge board unmergable-block) [board {:shape [[1 1 1 1]] :xy [0 6]}])))))

(def test-game-over?
  (testing "Should return true if a block collides with the board at it's starting position"
    (let [block (get-block)
         full-board (vec (take (* 10 20) (repeat 1)))
         empty-board (blank-board)]
    (is (game-over? full-board block))
    (is (not (game-over? empty-board block))))))
