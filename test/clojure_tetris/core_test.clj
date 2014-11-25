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
