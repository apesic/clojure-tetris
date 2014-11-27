(ns clojure-tetris.tetris)

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
  (vec (repeat (* COLS ROWS) 0)))

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
                        (partial filter #(some #{0} %))
                        (partial partition COLS))
                       board)
        lines-cleared (/ (- (count board) (count cleared-board)) COLS)
        new-board (into
                    (vec (repeat (* COLS lines-cleared) 0))
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
  (vec (map-indexed
         (fn [y row]
           (vec (map-indexed
                  (fn [x cell] (f cell x y))
                  row)))
      matrix)))

(defn block-coords [block]
  "Return a list of coordinates for each filled cell of block, e.g. ((1 3) (2 3) (2 4) (2 5))"
  (let [matrix (:shape block)
        [x y] (:xy block)
        coord-matrix (map-matrix
                       (fn [cell cell-x cell-y]
                       [(* cell (+ cell-x x)) (* cell (+ cell-y y))]) matrix)]
        (filter #(not= '(0 0) %) (partition 2 (flatten coord-matrix)))))

(defn collides?
  [board block]
  (let [block-coords (block-coords block)]
    (or (some (fn [[x y]] (> y ROWS))
              block-coords)
        (some (fn [[x y]] (or (< x 0) (>= x COLS)))
              block-coords)
        (some (fn [[x y]] (or (nil? (get board (xy->position [x y])))
                              (not (zero? (get board (xy->position [x y]))))))
              block-coords))))

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

