(ns pegthing.core
  (:gen-class))

(declare successful-move prompt-move game-over prompt-rows)

;;;;
;; Define axial operations
;;;;
(defn axial-add
  [p1 p2]
  {:q (+ (p1 :q) (p2 :q)) :r (+ (p1 :r) (p2 :r))}
  )

(defn axial-scale
  [p s]
  {:q (int (* s (p :q))) :r (int (* s (p :r)))}
  )

(defn axial-subtract
  [p1 p2]
  (axial-add p1 (axial-scale p2 -1))
  )

(defn axial-distance
  "Calculate the distance between two axial coordinates"
  [p1 p2]
  (let [diff-vec (axial-subtract p1 p2)]
    (/ (+
        (abs (diff-vec :q))
        (abs (+ (diff-vec :q) (diff-vec :r)))
        (abs (diff-vec :r))
        ) 2)
    ))

(defn axial-to-oddr
  [{:keys [q r]}]
  ;;{:q q :r r}
  (let [col (+ q (/ (- r (if (odd? r) 1 0)) 2))]
    {:col col :row r}
    )
  )

(def axial-ordinals
  {:left  {:q -1 :r 1}
   :right {:q 1 :r 0}
   :up-left {:q 0 :r -1}
   :up-right {:q 1 :r -1}
   :down-left {:q -1 :r 1}
   :down-right {:q 0 :r 1}})

(defn adjacent
  "Return the adjacent coordinate in the given direction"
  [p ord]
  (axial-add p (axial-ordinals ord)))

;;;;
;; Create the board
;;;;
(defn pegify-board
  [board]
  (update-vals board #(assoc % :pegged true)))

(defn tagged-order-assoc
  "Given a key and a map of keys->order,
  insert the key if it is not already in the collection.
  Assume the order is zero-indexed."
  [coll key]
  (if (contains? coll key)
    coll
    (assoc coll key {:order (count coll)})))

(defn build-triangle
  "Create a triangle to the given depth with axial coordinates
  represented as a map of coordinates->order.
  "
  ([depth]
   (if (> depth 0)
     (let [coords {:q (- depth 1) :r 0}]
       (build-triangle depth [coords] (tagged-order-assoc {} coords)))
     )
   )
  ([depth queue coll]
   (if (empty? queue)
     coll 
     (let [[head & remaining] queue]
       (if (= (- depth (:r head)) 1)
         coll
         (recur depth
                (conj (vec remaining) (adjacent head :down-left) (adjacent head :down-right))
                (tagged-order-assoc (tagged-order-assoc coll (adjacent head :down-left)) (adjacent head :down-right))
                ))))))

(defn new-board
  [rows]
  (pegify-board (build-triangle rows)))

;;;;
;; Move pegs
;;;;
(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil
  otherwise"
  [board p1 p2]
  (if (and (board p1)
           (board p2)
           (= (axial-distance p1 p2) 2)
           (pegged? board p1)
           (not (pegged? board p2)))
    ;; calculate the coordinates of the jumped peg
    (let [jumped (axial-add p1 (axial-scale (axial-subtract p2 p1) 1/2))]
      (if (pegged? board jumped) jumped))))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is the
  destination and the value is the jumped position"
  [board pos]
  (let [destinations (map #(axial-add pos (axial-scale % 2)) (vals axial-ordinals))]
    (into {} (filter second
                     (zipmap destinations (map #(valid-move? board pos %) destinations))
                     ))))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

;;;;
;; Represent board textually and print it
;;;;
(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(def ansi-styles
  {:red   "[31m"
   :green "[32m"
   :blue  "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn render-coord
  [board coord]
  (str (nth letters (get-in board [coord :order]))
       (if (get-in board [coord :pegged])
         (colorize "0" :blue)
         (colorize "-" :red))))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

#_
(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board) (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

;;;;
;; Interaction
;;;;
(defn letter->coord
  "Converts a letter string to the corresponding coordinate"
  [board letter]
  ;; calculate the order corresponding to the letter
  (let [order (- (int (first letter)) alpha-start)]
    ;; find the coordinate with order
    (first (first (filter (fn [[coord attrs]] (= (attrs :order) order)) board)))
    ))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input ""))
  ([default]
     (let [input (clojure.string/trim (read-line))]
       (if (empty? input)
         default
         (clojure.string/lower-case input)))))

(defn characters-as-strings
  "Given a string, return a collection consisting of each individual
  character"
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->coord (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (successful-move new-board)
      (do
        (println "\n!!! That was an invalid move :(\n")
        (prompt-move board)))))

(defn successful-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->coord (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn -main
  [& args]
  (println "Get ready to play peg thing!")
  (prompt-rows))
