(ns MatrixCalculator.Functions
  (:use clojure.test))

;predicate, is this a vector of vectors - a matrix?
(defn matrix? [m] 
  (if (or (number? m) (empty? m) (not (vector? m)) (and (not-every? vector? m) (not-every? number? m)) (some empty? m)) false
    (let [c (count (first m))]
      (if (every? #(= (count %) c) m) true false))))


;creates a matrix from a coll of vectors
;can create matrices with over 2 dimensions
(defn matrix [& v] {:pre [(every? vector? v)]} 
  (if (matrix? (vec v)) (vec v) nil))

;prints out a matrix in a form more easy for the user to read
;every function should be sent to this, for readability
;prints nil when done
(defn matPrint [m] {:pre [(matrix? m)]} 
  (print (apply str (interleave m (repeat "\n")))))


;gets the i-th row of a matrix (starts from 0)
(defn get-row [m i] {:pre [(matrix? m) (integer? i) (>= i 0)]} 
     (try (m i)
       (catch IndexOutOfBoundsException e nil)))


;calculates x^y
;return integer is possible
(defn expt [x y]
  (try 
    (if (== (java.lang.Math/pow x y) (int (java.lang.Math/pow x y)))
    (int (java.lang.Math/pow x y)) (java.lang.Math/pow x y))
    (catch IllegalArgumentException _ (java.lang.Math/pow x y))))

;gets the j-th col of a matrix (starts from 0)
(defn get-col [m j] {:pre [(matrix? m) (integer? j) (>= j 0)]} 
    (try
      (map #(% j) m) (vec (map #(% j) m))
      (catch IndexOutOfBoundsException _ nil)))



;counts rows
(defn rows 
  ([m] {:pre [(matrix? m)]}  (rows m 0))
  ([m v] (if (nil? (get-row m v)) v (rows m (+ v 1))))
  )


;counts columns
(defn cols
  ([m] {:pre [(matrix? m)]}  (cols m 0))
  ([m v] (if (nil? (get-col m v)) v (cols m (+ v 1))))
  )

;calculate transpose of matrix
(defn transpose [m]
  {:pre [(matrix? m)]}
    (vec (map get-col (repeat (count (m 0)) m) (iterate inc 0))))

;predicate, is a matrix square (num_rows==num_cols)
(defn square? [m] {:pre [(matrix? m)]} 
  (= (rows m) (cols m)))

;to use manually, enter (list coll) where coll are the matrices
;you wish to check
(defn equallySized? [coll] {:pre [(every? matrix? coll)]}
  (if (> (count coll) 1)
    (if (and (= (rows (first coll)) (rows (first (rest coll)))) 
             (= (cols (first coll)) (cols (first (rest coll)))))
      (equallySized? (rest coll)) false)
      true))

;util for divisable
(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

; another last minute, isn't in report and doesn't need to be
; predicate for matFunc to allow division
(defn divisable? [coll]
  (not (in? (flatten coll) 0)))

;element by element actions
;called through matFunc
(defn vecFunc [f & v] 
  {:pre [(function? f) (not-empty v) (every? vector? v)]} 
  (vec (reduce #(map f %1 %2) v)))




;; element by element actions
;; applicable functions are:
 ; + - element by element addition
 ; - - element by element substraction
 ; * - element by element multiplication
 ; / - element by element division
 ; expt - element by element power
;; has inlined lazyseq function
(defn matFunc [f & m]
  {:pre [(function? f) (not-empty m) (every? matrix? m) (equallySized? m)
         (if (= f /) (divisable? (rest m)) true)]}
  (defn- listVec "Turns a list of lists into a vector of vectors"
    [lst] (cons (vec (first lst)) 
                (lazy-seq (listVec (rest lst)))))
  (if (or (= f -) (= f /) (= f *) (= f +) (= f expt))  
    (let [lst (butlast(conj
         (apply vecFunc #(map f %1 %2) m) ()))]
      (vec(take (count 
                  (get-col (first m) 0)) (listVec lst)))) nil)
  )

;multiply two vectors (for matMul)
(defn vecMul [v1 v2] {:pre [(vector? v1) (vector? v2)]} 
  (if (= (count v1) (count v2))
    (apply + (map * v1 v2))))

;multiply any number of matrices
(defn matMul 
  ([] nil)
  ([m] {:pre [(matrix? m)]}  m)
  ([m1 m2] {:pre [(matrix? m1) (matrix? m2)]} 
           (if (and (= (count (m1 0)) (count m2))) 
             (let [tm2 (transpose m2)]
               (vec (map 
                      #(vec (map vecMul (repeat %) tm2)) m1))) nil))
  ([m1 m2 & more]
    (if (every? matrix? more)
      (reduce matMul (matMul m1 m2) more)))
  ) 

;multiply matrix by itself i times
(defn matPow 
  ([] nil)
  ([m] {:pre [(square? m)]} 
       m)
  ([m i] {:pre [(> i 0) (integer? i)]} 
         ;testing for integer isn't enough, float-points are applicable
    (if (= i 1) (matPow m)
      (matMul (matPow m (int (/ i 2))) 
              (matPow m (- i (int (/ i 2))))))))



;create rows of vectors of same value (for scalarMat)
(defn- scalarVec [i] 
  (cons i (lazy-seq (scalarVec i))))

;creates cols of vectors of same value (for scalarMul)
 (defn- scalarMatSeq[i c] 
      (cons (vec (take c (scalarVec i))) 
            (lazy-seq (scalarMatSeq i c))))
 
;takes the wanted size
(defn scalarMat [i c r] 
  {:pre [(integer? r) (> r 0) (integer? c) (> c 0)]}  
    (vec (take r (scalarMatSeq i c))))


;multiply a matrix by any number of scalars
(defn  scalarMul 
  ([m i] {:pre [(matrix? m) (number? i)]}
         (matFunc * m
                  (scalarMat i (cols m) (rows m))))
  ([m i & more] (if (every? number? more)
                  (reduce scalarMul 
                          (scalarMul m i) more)))   
)


;calls java's Math class
;Euclidean norm
(defn norm2 [m] {:pre [(matrix? m)]}
  ;sums a list using a lazy seq
  (defn- sumList "sums a list" [lst v]
    (cons v (lazy-seq (sumList (rest lst) (+ (first lst) v)))))
    ;square root of the sum of squares
  (java.lang.Math/sqrt (last (take (+ 1 (* (cols m) (rows m)))
                (sumList (flatten (matFunc * m m)) 0))))
    )

;normalizes a matrix using norm2
;only if norm2 is not 0
(defn normalize [m] {:pre [(not= 0.0 (norm2 m))]}
  (scalarMul m (/ 1 (norm2 m)))
 )


;returns the vector without the selected element
;aid function for det
(defn vdelete [v i] 
  (if (vector? v)
    (into (subvec v 0 i) (subvec v (+ i 1)))))


;returns the matrix without the column
;aid function for det
(defn erase-col [m j] {:pre [(matrix? m)]}
    (vec (map vdelete m (repeat j))))


;calculates determinant
(defn det [m]  {:pre [(square? m)]}
    (if (= (count m) 1)
      ((m 0) 0)
      ;reduction by definition
      (apply + 
             (map #(* (expt -1 %2) %1 
                      (det (erase-col (vdelete m 0) %2)))
                  (m 0) (iterate inc 0)))))

;inserts element into vector at the index specified
;aid function for det
(defn insert-row [v item i]
  (if (and (vector? v) (integer? i))
    (vec (into (conj (subvec v 0 i) item) (subvec v i)))))
;applies pred to the i-th item in vector v
;aid function for det
(defn vector_item [v pred i]
  (into (conj (subvec v 0 i) (pred (v i))) (subvec v (+ i 1))))

;applies pred to each item in m's i-th row
;aid function for det
(defn row_pred [m pred i]
  (if (and (matrix? m) (integer? i))
    (let [new_m (vdelete m i)]
      (insert-row new_m (map pred (m i)) i))))

;applies pred to each item in m's j-th col
;aid function for det
(defn col_pred [m pred j]
  (if (and (matrix? m) (integer? j))
    (map vector_item m (repeat pred) (repeat j))))

;lazy seq to create indexes for row/col for replace in setInd
;x is the number of rows/colmuns in the matrix
;n is desired row/col to change repectively
;i is a counter
(defn ind 
  ([x n] 
    {:pre [(integer? x) (>= x 0) (integer? n) (> x n)]} 
    (ind x n 0))
  ([x n i] (if (= i n) 
             (cons x (lazy-seq (ind x n (+ i 1))))
             (cons i (lazy-seq (ind x n (+ i 1))))
             ))
  )

;sets matrix cell at index i with number n
;the operation method is similar to permutation
(defn setInd [m ^java.lang.Integer i ^java.lang.Number n]
  {:pre [(matrix? m) (< i (* (rows m) (cols m))) (> i 0)
         (integer? i) (number? n)]}
  (try
    (let [c (rem i (cols m)) r (quot i (cols m))] 
      ;replace the selected index
      (replace 
        ;col parameters
        (conj m (replace (conj (get-row m r) n)
                         (vec (take (cols m) (ind (cols m) c)))))
        ;row parameters
        (vec (take (rows m) (ind (rows m) r))))
        )
    (catch IllegalArgumentException e nil)
    (catch ClassCastException e nil))
  )

