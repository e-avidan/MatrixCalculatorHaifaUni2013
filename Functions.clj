(ns MatrixCalculator.Functions
   (:use clojure.test))


;predicate, is this a vector of vectors - a matrix?
(defn matrix? [m]
  (if (or (number? m) (empty? m) (not (vector? m)) (and (not-every? vector? m) (not-every? number? m)) (some empty? m)) false
    (let [c (count (first m))]
      (if (every? #(= (count %) c) m) true false))))


;creates a matrix from a coll of vectors
;can create matrices with over 2 dimensions
(defn matrix [& v]
  (if (matrix? (vec v)) (vec v) nil))

;prints out a matrix in a form more easy for the user to read
;every function should be sent to this, for readability
;prints nil when done
(defn matPrint [m]
  (if (matrix? m) (print (apply str (interleave m (repeat "\n"))))) nil)


;gets the i-th row of a matrix (starts from 0)
(defn get-row [m i] 
   (if (and (matrix? m) (number? i))
     (try (m i)
       (catch IndexOutOfBoundsException e nil))))
 
;calculates x^y
(defn expt [x y]
     (java.lang.Math/pow x y))


;gets the j-th col of a matrix (starts from 0)
(defn get-col [m j] 
  (if (and (matrix? m) (number? j))
    (try
      (map #(% j) m) (vec (map #(% j) m))
      (catch IndexOutOfBoundsException _ nil))))


;counts rows
(defn rows 
  ([m] (rows m 0))
  ([m v] (if (nil? (get-row m v)) v (rows m (+ v 1))))
  )


;counts columns
(defn cols
  ([m] (cols m 0))
  ([m v] (if (nil? (get-col m v)) v (cols m (+ v 1))))
  )


;calculate transpose of matrix
(defn transpose [m]
  (if (matrix? m)
    (vec (map get-col (repeat (count (m 0)) m) (iterate inc 0)))))


;predicate, is a matrix square (num_rows==num_cols)
(defn square? [m]
     (and (matrix? m) (= (rows m) (cols m))))


;element by element actions
;called through matFunc
(defn vecFunc [f & v]
  (if (and (not-empty v) (every? vector? v)) (vec (reduce #(map f %1 %2) v))))


;; element by element actions
;; applicable functions are:
 ; + - element by element addition
 ; - - element by element substraction
 ; * - element by element multiplication
 ; / - element by element division
 ; expt - element by element power
;; if matrices sizes aren't the same results may vary!
;; GUI takes care of this (commitAction listener!)
;; has inlined lazyseq function
(defn matFunc [f & m] 
  (defn listVec "Turns a list of lists into a vector of vectors"
    [lst] (cons (vec (first lst)) 
                (lazy-seq (listVec (rest lst)))))
  (if (and (not-empty m) (every? matrix? m) (function? f)
           (or (= f -) (= f /) (= f *) (= f +) (= f expt)))  
    (let [lst (butlast(conj (apply vecFunc #(map f %1 %2) m) ()))]
      (vec(take (count (get-col (first m) 0)) (listVec lst)))) nil)
  )

;multiply two vectors (for matMul)
(defn vecMul [v1 v2]
  (if (and (vector? v1) (vector? v2) (= (count v1) (count v2)))
    (apply + (map * v1 v2))))


;multiply any number of matrices
(defn matMul 
  ([] nil)
  ([m] m)
  ([m1 m2] (if (and (matrix? m1) (matrix? m2) 
                    (= (count (m1 0)) (count m2))) 
             (let [tm2 (transpose m2)]
       (vec (map #(vec (map vecMul (repeat %) tm2)) m1))) nil))
  ([m1 m2 & more]
    (if (and (every? matrix? more) (matrix? m1) (matrix? m2))
     (reduce matMul (matMul m1 m2) more)))
  ) 

;multiply matrix by itself i times
(defn matPow 
  ([] nil)
  ([m] (if (matrix? m) m nil))
  ([m i] (if (= i 1) (matPow m)
           (if (and (square? m) (> i 0) (= (int i) i))
             (matMul (matPow m (int (/ i 2))) 
                     (matPow m (- i (int (/ i 2)))))
             nil
             )
           )
         )
  )
  
 
;create rows of vectors of same value (for scalarMat)
(defn scalarVec [i] (cons i (lazy-seq (scalarVec i))))

 
;creates cols of vectors of same value (for scalarMul)
(defn scalarMat [i r] (cons (vec (take r (scalarVec i))) 
                            (lazy-seq (scalarMat i r))))
 
 
;multiply a matrix by any number of scalars
(defn  scalarMul
  ([m i] (if(and (matrix? m) (number? i))
            (matFunc * m (vec 
                  (take (rows m)
                     (scalarMat i (cols m))
                     )))
            nil
            )
   )
   ([m i & more] (if (and (matrix? m) (number? i) (not-empty more) 
                       (every? number? more))
                   (reduce scalarMul (scalarMul m i) more)))   
)

;calls java's Math class
;Euclidean norm
(defn norm2 [m]
  (defn sumList "sums a list" [lst v] (cons v (lazy-seq (sumList (rest lst) (+ (first lst) v)))))
  (if (matrix? m)
      (java.lang.Math/sqrt (last (take (+ 1 (* (cols m) (rows m))) (sumList (flatten (matFunc * m m)) 0))))
    )
)


;normalizes a matrix using norm2
(defn normalize [m] 
  (scalarMul m (/ 1 (norm2 m)))
 )



(comment ???)
(defn vdelete [v i]
  (if (vector? v)
    (into (subvec v 0 i) (subvec v (+ i 1)))))


(comment ???)
(defn erase-col [m j]
  (if (matrix? m)
    (vec (map vdelete m (repeat j)))))


(comment calculates determinant)
(defn det [m]
  (if (square? m)
    (if (= (count m) 1)
      ((m 0) 0)
      (apply + (map #(* (expt -1 %2) %1 (det (erase-col (vdelete m 0) %2))) (m 0) (iterate inc 0))))))


(comment ???)
(defn insert-row [v item i]
  (if (and (vector? v) (integer? i))
    (vec (into (conj (subvec v 0 i) item) (subvec v i)))))


(comment ???)
(defn vector_item [v pred i]
  (into (conj (subvec v 0 i) (pred (v i))) (subvec v (+ i 1))))


(comment ???)
(defn row_pred [m pred i]
  (if (and (matrix? m) (integer? i))
    (let [new_m (vdelete m i)]
      (insert-row new_m (map pred (m i)) i))))


(comment ???)
(defn col_pred [m pred j]
  (if (and (matrix? m) (integer? j))
    (map vector_item m (repeat pred) (repeat j))))
