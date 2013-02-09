(ns MatrixCalculator)


(comment predicate, is this a vector of vectors - a matrix?)
(defn matrix? [m]
  (if (or (not (vector? m)) (not-every? vector? m) (some empty? m)) false
    (let [c (count (first m))]
      (if (every? #(= (count %) c) m) true false))))


(comment creates a matrix from a coll of vectors)
(defn matrix [& v]
  (if (matrix? (vec v)) (vec v) false))


(comment prints out a matrix in a form more easy for the user to read)
(comment every function should be sent to this, for readability)
(comment prints nil when done)
(defn matPrint [m]
     (print (apply str (interleave m (repeat "\n")))))


(comment gets the i-th row of a matrix (starts from 0))
 (defn get-row [m i] 
   (if (and (matrix? m) (number? i))
     (try (m i)
     (catch IndexOutOfBoundsException e nil))))
 

(comment gets the j-th col of a matrix (starts from 0))
(defn get-col [m j] 
  (if (and (matrix? m) (number? j)) (try
    (map #(% j) m) (vec (map #(% j) m))
    (catch IndexOutOfBoundsException _ nil))))


(comment counts rows)
(defn rows 
  ([m] (rows m 0))
  ([m v] (if (nil? (get-row m v)) v (rows m (+ v 1))))
)


(comment counts columns)
(defn cols
  ([m] (cols m 0))
  ([m v] (if (nil? (get-col m v)) v (cols m (+ v 1))))
)


(comment calculate transpose of matrix)
(defn transpose [m]
  (if (matrix? m)
    (vec (map get-col (repeat (count (m 0)) m) (iterate inc 0)))))
 



(comment need to check sizes!!)
(comment element by element actions)
(defn vecFunc [f & v]
  (if (and (not-empty v) (every? vector? v)) (vec (reduce #(map f %1 %2) v))))
  

(comment need to check sizes!!)
(comment element by element actions)
(defn matFunc [f & m] 
  (defn listVec "Turns a list of lists into a vector of vectors"
     [lst] (cons (vec (first lst)) 
                             (lazy-seq (listVec (rest lst)))))
    (if (and (not-empty m) (every? matrix? m))  
       (let [lst (butlast(conj (apply vecFunc #(map f %1 %2) m) ()))]
          (vec(take (count (get-col (first m) 0)) (listVec lst)))))
)




(comment multiply two vectors (for matMul))
(defn vecMul [v1 v2]
  (if (and (vector? v1) (vector? v2) (= (count v1) (count v2)))
    (apply + (map * v1 v2))))


(comment multiply any number of matrices)
 (defn matMul 
  ([] nil)
  ([m] m)
  ([m1 m2] (if (and (matrix? m1) (matrix? m2) 
                    (= (count (m1 0)) (count m2))) 
     (let [tm2 (transpose m2)]
       (vec (map #(vec (map vecMul (repeat %) tm2)) m1)))))
  ([m1 m2 & more]
   (if (and (every? matrix? more) (matrix? m1) (matrix? m2))
     (reduce matMul (matMul m1 m2) more)))
  ) 

 
 
 
 (comment create rows of vectors of same value (for scalarMat))
 (defn scalarVec [i] (cons i (lazy-seq (scalarVec i))))
 
 
 (comment creates cols of vectors of same value (for scalarMul))
 (defn scalarMat [i r] (cons (vec (take r (scalarVec i))) 
                             (lazy-seq (scalarMat i r))))


(comment multiply a matrix by any number of scalars)
(defn  scalarMul
   ([m i] (if(and (matrix? m) (number? i))
            (matFunc * m (vec 
                  (take (rows m)
                     (scalarMat i (cols m))
                     )))
            )
   )
   ([m i & more] (if (and (matrix? m) (number? i) (not-empty more) 
                       (every? number? more))
                   (reduce scalarMul (scalarMul m i) more)))   
)
 

(comment add constant scalar to matrix)
(comment all numbers lesser than 1 (including negatives) are added)
(comment normally, but n>=1 are added using trampoline to show)
(comment clojure's ability to recurse in different ways)
(defn scalarAdd
   ([m x] (trampoline(fn [] (scalarAdd m x (fn [m]
     (let [y m]
       (fn [z] (matFunc + z  (vec(take (rows y) (scalarMat 0.5 (cols y))))))))))))
  
   ([m x f] (if (< x 1)  
      (matFunc + m  (vec(take (rows m) (scalarMat x (cols m)))))
     (trampoline (fn [] ((def addScal (f m))
     (scalarAdd (addScal m) (- x 1) f))))))
)


(comment need to sum current, and get sqrt somehow)
(comment calls java's Math class)
(defn norm2 [m]
  (defn sumList "sums a list" [lst v] (cons v (lazy-seq (sumList (rest lst) (+ (first lst) v)))))
  (if (matrix? m)
      (java.lang.Math/sqrt (last (take (+ 1 (* (cols m) (rows m))) (sumList (flatten (matFunc * m m)) 0))))
    )
)


(comment normalizes a matrix using norm2)
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


(comment predicate, is a matrix square (num_rows==num_cols))
(defn square? [m]
     (and (matrix? m) (= (rows m) (cols m))))


(comment ???)
(defn expt [x y]
     (if (= y 0) 1
       (* x (expt x (- y 1)))))


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


  
  
 (comment This area is for testing!!!!)
 
 
 (def mat1 [
               [1 0 0 1]
               [0 1 0 1]
               [0 0 4 2]
               [5 3 2 3]
               [3 1 0 3]
                ])
 (def I [
               [1 0 0 0]
               [0 1 0 0]
               [0 0 1 0]
               [0 0 0 1]
                ])
 
 (def s [[5]])
 (matFunc + mat1 mat1 mat1)
 (matrix? mat1)
 (transpose mat1)
 (get-col mat1 2)
 (get-row mat1 1)
 (det I)
(transpose mat1)
(vecFunc + (get-row mat1 1) (get-col mat1 2))
(get-row mat1 5)
(scalarMul mat1 2 4)
(matMul s s)
(norm2 mat1)
(matPrint (scalarMul mat1 4 0.5 1 -0.25 2))
(scalarAdd I 15)
(matFunc * mat1 mat1)
(get (get (matFunc * mat1 mat1) 3) 1)
(cols mat1)
(matFunc + mat1 (vec(take (rows mat1) (scalarMat 1 (cols mat1)))))
  (defn make-adder [m]
    (let [y m]
      (fn [z] (matFunc + z  (vec(take (rows y) (scalarMat 1 (cols y))))))))
   (def addScal (make-adder I))
(addScal I)
(addScal (addScal I))
(scalarMul mat1 0)