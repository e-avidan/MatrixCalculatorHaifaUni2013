(ns MatrixCalculator.core_test
  (:use MatrixCalculator.Functions clojure.test))

;3x5 matrix to test
(def mat1 (matrix [0 1 2 3 4] [5 6 7 8 9] [10 11 12 13 14]))

;5x3 matrix to test
(def mat2 (matrix [0 1 2] [3 4 5] [6 7 8] [9 10 11] [12 13 14]))

;5x5 matrix to test
(def mat3 (matrix [0 1 2 3 4] [5 6 7 8 9] [10 11 12 13 14] [15 16 17 18 19] [20 21 22 23 24]))

;symmetric matrix 5x5
(def mat4 (matrix [0 1 2 3 4] [1 6 7 8 9] [2 7 12 13 14] [3 8 13 18 19] [4 9 14 19 24]))

;identity matrix 5x5
(def I (matrix [1 0 0 0 0] [0 1 0 0 0] [0 0 1 0 0] [0 0 0 1 0] [0 0 0 0 1]))

(deftest test-matrix?
  ;empty matrix
  (is (= (matrix? [[][]])  false))
  (is (= (matrix? [])  false))
  ;not equally sized rows or columns
  (is (= (matrix? [[5][]])  false))
  (is (= (matrix? [[5][3 4]])  false))
  ;true matrix
  (is (= (matrix? [[538 8 6][7 3 5]])  true))
  ;(is (= (matrix? [3 4 5 6])  true))
  ;single number
  (is (= (matrix? 5)  false))
  ;list
  ;(is (= (matrix? (3 4 5))  false))
  ;test variables
  (is (= true (and (matrix? mat1) (matrix? mat2) (matrix? mat3))))
    )

(deftest test-matrix
 ;(is (= nil (matrix 5)))
  (is (= nil (matrix [])))
  (is (= [[5]] (matrix [5])))
  ;creates 3D matrix
  (is (not= [[3 4 5] [3 4 5]] (matrix [[3 4 5] [3 4 5]])))
  (is (= [[3 4 5] [3 4 5]] (matrix [3 4 5] [3 4 5])))
  (is (= (matrix(vec(flatten mat1))) (matrix(vec(flatten mat2)))))
  )

(deftest test-get-row 
  ;in range of matrix
  (is (= (get-row mat1 0) [0 1 2 3 4]))
  (is (= (get-row mat2 0) [0 1 2]))
  (is (= (get-row mat3 0) [0 1 2 3 4]))
  ;out of range
  (is (= (get-row mat1 5) nil))
  (is (= (get-row mat2 5) nil))
  (is (= (get-row mat3 5) nil))
  (is (= (get-row mat1 3) nil))
  )

(deftest test-tranpose
  ;double transpose
  (is (= (transpose (transpose mat3)) mat3))
  (is (= (transpose (transpose mat1)) mat1))
  (is (= (transpose (transpose mat2)) mat2))
  ;symmetric tranpose
  (is (= (transpose mat4) mat4))
  ;non-symmetric transpose
  (is (not= (transpose mat1) mat1))
  (is (not= (transpose mat2) mat2))
  (is (not= (transpose mat3) mat3))
  ;same size check
  (is (= (* (rows (transpose mat1)) (cols (transpose mat1)))
         (* (rows mat1) (cols mat1))))
  )


(deftest test-square?
  ;square
  (is (= true (square? mat3)))
  (is (= true (square? mat4)))
  ;non-square
  (is (= false (square? mat1)))
  (is (= false (square? mat2)))
  )

(deftest test-matFunc
  ;functions outside the allowed functions
  (is (= (matFunc = mat3 mat3) nil))
  (is (= (matFunc square? mat3 mat3) nil))
  ;even itself!
  (is (= (matFunc matFunc mat3 mat3) nil))
  ;multi-arity, distributivity and commutativity
  (is (= (matFunc + mat4 I mat4 mat4 I mat4)
         (matFunc + I (matFunc + mat4 mat4) 
                  (matFunc + mat4 (matFunc + I mat4)))))
  ;bad sizes assertion
  (is (thrown? AssertionError (matFunc + mat3 mat1 mat2)))
  )

(deftest test-matMul
  (is (= (matMul mat1 mat2) [[90 100 110] [240 275 310] [390 450 510]]))
  ;mismatching sizes - check assertion
  (is (thrown? AssertionError (matMul mat1 mat2 mat3 mat1 mat2)))
  ;non commutative on general matrices
  (is (not= (matMul mat3 mat4) (matMul mat4 mat3)))
  ;associativity
  (is (= (matMul (matMul mat3 mat2) mat1) 
         (matMul mat3 (matMul mat2 mat1))))
  ;distributive 1
  (is (= (matMul (matFunc + mat3 mat4) mat2) 
         (matFunc + (matMul mat3 mat2) (matMul mat4 mat2))))
  ;ditributive 2
  (is (= (matMul mat1 (matFunc + mat3 mat4))
         (matFunc + (matMul mat1 mat3) (matMul mat1 mat4))))
  ;tranpose
  (is (= (transpose (matMul mat1 mat2)) 
         (matMul (transpose mat2) (transpose mat1))))
  (is (= (transpose (matMul mat2 mat1)) 
         (matMul (transpose mat1) (transpose mat2))))
  ;identity matrix multiplication is commutative and returns same matrix
  (is (= (matMul mat3 I) (matMul I mat3) mat3 (matMul I I mat3 I I I)))
  ;multi-arity (not commutative!!!!!)
  (is (= (matMul mat4 mat3 mat4 mat4 mat3 mat4)
         (matMul mat4 (matMul mat3 mat4) 
                  (matMul mat4 (matMul mat3 mat4)))))
  (is (not= (matMul mat3 mat4 mat3 mat4 mat4 mat4)
         (matMul mat4 (matMul mat3 mat4) 
                  (matMul mat4 (matMul mat3 mat4)))))
  )
 
(deftest test-matPow
  ;non-square matrix - check for assertion
  (is (thrown? AssertionError (matPow mat1 3)))
  ;definition
  (is (= (matPow mat3 3) (matMul mat3 mat3 mat3)))
  ;commutative
  (is (= (matPow mat3 6) (matMul (matPow mat3 3) (matPow mat3 3))))
  ;(cA)^k=((c^k)*(A^k))
  (is (= (matPow (scalarMul mat4 8.0) 7) 
         (scalarMul (matPow mat4 7) (double(expt 8 7)))))
  ;identity matrix
  (is (= (matPow I 1000) I))
  ;distributive
  (is (= (matPow (matPow mat3 3) 2) (matPow (matPow mat3 2) 3)
         (matPow mat3 6)))
  )

(deftest test-scalarMul
  ;definition
  (is (= (scalarMul mat1 3) (matFunc + mat1 mat1 mat1)))
  (is (= (scalarMul mat1 -1) (matFunc - mat1 mat1 mat1)))
  ;allows double
  (is (not= nil (scalarMul mat1 2.5)))
  ;commutative
  (is (= (scalarMul mat1 3 7 5) (scalarMul mat1 3 5 7)
      (scalarMul mat1 5 7 3) (scalarMul mat1 5 3 7)
      (scalarMul mat1 7 5 3) (scalarMul mat1 7 3 5)))
  ;distributivity (over halves)
  (is (= (scalarMul mat1 2.5) (matFunc + mat1 mat1 (scalarMul mat1 0.5))))
  ;distributivity (over real numbers)
  (is (= (scalarMul mat1 java.lang.Math/PI)
         (matFunc + mat1 mat1 mat1 
                  (scalarMul mat1 (- java.lang.Math/PI 3)))))
  ;multi-arity and commutativity
  (is (= (scalarMul mat1 3 5 8 2 8 4) (scalarMul mat1 4 2 5 3 8 8)
         (scalarMul mat1 7680)))
  (is (= (scalarMul mat2 1.0) (scalarMul mat2 5 0.5 2 0.2 4 0.25)))
  )

(deftest test-norm2
  ;matrices with same elements
  (is (= (norm2 mat1) (norm2 mat2)))
  ;different elements
  (is (not= (norm2 mat1) (norm2 mat3)))
  ;identity matrix has norm equal to sqrt of it's size
  (is (= (norm2 I) (java.lang.Math/sqrt 5)))
  ;norm2(cA)==c*norm2(A)
  (is (= (norm2 (scalarMul mat2 57)) (* 57 (norm2 mat2))))
  ;norm2(A+B)<=(norm2(A)+norm2(B))
  (is (<= (norm2 (matFunc + mat3 mat4)) (+ (norm2 mat3) (norm2 mat4))))
  (is (<= (norm2 (matFunc + I mat4)) (+ (norm2 I) (norm2 mat4))))
  (is (<= (norm2 (matFunc + I mat3)) (+ (norm2 I) (norm2 mat3))))
  )

(deftest test-normalize
  ;defintion
  (is (= (normalize mat1) (scalarMul mat1 (/ 1 (norm2 mat1)))))
  ;norm = 1 after normalization
  (is (== 1 (norm2 (normalize mat1)) (norm2 (normalize mat2))))
  (is (= (norm2 (normalize mat3)) (norm2 (normalize mat4))))
  ;zero matrix - check assertion
  (is (thrown? AssertionError (normalize [[0 0 0]])))
  )

(deftest test-det
  ;det(A)^k=det(A^k)
  ;after 4 there is numeric deviation in the LSBs
  (is (== (det (matPow mat3 4)) (expt (det mat3) 4)))
  (is (== (det (matPow mat4 4)) (expt (det mat4) 4)))
  ;det(A)=det(A^T)
  (is (= (det mat3) (det (transpose mat3))))
  (is (= (det mat4) (det (transpose mat4))))
  ;det(AB)=det(A)det(B) is s(A)=s(B)
  (is (= (det (matMul mat3 mat4)) (* (det mat3) (det mat4))))
  (is (= (det (matMul mat4 mat3)) (* (det mat3) (det mat4))))
  ;det(cA)=det(A)*c^(rows(A))
  (is (= (det (scalarMul mat3 3)) (* (det mat3) (expt 3 (cols mat3)))))
  (is (= (det (scalarMul mat4 3)) (* (det mat4) (expt 3 (rows mat4)))))
  ;det(I) = 1
  (is (== (det I) 1))
  ;non-square - check assertion
  (is (thrown? AssertionError (matPow mat1 3)))
  (is (thrown? AssertionError (matPow mat2 3)))
  )

(deftest test-equallySized? 
  ;unequale sizes
  (is (= (equallySized? (list mat1 mat2 mat4)) false))
  ;equal sizes
  (is (= (equallySized? (list I mat4 mat3)) true))
  ;using other functions
  (is (= (equallySized? (list (matMul mat2 mat1) mat3)) true))
  ;matrix multiplication is not cummutable
  (is (= (equallySized? (list (matMul mat1 mat2) mat3)) false))
  )

(run-tests)