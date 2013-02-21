(ns MatrixCalculator.test
  (:use clojure.test
        MatrixCalculator.Functions))

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
  ;same size
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
  (is (= (matFunc = mat1 mat2) nil))
  (is (= (matFunc square? mat1 mat2) nil))
  ;even itself!
  (is (= (matFunc matFunc mat1 mat2) nil))
  )

(deftest test-matMul
  (is (= (matMul mat1 mat2) [[90 100 110] [240 275 310] [390 450 510]]))
  ;mismatching sizes
  (is (= nil (matMul mat1 mat2 mat3 mat1 mat2)))
  ;non commutative on general matrices
  (is (not= (matMul mat3 mat4) (matMul mat4 mat3)))
  ;identity matrix multiplication is commutative and returns same matrix
  (is (= (matMul mat3 I) (matMul I mat3) mat3 (matMul I I mat3 I I I)))
  )
  
(deftest test-matPow
  ;non-square matrix
  (is (= (matPow mat1 3) nil))
  ;definition
  (is (= (matPow mat3 3) (matMul mat3 mat3 mat3)))
  ;commutative
  (is (= (matPow mat3 6) (matMul (matPow mat3 3) (matPow mat3 3))))
  ;identity matrix
  (is (= (matPow I 1000) I))
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
  ;disjunction (over halves)
  (is (= (scalarMul mat1 2.5) (matFunc + mat1 mat1 (scalarMul mat1 0.5))))
  ;disjunction (over real numbers)
  (is (= (scalarMul mat1 java.lang.Math/PI) (matFunc + mat1 mat1 mat1 (scalarMul mat1 (- java.lang.Math/PI 3)))))
  )

(deftest test-norm2
  ;matrices with same elements
  (is (= (norm2 mat1) (norm2 mat2)))
  ;different elements
  (is (not= (norm2 mat1) (norm2 mat3)))
  ;identity matrix has norm equal to sqrt of it's size
  (is (= (norm2 I) (java.lang.Math/sqrt 5)))
  )

(deftest test-normalize
  ;defintion
  (is (= (normalize mat1) (scalarMul mat1 (/ 1 (norm2 mat1)))))
  ;norm = 1 after normalization
  (is (== 1 (norm2 (normalize mat1)) (norm2 (normalize mat2))))
  (is (= (norm2 (normalize mat3)) (norm2 (normalize mat4))))
  )
(deftest test-det
  ;det(A)^k=det(A^k)
  ;after 4 there is numeric deviation in the LSBs
  (is (= (det (matPow mat3 4)) (expt (det mat3) 4)))
  (is (= (det (matPow mat4 4)) (expt (det mat4) 4)))
  ;det(A)=det(A^T)
  (is (= (det mat3) (det (transpose mat3))))
  (is (= (det mat4) (det (transpose mat4))))
  ;det(AB)=det(A)det(B) is s(A)=s(B)
  (is (= (det (matMul mat3 mat4)) (* (det mat3) (det mat4))))
  (is (= (det (matMul mat4 mat3)) (* (det mat3) (det mat4))))
  ;det(cA)=det(A)*c^sqrt(s(A))
  (is (= (det (scalarMul mat3 3)) (* (det mat3) (expt 3 5))))
  (is (= (det (scalarMul mat4 3)) (* (det mat4) (expt 3 5))))
  ;det(I) = 1
  (is (== (det I) 1))
  ;non-square
  (is (= nil (det mat1)))
  (is (= nil (det mat2)))
  )

(run-tests)