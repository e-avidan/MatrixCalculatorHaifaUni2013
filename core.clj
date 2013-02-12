(ns MatrixCalculator.core
  (:use MatrixCalculator.GUI))

(defn -main
  "I don't do a whole lot."
  [&args]
  (
    (GUI)
    (def mat4 [[5 5 5] [3 3 3] [1 1 1]])
    (matFunc + mat4)
    )
)

