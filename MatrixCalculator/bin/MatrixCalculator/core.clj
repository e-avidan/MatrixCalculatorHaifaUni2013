(ns MatrixCalculator.core
  (:use MatrixCalculator.GUI)
  (import javax.swing.SwingUtilities)
  )

(defn -main
  "I don't do a whole lot. No really, I just call the GUI function.
   You should name her main!"
  [&args]
  ;invokeLater makes swing thread safe!
  (SwingUtilities/invokeLater gui-main))

