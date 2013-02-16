(ns MatrixCalculator.GUI
  (:use MatrixCalculator.Functions)
  (:use MatrixCalculator.Contributed)
  (:import 
    (javax.swing JPanel, JFrame, JButton, JTextField,
                 JLabel, JComboBox)
    (java.awt.event KeyEvent, KeyAdapter)
    (java.awt GridLayout, Color, Dimension, BorderLayout)
    (java.text NumberFormat)
    (java.lang Exception)
    )
  )

;turn string into double or return nil if not possible
  ;if a number is extractable from the start, even if non-numeric
  ;characters appear later, it is extracted, so slips on the keyboard
  ;are forgiven
(defn toDouble [str]
  (
    try (.parse (NumberFormat/getNumberInstance) str)
    (catch Exception e nil))
  )  

(defn gui-main "the GUI" [] (         
  (def frame (JFrame. "Matrix Calculator Motherfucker!"))
  
  ;control variables
  ; :d-destination (lower 2) matrices, :s-source (upper 2) matrices
  ; :dst :src :act :scalar - current actions on panel 1 - index
  ; :k- keywords
  ; :r :c - dimensions of the source matrices, respectively
  ;    (1 on the left 2 on the right)
  ; :rd :cd - dimensions of destination matrices 
  ; :nil- constants for keyword control
  (def vars (ref (zipmap 
              [:dst :src1 :act :src2 :scalar
               :kd :krd :kcd :ks1 :kr1 :kc1 :ks2 :kr2 :kc2
               :s1 :s2 :r1 :c1 :r2 :c2 
               :d1 :d2  :rd1 :cd1 :rd2 :cd2
               :nilM :nilR :nilC] 
              [0 0 0 0 1 
               :d1 :rd1 :cd1 0 0 0 0 0 0 
               nil nil 6 2 5 6
               nil nil 0 0 0 0
               nil 0 0]))) 
  ;status of GUI input and output
  ;for the dst, we need to set more than the constructor allows
  ;and we define a variable number of JTextFields so we need this
   (def txtDst1 (ref {}))
   (def txtDst2 (ref {}))
   (def txtSrc1 (ref {}))
   (def txtSrc2 (ref {}))
   
   ;reset the source matrices (dst are not yet made)
   (dosync 
     (let [r (java.util.Random.)]
       (alter vars assoc :r1 (+ (mod (.nextInt r) 9) 1))
       (alter vars assoc :c1 (+ (mod (.nextInt r) 9) 1))
       (alter vars assoc :r2 (+ (mod (.nextInt r) 9) 1))
       (alter vars assoc :c2 (+ (mod (.nextInt r) 9) 1))
       )
     ) 
   (dosync 
	   (alter vars assoc :s1
	     (vec(take (vars :c1)
	               (scalarMat 2 (vars :r1))))))

   (dosync 
	   (alter vars assoc :s2
	     (vec(take (vars :c2)
	               (scalarMat 3 (vars :r2))))))
   
  ;window settings
  (def x 800)
  (def y 480)
	(.setSize frame x y)
  ;so the user doesn't spoil his experience...
  ;BorderLayout isn't smart enough, but since GUI isn't a must
  ;we preffered not to delve into GridBagLayout this time..
  (.setMinimumSize frame (Dimension. x y)) 
  (.setLayout frame (BorderLayout. 0 0))
  (.setLocationRelativeTo frame (.getRootPane frame))
	(.setVisible frame true)
 
  ;panels inside the window
  ;control panel
  (def panel1 (JPanel.))
  ;source matrix 1
  (def panel2 (ref (JPanel.)))
  ;source matrix 2
  (def panel3 (ref (JPanel.)))

  ;destination matrices panels
  (def w (ref (JPanel.)))
  (def e (ref (JPanel.)))
  
  ;central panel - has all 4 src and dst matrices, for equal stretch
  (def panel4 (JPanel.))
  
    ;window populating
  (.add frame panel1 BorderLayout/NORTH)
  (.add panel4 (deref panel2))
  (.add panel4 (deref panel3))
  (.add panel4 (deref w))
  (.add panel4 (deref e))
  (.add frame panel4 BorderLayout/CENTER)
  
  (def p1 80)
  (.setPreferredSize panel1 (Dimension. x p1))
  (.setPreferredSize (deref panel2) (Dimension. (/ x 2) (/ (- y p1) 2)))
  (.setPreferredSize (deref panel3) (Dimension. (/ x 2) (/ (- y p1) 2)))
  (.setPreferredSize (deref w) (Dimension. (/ x 2) (/ (- y p1) 2)))
  (.setPreferredSize (deref e) (Dimension. (/ x 2) (/ (- y p1) 2)))
  
  ;so it looks good and you can see the different areas
  (.setBackground panel1 (Color. 0, 255, 0))
  (.setBackground (deref panel2) (Color. 255, 0, 0))
  (.setBackground (deref panel3) (Color. 0, 0, 200))
  (.setBackground (deref w) (Color. 250, 200, 0))
  (.setBackground (deref e) (Color. 150, 00, 150))
    
   ;textfields for matrix size input
  (def r1 (ref (JTextField. (.toString (vars :r1)))))
  (def c1 (ref (JTextField. (.toString (vars :c1)))))
  (def r2 (ref (JTextField. (.toString (vars :r2)))))
  (def c2 (ref (JTextField. (.toString (vars :c2)))))
  
  ;reference for changeSize and changeScalar.. clojrue allows this so why not..
  ;it's updated upon entrance to the listener so access is not an
  ;issue
  (def d (ref nil))
  
  ;combo boxes to choose actions
  (def ComboDst (JComboBox. 
                  (to-array 
                    (list "LeftOutput" "RightOutput"))))
  (def ComboSrc1 (JComboBox. 
                   (to-array
                     (list "" "LeftInput" "RightInput" 
                           "LeftOutput" "RightOutput"))))
  (def ComboAct (JComboBox.
                  (to-array 
                    (list "" "+" "-" ".*" "./" ".^" "*"
                          "^" "ScalarMatrix with"
                          "Euclidean norm of" "Determinant of" 
                          "Normalize" "Transpose of"))))
  ;scalarInput is here for syntax and meaning, it's logical to humans
  (def ComboSrc2 (JComboBox. (to-array
                               (list "" "LeftInput" "RightInput" 
                                     "LeftOutput" "RightOutput" 
                                     "Scalar" ))))
  (def Scalar (JTextField. (.toString (vars :scalar))))
  (.setPreferredSize Scalar (Dimension. 30 30))
  (def Commit (JButton. "Commit!"))
  
  ;; combo box's listeners
  ;updates vars when normal combo boxes change
  (defn comboUpdate [e component str]
    (dosync 
      (alter vars assoc (keyword str) 
             (.getSelectedIndex component))
      
      (.setText (deref r1) (.toString ((deref vars) :r1)))
      (.setText (deref c1) (.toString ((deref vars) :c1)))
      (.setText (deref r2) (.toString ((deref vars) :r2)))
      (.setText (deref c2) (.toString ((deref vars) :c2)))
      (.setText Scalar (.toString ((deref vars) :scalar)))
      
      ;dst matrix set
      (if (= (vars :dst) 0)
        (
          (alter vars assoc :kd (keyword "d1"))
          (alter vars assoc :krd (keyword "rd1"))
          (alter vars assoc :kcd (keyword "cd1"))
          )
        (
          (alter vars assoc :kd (keyword "d2"))
          (alter vars assoc :krd (keyword "rd2"))
          (alter vars assoc :kcd (keyword "cd2"))
          )
        )
      ;src1 matrix set
      (if (= (vars :src1) 0)
        (
          (alter vars assoc :ks1 (keyword "nilM"))
          (alter vars assoc :kr1 (keyword "nilR"))
          (alter vars assoc :kc1 (keyword "nilC"))
          )
        )
      (if (= (vars :src1) 1)
        (
          (alter vars assoc :ks1 (keyword "s1"))
          (alter vars assoc :kr1 (keyword "r1"))
          (alter vars assoc :kc1 (keyword "c1"))
          )
        )
      (if (= (vars :src1) 2)
        (
          (alter vars assoc :ks1 (keyword "s2"))
          (alter vars assoc :kr1 (keyword "r2"))
          (alter vars assoc :kc1 (keyword "c2"))
          )
        )
      (if (= (vars :src1) 3)
        (
          (alter vars assoc :ks1 (keyword "d1"))
          (alter vars assoc :kr1 (keyword "rd1"))
          (alter vars assoc :kc1 (keyword "cd1"))
          )
        )
      (if (= (vars :src1) 4)
        (
          (alter vars assoc :ks1 (keyword "d2"))
          (alter vars assoc :kr1 (keyword "rd2"))
          (alter vars assoc :kc1 (keyword "cd2"))
          )
        )
      ;src2 matrix set
      (if (= (vars :src2) 0)
        (
          (alter vars assoc :ks2 (keyword "nilM"))
          (alter vars assoc :kr2 (keyword "nilR"))
          (alter vars assoc :kc2 (keyword "nilC"))
          )
        )
      (if (= (vars :src2) 1)
        (
          (alter vars assoc :ks2 (keyword "s1"))
          (alter vars assoc :kr2 (keyword "r1"))
          (alter vars assoc :kc2 (keyword "c1"))
          )
        )
      (if (= (vars :src2) 2)
        (
          (alter vars assoc :ks2 (keyword "s2"))
          (alter vars assoc :kr2 (keyword "r2"))
          (alter vars assoc :kc2 (keyword "c2"))
          )
        )
      (if (= (vars :src2) 3)
        (
          (alter vars assoc :ks2 (keyword "d1"))
          (alter vars assoc :kr2 (keyword "rd1"))
          (alter vars assoc :kc2 (keyword "cd1"))
          )
        )
      (if (= (vars :src2) 4)
        (
          (alter vars assoc :ks2 (keyword "d2"))
          (alter vars assoc :kr2 (keyword "rd2"))
          (alter vars assoc :kc2 (keyword "cd2"))
          )
        )
      ;show of scalar box
      (if (= (vars :src2) 5) 
        (.setVisible Scalar true)
        (.setVisible Scalar false))
      (.revalidate panel1)
      )
    )
  
  
  ;element by element actions
  (defn eleByele [f]                     
    (if (and (= (vars (vars :kr1)) (vars (vars :kr2)))
      (= (vars (vars :kc1)) (vars (vars :kc2))))
      (
        ;change vars
        (alter vars assoc (vars :kd)
               (matFunc f (vars (vars :ks1)) (vars (vars :ks2))))   
        (alter vars assoc (vars :krd) (vars (vars :kr2)))
        (alter vars assoc (vars :kcd) (vars (vars :kc2)))
        )
      )
    )
  
  
  ;updates vars when scalar box is changed
  (defn changeScalar [e]
    (dosync (ref-set d (toDouble (.getText Scalar)))
        (try
          (if (nil? (deref d))      
            (;restore val - is not parsable  
              (.setText (deref r1) (.toString ((deref vars) :r1)))
              (.setText (deref c1) (.toString ((deref vars) :c1)))
              (.setText (deref r2) (.toString ((deref vars) :r2)))
              (.setText (deref c2) (.toString ((deref vars) :c2)))
              (.setText Scalar (.toString ((deref vars) :scalar)))
              )
            (
              ;; is parseable
              ;set new val
              (alter vars assoc :scalar (deref d))
              ;restore fields
              (.setText (deref r1) (.toString ((deref vars) :r1)))
              (.setText (deref c1) (.toString ((deref vars) :c1)))
              (.setText (deref r2) (.toString ((deref vars) :r2)))
              (.setText (deref c2) (.toString ((deref vars) :c2)))
              (.setText Scalar (.toString ((deref vars) :scalar)))                           
              )        
            )
          ;helps!
          (catch Exception ex nil))
        ) 
    )
  
  (defn commitAction [e] 
    (dosync 
      (try
        (
          (.setText (deref r1) (.toString ((deref vars) :r1)))
          (.setText (deref c1) (.toString ((deref vars) :c1)))
          (.setText (deref r2) (.toString ((deref vars) :r2)))
          (.setText (deref c2) (.toString ((deref vars) :c2)))
          (.setText Scalar (.toString ((deref vars) :scalar)))
          ;put a matrix into the destination specified
          (if (and (not= (vars :src1) 0) (= (vars :act) 0)
                   (= (vars :src2 0))) 
            (
              ;change vars
              (alter vars assoc (vars :kd)
                     (vars (vars :ks1)))  
              (alter vars assoc (vars :krd) (vars (vars :kr1)))
              (alter vars assoc (vars :kcd) (vars (vars :kc1)))
              )
            )
          ;;element by element dual matrix operation
          ;addition
          (if (= (vars :act) 1) 
            (eleByele +))
          
          ;substraction
          (if (= (vars :act) 2) 
            (eleByele -))
          
          ;point multiplication
          (if (= (vars :act) 3) 
            (eleByele *))
          
          ;point division
          (try 
            (if (= (vars :act) 4) 
              (eleByele /))
            ;devide by zero
            (catch Exception ex nil)
            )
          
          ;point power
          (try
            (if (= (vars :act) 5) 
              (eleByele expt))
            ;out of range
            (catch Exception ex nil)
            ) 
          
          ;;scalar (and possibly matrix) operations
          ;multiplication
          (if (= (vars :act) 6) 
            (if (= (vars :src2) 5)
              ;scalar
              (
                ;change vars
                (alter vars assoc (vars :kd)
                       (scalarMul (vars (vars :ks1)) (vars :scalar)))   
                (alter vars assoc (vars :krd) (vars (vars :kr1)))
                (alter vars assoc (vars :kcd) (vars (vars :kc1)))
                )
              ;matrix
              (
                ;change vars
                (alter vars assoc (vars :kd)
                       (matMul (vars (vars :ks1)) (vars (vars :ks2))))   
                (alter vars assoc (vars :krd) (vars (vars :kr1)))
                (alter vars assoc (vars :kcd) (vars (vars :kc2)))
                )
              )
            )
          
          ;power (only with scalar!)
          (if (= (vars :act) 7)
            (if (= (vars :src2) 5)
              (
                ;change vars
                (alter vars assoc (vars :kd)
                       (matPow (vars (vars :ks1)) (vars :scalar)))   
                (alter vars assoc (vars :krd) (vars (vars :kr1)))
                (alter vars assoc (vars :kcd) (vars (vars :kc1)))
                )
              )
            )
          
          ;scalar matrix (only with scalar!)
          (if (= (vars :act) 8) 
            (if (= (vars :src2) 5)
              (
                ;change vars
                (alter vars assoc (vars :kd)
                       (take (vars (vars :kc1)) 
                             (scalarMat  (vars :scalar) (vars (vars :kr1))))) 
                (alter vars assoc (vars :krd) (vars (vars :kr1)))
                (alter vars assoc (vars :kcd) (vars (vars :kc1)))
                )
              )
            )
   
          ;;Right source only
          ;Euclidean norm
          (if (= (vars :act) 9) 
            (if (and (= (vars :src1) 0) (> (vars :src2) 0) (< (vars :src2) 5))
              (
                ;change vars
                (alter vars assoc (vars :kd)
                       (matrix(vector(norm2  (vars (vars :ks2))))))
                (alter vars assoc (vars :krd) 1)
                (alter vars assoc (vars :kcd) 1)
                )
              )
            )
          
          ;Determinant
          (if (= (vars :act) 10) 
            (if (and (= (vars :src1) 0) (> (vars :src2) 0) (< (vars :src2) 5))
              (
                ;change vars
                (alter vars assoc (vars :kd)
                       (matrix (vector (det  (vars (vars :ks2)))))) 
                (alter vars assoc (vars :krd) 1)
                (alter vars assoc (vars :kcd) 1)
                )
              )
            )
          
          ;Normalize
          (if (= (vars :act) 11) 
            (if (and (= (vars :src1) 0) (> (vars :src2) 0) (< (vars :src2) 5))
              (
                ;change vars
                (alter vars assoc (vars :kd)
                       (normalize (vars (vars :ks2))))
                (alter vars assoc (vars :krd) (vars (vars :kr2)))
                (alter vars assoc (vars :kcd) (vars (vars :kc2)))
                )
              )
            )
          
          ;Transpose
          (if (= (vars :act) 12) 
            (if (and (= (vars :src1) 0) (> (vars :src2) 0) (< (vars :src2) 5))
              (
                ;change vars
                (alter vars assoc (vars :kd)
                       (transpose  (vars (vars :ks2))))
                (alter vars assoc (vars :krd) (vars (vars :kc2)))
                (alter vars assoc (vars :kcd) (vars (vars :kr2)))
                )
              )
            )
               
          (if (= (vars :dst) 0)
            (
              (.removeAll (deref w)) 
              (.setLayout (deref w) (GridLayout. (vars :rd1) (vars :cd1)))
              (ref-set txtDst1 {})
              (dotimes [n (* (vars :rd1) (vars :cd1))] 
                (try
                  ((alter txtDst1 assoc (keyword (.toString n))
                         (JTextField. (.toString (get (vec (flatten (vars :d1))) n))))
                  (.setEditable (txtDst1 (keyword (.toString n))) false)
                  (.setHorizontalAlignment 
                    (txtDst1 (keyword (.toString n))) JTextField/CENTER)
                  (.setCaretPosition (txtDst1 (keyword (.toString n))) 0)
                  (.add (deref w) (txtDst1 (keyword (.toString n))))
                  (.revalidate panel4))
                  (catch Exception ex nil)
                  )
                )
              )
           (
             (.removeAll (deref e)) 
             (.setLayout (deref e) (GridLayout. (vars :rd2) (vars :cd2)))
             (ref-set txtDst2 {})
             (dotimes [n (* (vars :rd2) (vars :cd2))] 
               (try
                 ((alter txtDst2 assoc (keyword (.toString n))
                         (JTextField. (.toString (get (vec (flatten (vars :d2))) n))))
                   (.setEditable (txtDst2 (keyword (.toString n))) false)
                   (.setHorizontalAlignment 
                     (txtDst2 (keyword (.toString n))) JTextField/CENTER)
                   (.setCaretPosition (txtDst2 (keyword (.toString n))) 0)
                   (.add (deref e) (txtDst2 (keyword (.toString n))))
                   (.revalidate panel4))
                 (catch Exception ex nil)
                 )
               )
             )
           )
          )
        (catch Exception ex nil)
        )
      )
    )
  
  ;combo box's listeners addition
  (add-action-listener ComboDst comboUpdate ComboDst "dst")
  (add-action-listener ComboSrc1 comboUpdate ComboSrc1 "src1")
  (add-action-listener ComboAct comboUpdate ComboAct "act")
  (add-action-listener ComboSrc2 comboUpdate ComboSrc2 "src2")
  (add-key-released-listener Scalar changeScalar)
  (add-action-listener Commit commitAction)
  
  ;layouts of main panels
  (.setLayout (deref panel2)(GridLayout. (vars :r1) (vars :c1)) )
  (.setLayout (deref panel3) (GridLayout. (vars :r2) (vars :c2)))
  (.setLayout panel4 (GridLayout. 2 2))
    
      ;populating top control panel
  (.add panel1 (JLabel. "rows_mat1:"))
  (.add panel1 (deref r1))
  (.add panel1 (JLabel. "cols_mat1:"))
  (.add panel1 (deref c1))
  
  (.add panel1 (JLabel. " ||||| "))
  (.add panel1 ComboDst)
  (.add panel1 (JLabel. "="))
  (.add panel1 ComboSrc1)
  (.add panel1 ComboAct)
  (.add panel1 ComboSrc2)
  ;no need for scalar box at this time
  (.setVisible Scalar false)
  (.add panel1 Scalar)
  (.add panel1 Commit)

  (.add panel1 (JLabel. " ||||| "))
  (.add panel1 (JLabel. "rows_mat2:"))
  (.add panel1 (deref r2))
  (.add panel1 (JLabel. "cols_mat2:"))
  (.add panel1 (deref c2))
  
  (defn changeSize "change size of matrix listener" [e component str] 
    (if (= (.getKeyCode e) (KeyEvent/VK_ENTER))  
      (dosync (ref-set d (toDouble (.getText component)))
        (try
          (if (nil? (deref d))
	         (
	           ;restore val - is not parsable
	           (.setText (deref r1) (.toString ((deref vars) :r1)))
            (.setText (deref c1) (.toString ((deref vars) :c1)))
            (.setText (deref r2) (.toString ((deref vars) :r2)))
            (.setText (deref c2) (.toString ((deref vars) :c2)))
            (.setText Scalar (.toString ((deref vars) :scalar)))
	           )
	        
		        ;is parseable
		        (if (> (int (deref d)) 0)
		          (
                ;needed!
                (if (= str "null")
                   (alter vars assoc :r1 (deref d)))
		            (if (= str "r1" )
		              (alter vars assoc :r1 (deref d)))
		            (if (= str "c1" )
		              (alter vars assoc :c1 (deref d)))
		            (if (= str "r2" )
		              (alter vars assoc :r2 (deref d)))
		            (if (= str "c2" )
		              (alter vars assoc :c2 (deref d)))
              
              (.setText (deref r1) (.toString ((deref vars) :r1)))
              (.setText (deref c1) (.toString ((deref vars) :c1)))
              (.setText (deref r2) (.toString ((deref vars) :r2)))
              (.setText (deref c2) (.toString ((deref vars) :c2)))
              (.setText Scalar (.toString ((deref vars) :scalar)))
            
		            (if (or (= str "r1") (= str "c1")) 
	               	;first src
		              (alter vars assoc :s1
		                     (vec(take ((deref vars) :c1)
		                               (scalarMat 0 ((deref vars) :r1))))) 
		              ;second src
		              (alter vars assoc :s2
		                     (vec(take ((deref vars) :c2)
		                               (scalarMat 0 ((deref vars) :r2)))))
		              )  
            
              (if (or (= str "r1") (= str "c1")) 
	               ;first src                         
                  (
                    (.removeAll (deref panel2))
                    (.setLayout (deref panel2) (GridLayout. ((deref vars) :r1) ((deref vars) :c1))) 
                    (ref-set txtSrc1 {:count 0})
                    (dotimes [n (* ((deref vars) :r1) ((deref vars) :c1))] 
                      (alter txtSrc1 assoc (keyword (.toString n)) (JTextField. "0"))
                        (alter txtSrc1 assoc :count (inc((deref txtSrc1) :count)))
                        (.setHorizontalAlignment 
                          ((deref txtSrc1) (keyword (.toString n))) JTextField/CENTER)
                        (.add (deref panel2) ((deref txtSrc1) (keyword (.toString n)))))
                    (.revalidate panel1)
                    )
                    ;second src
                  (
                    (.removeAll (deref panel3))
                    (.setLayout (deref panel3) (GridLayout. ((deref vars) :r2) ((deref vars) :c2))) 
                    (ref-set txtSrc2 {:count 0})
                    (dotimes [n (* ((deref vars) :r2) ((deref vars) :c2))] 
                      (alter txtSrc2 assoc (keyword (.toString n)) (JTextField. "0"))
                        (alter txtSrc2 assoc :count (inc((deref txtSrc2) :count)))
                        (.setHorizontalAlignment 
                          ((deref txtSrc2) (keyword (.toString n))) JTextField/CENTER)
                        (.add (deref panel3) ((deref txtSrc2) (keyword (.toString n)))))
                    (.revalidate panel1)
                    )                    
	               )
		            )
		          ;non-positive value
		          (
		            (.setText (deref r1) (.toString ((deref vars) :r1)))
	              (.setText (deref c1) (.toString ((deref vars) :c1)))
	              (.setText (deref r2) (.toString ((deref vars) :r2)))
	              (.setText (deref c2) (.toString ((deref vars) :c2)))
	              (.setText Scalar (.toString ((deref vars) :scalar)))
		            )
		          )
		        )
          ;helps!
          (catch Exception ex nil))
        ) 
      ) 
    )  
  
  ;sizes and lisetners of textfields for matrix size input
  (add-key-released-listener (deref r1) changeSize (deref r1) "r1")
  (add-key-released-listener (deref c1) changeSize (deref c1) "c1")
  (add-key-released-listener (deref r2) changeSize (deref r2) "r2")
  (add-key-released-listener (deref c2) changeSize (deref c2) "c2")
  (.setPreferredSize (deref r1) (Dimension. 30 30))
  (.setPreferredSize (deref c1) (Dimension. 30 30))
  (.setPreferredSize (deref r2) (Dimension. 30 30))
  (.setPreferredSize (deref c2) (Dimension. 30 30))
  
  
  ;populate source 1 - needed for alignment
  (dotimes [n (* (vars :r1) (vars :c1))] 
    (dosync
      (alter txtSrc1 assoc (keyword (.toString n)) (JTextField.
                                                     (.toString (get (get-col (vars :s1) (quot n (vars :c1))) (rem n (vars :c1))))))
      (.setHorizontalAlignment 
        ((deref txtSrc1) (keyword (.toString n))) JTextField/CENTER)
      (.setCaretPosition (txtSrc1 (keyword (.toString n))) 0)
      (.add (deref panel2) ((deref txtSrc1) (keyword (.toString n)))))
    )
   
  ;populate source 2
  (dotimes [n (* (vars :r2) (vars :c2))] 
    (dosync
      (alter txtSrc2 assoc (keyword (.toString n)) (JTextField.
                                                     (.toString (get (get-col (vars :s2) (quot n (vars :c2))) (rem n (vars :c2))))))
      (.setHorizontalAlignment 
        (txtSrc2 (keyword (.toString n))) JTextField/CENTER)
      (.setCaretPosition (txtSrc2 (keyword (.toString n))) 0)
      (.add (deref panel3) (txtSrc2 (keyword (.toString n)))))
    )
  (.revalidate panel1)
 ))

(try (gui-main)(catch Exception ex nil))
