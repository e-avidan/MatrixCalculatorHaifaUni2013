(ns MatrixCalculator.GUI
  (:use MatrixCalculator.Functions)
  (:use MatrixCalculator.Contributed)
  (:import 
    (javax.swing JPanel, JFrame, JButton, JTextField,
                 JLabel, JComboBox)
    (java.awt.event KeyEvent, KeyAdapter)
    (java.awt GridLayout, Color, Dimension, BorderLayout)
    (java.text NumberFormat))
  )

(defn GUI "the GUI" [] (         
  (def frame (JFrame. "Hello Frame"))
  
  ;control variables
  (def vars (ref (zipmap 
              [:dst1 :dst2 :src1 :act :src2 :scalar :r1 :c1 :r2 :c2 :d1 :d2 :s1 :s2] 
              [0 0 0 0 0 2 1 1 2 2 0 0 0 0 ]))) 
  
  ;status of GUI input and output
   (def txtDst1 (ref {:count 0}))
   (def txtDst2 (ref {:count 0}))
   (def txtSrc1 (ref {:count 0}))
   (def txtSrc2 (ref {:count 0}))
  
  ;choose sizes of input matrices
  (comment
  (println "Enter # of rows in mat1")
  (dosync (alter vars assoc :r1 (Integer. (read-line))))
  (println "Enter # of columns in mat1")
  (dosync (alter vars assoc :c1 (Integer. (read-line))))
  (println "Enter # of rows in mat2")
  (dosync (alter vars assoc :r2 (Integer. (read-line))))
  (println "Enter # of columns in mat2")
  (dosync (alter vars assoc :c2 (Integer. (read-line)))))   
  
 
   ;reset the source matrices (dst are not yet made)
  (dosync 
	   (alter vars assoc :s1
	     (vec(take ((deref vars) :c1)
	               (scalarMat 0 ((deref vars) :r1))))))
   (dosync 
	   (alter vars assoc :s2
	     (vec(take ((deref vars) :c2)
	               (scalarMat 0 ((deref vars) :r2))))))
  ;;just for now
  (dosync 
	   (alter vars assoc :d1
	     (vec(take ((deref vars) :c1)
	               (scalarMat 0 ((deref vars) :r2))))))
  (dosync 
	   (alter vars assoc :d2
	     (vec(take ((deref vars) :c1)
	               (scalarMat 0 ((deref vars) :r1))))))
   
  ;window settings
  (def x 1280)
  (def y 720)
	(.setSize frame x y)
  (.setMaximumSize frame (Dimension. x y))
  (.setMinimumSize frame (Dimension. x y)) 
  (.setLayout frame (BorderLayout. 0 0))
  (.setLocationRelativeTo frame (.getRootPane frame))
	(.setVisible frame true)
 
  ;panels inside the window
  ;matrix size panel
  (def panel1 (JPanel.))
  ;source matrix 1
  (def panel2 (ref (JPanel.)))
  ;source matrix 2
  (def panel3 (ref (JPanel.)))
  (def panel4 (JPanel.))
  ;action panel
  (def panel5 (JPanel.))
  ;destination matrices panels
  (def w (JPanel.))
  (def e (JPanel.))
  
  
  ;turn string into double or return nil if not possible
  ;if a number is extractable from the start, even if non-numeric
  ;characters appear later, it is extracted, so slips on the keyboard
  ;are forgiven
  (defn toDouble [str] (
         try (.parse (NumberFormat/getNumberInstance) str)
           (catch Exception e nil))
  )  
  
  ;reference for all
  (def d (ref nil))


  ;keyevent for source matrix size change
  (defn changeSize "change size of matrix listener" [e component str] 
    (if (= (.getKeyCode e) (KeyEvent/VK_ENTER))  
      (dosync (ref-set d (toDouble (.getText component)))
        (try
          (if (nil? (deref d))
	         (
	           ;restore val - is not parsable
	           (if (= str "r1" )
	             (.setText component (.toString ((deref vars) :r1))))
	           (if (= str "c1" )
	             (.setText component (.toString ((deref vars) :c1))))
	           (if (= str "r2" )
	             (.setText component (.toString ((deref vars) :r2))))
	           (if (= str "c2" )
	             (.setText component (.toString ((deref vars) :c2))))
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
		            (if (= str "r1" )
		              (.setText component (.toString ((deref vars) :r1))))
		            (if (= str "c1" )
		              (.setText component (.toString ((deref vars) :c1))))
		            (if (= str "r2" )
		              (.setText component (.toString ((deref vars) :r2))))
		            (if (= str "c2" )
		              (.setText component (.toString ((deref vars) :c2))))
		            )
		          )
		        )
          ;helps!
          (catch NullPointerException e nil))
        ) 
      ) 
    )

  ;textfields for matrix size input
  (def r1 (ref (JTextField. (.toString (vars :r1)))))
  (add-key-released-listener (deref r1) changeSize (deref r1) "r1")
  (def c1 (ref (JTextField. (.toString (vars :c1)))))
  (add-key-released-listener (deref c1) changeSize (deref c1) "c1")
  (def r2 (ref (JTextField. (.toString (vars :r2)))))
  (add-key-released-listener (deref r2) changeSize (deref r2) "r2")
  (def c2 (ref (JTextField. (.toString (vars :c2)))))
  (add-key-released-listener (deref c2) changeSize (deref c2) "c2")
  (.setPreferredSize (deref r1) (Dimension. 30 30))
  (.setPreferredSize (deref c1) (Dimension. 30 30))
  (.setPreferredSize (deref r2) (Dimension. 30 30))
  (.setPreferredSize (deref c2) (Dimension. 30 30))
  
  ;populating top control panel
  (.add panel1 (JLabel. "rows_mat1:"))
  (.add panel1 (deref r1))
  (.add panel1 (JLabel. "cols_mat1:"))
  (.add panel1 (deref c1))
  (.add panel1 (JLabel. "rows_mat2:"))
  (.add panel1 (deref r2))
  (.add panel1 (JLabel. "cols_mat2:"))
  (.add panel1 (deref c2))
  
  ;layouts of main panels
  (.setLayout (deref panel2)(GridLayout. ((deref vars) :r1) ((deref vars) :c1)) )
  (.setLayout (deref panel3) (GridLayout. ((deref vars) :r2) ((deref vars) :c2)))
  (.setLayout panel4 (GridLayout. 1 2))
  
  ;populate source 1 - needed for alignment
  (dotimes [n (* (vars :r1) (vars :c1))] 
    (dosync(alter txtSrc1 assoc (keyword (.toString n)) (JTextField. "0"))
      (alter txtSrc1 assoc :count (inc((deref txtSrc1) :count)))
      (.setHorizontalAlignment 
        ((deref txtSrc1) (keyword (.toString n))) JTextField/CENTER)
      (.add (deref panel2) ((deref txtSrc1) (keyword (.toString n))))))

  ;populate source 2
  (dotimes [n (* (vars :r2) (vars :c2))] 
    (dosync(alter txtSrc2 assoc (keyword (.toString n)) (JTextField. "0"))
      (alter txtSrc2 assoc :count (inc((deref txtSrc2) :count)))
      (.setHorizontalAlignment 
        ((deref txtSrc2) (keyword (.toString n))) JTextField/CENTER)
      (.add (deref panel3) ((deref txtSrc2) (keyword (.toString n))))))


  ;set layouts of destinations (1 is MUL, 2 is ADD - for now)
  (if (= (vars :r2) (vars :c1))
    (.setLayout w (GridLayout. (vars :r1) (vars :c2))))
  (if (and (= (vars :r1) (vars :r2)) (= (vars :c1) (vars :c2)))
    (.setLayout e (GridLayout. (vars :r1) (vars :c1))))
  
  ;populate destination 1
  (if (= (vars :r2) (vars :c1)) 
    (dotimes [n (* (vars :r1) (vars :c2))]
      (dosync(alter txtDst1 assoc (keyword (.toString n)) (JTextField. (.toString n)))
        (alter txtDst1 assoc :count (inc((deref txtDst1) :count)))
        (.setEditable ((deref txtDst1) (keyword (.toString n))) false)
        (.setHorizontalAlignment 
          ((deref txtDst1) (keyword (.toString n))) JTextField/CENTER)
        (.add w ((deref txtDst1) (keyword (.toString n)))))
      )
   ((dosync(alter txtDst1 assoc :0 (JTextField. "Matrices size mismatch!"))
         (alter txtDst1 assoc :count (inc((deref txtDst1) :count))))
        (.setEditable ((deref txtDst1) :0) false)
        (.add w ((deref txtDst1) :0))) 
      )

  ;populate destination 2
  (if (and (= (vars :r1) (vars :r2)) (= (vars :c1) (vars :c2)))
      (dotimes [n (* (vars :r1) (vars :c1))] 
        (dosync(alter txtDst2 assoc (keyword (.toString n)) (JTextField. (.toString n)))
          (alter txtDst2 assoc :count (inc((deref txtDst2) :count)))
          (.setEditable ((deref txtDst2) (keyword (.toString n))) false)
          (.setHorizontalAlignment 
            ((deref txtDst2) (keyword (.toString n))) JTextField/CENTER)
          (.add e ((deref txtDst2) (keyword (.toString n)))))
       )
      ((dosync(alter txtDst2 assoc :0 (JTextField. "Matrices size mismatch!"))
         (alter txtDst2 assoc :count (inc((deref txtDst2) :count))))
        (.setEditable ((deref txtDst2) :0) false)
        (.add e ((deref txtDst2) :0))) 
      )
  
  ;window populating
  (.add panel4 w)
  (.add panel4 e)
  (.add frame panel1 BorderLayout/NORTH)
  (.add frame (deref panel2) BorderLayout/WEST)
  (.add frame (deref panel3) BorderLayout/EAST)
  (.add frame panel4 BorderLayout/SOUTH)
  (.add frame panel5 BorderLayout/CENTER)
  
  (.setPreferredSize panel1 (Dimension. 400 50))
  (.setPreferredSize panel5 (Dimension. 200 225))
  (.setMaximumSize panel5 (Dimension. 200 225))
  (.setPreferredSize (deref panel2) (Dimension. 500 500))
  (.setPreferredSize (deref panel3) (Dimension. 500 500))
  (.setPreferredSize panel4 (Dimension. 200 225))
  (.setPreferredSize w (Dimension. 100 225))
  (.setPreferredSize e (Dimension. 100 225))
  
  ;so it looks good and you can see the different areas
  (.setBackground panel1 Color/green)
  (.setBackground (deref panel2) Color/black)
  (.setBackground (deref panel3) Color/blue)
  (.setBackground w Color/red)
  (.setBackground e Color/orange)
  (.setBackground panel5 Color/magenta)
  
  ;combo boxes to choose actions
  (def ComboDst (JComboBox. (to-array (list "LeftResult" "RightResult"))))
  (def ComboSrc1 (JComboBox. (to-array (list "" "LeftMat" "Rightmat" "LeftResult" "RightResult" "Scalar" ))))
  (def ComboAct (JComboBox. (to-array (list "+" "-" "*" ".*" "./" "norm2 of" "determinant of" "normalize" "add to each element"))))
  (def ComboSrc2 (JComboBox. (to-array (list "LeftMat" "Rightmat" "LeftResult" "RightResult" "Scalar" ))))
  (def Scalar (JTextField. (.toString (vars :scalar))))
  (def ScalarBox (JPanel.))
  (.add ScalarBox Scalar)
  (def Commit (JButton. "Commit!"))
  
  (.add panel5 ComboDst)
  (.add panel5 (JLabel. "="))
  (.add panel5 ComboSrc1)
  (.add panel5 ComboAct)
  (.add panel5 ComboSrc2)
  ;no need for scalar box at this time
  (.setVisible ScalarBox false)
  (.add panel5 ScalarBox)
  (.add panel5 Commit)

  (.revalidate panel1)
 ))

(GUI)