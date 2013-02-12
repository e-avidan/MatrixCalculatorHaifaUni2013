(ns MatrixCalculator.Contributed
    (:import 
    (javax.swing JPanel, JFrame, JButton, JTextField,
                 JLabel, JComboBox)
    (java.awt.event KeyEvent, KeyAdapter)
    (java.awt GridLayout, Color, Dimension, BorderLayout)
    (java.text NumberFormat)))

;a contrib function.. it isn't ours, but we need it and can't import
;however we did modify it to a different kind of listener
  (defn add-key-released-listener
  "Adds a KeyListener to component that only responds to KeyTyped events.
  When a key is typed, f is invoked with the KeyEvent as its first argument
  followed by args. Returns the listener."
  [component f & args]
  (let [listener (proxy [KeyAdapter] []
                   (keyReleased [event] (apply f event args)))]
    (.addKeyListener component listener)
    listener))
