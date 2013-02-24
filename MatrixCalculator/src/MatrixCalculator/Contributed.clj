(ns MatrixCalculator.Contributed
    (:import 
    (javax.swing JPanel, JFrame, JButton, JTextField,
                 JLabel, JComboBox)
    (java.awt.event KeyEvent, KeyAdapter, ActionListener)
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

;another contrib function, not modified this time.
(defn add-action-listener
  "Adds an ActionLister to component. When the action fires, f will be
  invoked with the event as its first argument followed by args.
  Returns the listener."
  [component f & args]
  (let [listener (proxy [ActionListener] []
                   (actionPerformed [event] (apply f event args)))]
    (.addActionListener component listener)
    listener))