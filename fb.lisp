; K = knows; TB = true belief; FB = false belief
; t0s/ns; t0p/q; t1s/ns; t1p/q
(setq training-set
  (list
    ;(list 0 01 0 01 0)
    (list 0 01 1 01 101 "K (0 01 1 01 101)")
    ;(list 0 01 0 10)
    (list 0 01 1 10 110 "K (0 01 1 10 110)")
    (list 1 01 0 01 001 "TB (1 01 0 01 001)") ; true belief
    (list 1 01 1 01 101 "K (1 01 1 01 101)")
    (list 1 01 0 10 001 "FB (1 01 0 10 001)") ; false belief
    (list 1 01 1 10 110 "K (1 01 1 10 110)")
    ;(list 0 10 0 01)
    (list 0 10 1 01 101 "K (0 10 1 01 101)")
    ;(list 0 10 0 10)
    (list 0 10 1 10 110 "K (0 10 1 10 110)")
    (list 1 10 0 01 010 "FB (1 10 0 01 010)") ; false belief
    (list 1 10 1 01 101 "K (1 10 1 01 101)")
    (list 1 10 0 10 010 "TB (1 10 0 10 010)") ; true belief
    (list 1 10 1 10 110 "K (1 10 1 10 110)")))

(defvar w1 (random 1.0))
(defvar w2 (random 1.0))
(defvar w3 (random 1.0))
(defvar w4 (random 1.0))

(defun sum-weights (x1 w1 x2 w2 x3 w3 x4 w4)
  (/ (+ (* x1 w1) (* x2 w2) (* x3 w3) (* x4 w4)) (length training-set)))

(defun cost (x1 w1 x2 w2 x3 w3 x4 w4 expected)
  (setq distance (- (sum-weights x1 w1 x2 w2 x3 w3 x4 w4) expected))
  (* distance distance))

(defvar eps  1e-3)
(defvar rate 1e-1)

(format t "w1: ~f, w2: ~f, w3: ~f, w4: ~f" w1 w2 w3 w4)

(loop for _ from 1 to 6000 do
      (loop for data in training-set do
	    (setq x1 (nth 0 data))
	    (setq x2 (nth 1 data))
	    (setq x3 (nth 2 data))
	    (setq x4 (nth 3 data))
	    (setq expected (nth 4 data))
	    (setq c (cost x1 w1 x2 w2 x3 w3 x4 w4 expected))
	    ;(print c)
	    (setq c1 (cost x1 (+ w1 eps) x2 w2 x3 w3 x4 w4 expected))
	    (setq c2 (cost x1 w1 x2 (+ w2 eps) x3 w3 x4 w4 expected))
	    (setq c3 (cost x1 w1 x2 w2 x3 (+ w3 eps) x4 w4 expected))
	    (setq c4 (cost x1 w1 x2 w2 x3 w3 x4 (+ w4 eps) expected))

	    (setf w1 (- w1 (/ (* rate (- c1 c)) eps)))
	    (setf w2 (- w2 (/ (* rate (- c2 c)) eps)))
	    (setf w3 (- w3 (/ (* rate (- c3 c)) eps)))
	    (setf w4 (- w4 (/ (* rate (- c4 c)) eps)))))

(terpri)
(format t "w1: ~f, w2: ~f, w3: ~f, w4: ~f" w1 w2 w3 w4)

(loop for data in training-set do
      (setq x1 (nth 0 data))
      (setq x2 (nth 1 data))
      (setq x3 (nth 2 data))
      (setq x4 (nth 3 data))
      (setq expected (nth 4 data))
      (setq answer (sum-weights x1 w1 x2 w2 x3 w3 x4 w4))
      (setq situation (nth 5 data))
      (setq c (cost x1 w1 x2 w2 x3 w3 x4 w4 expected))

      (format t "--------------~C" #\linefeed)
      (format t "type: ~d ~Cexpect: ~d~Canswer: ~f~Ccost: ~f~C"
	      situation #\linefeed
	      expected  #\linefeed
	      answer    #\linefeed
	      c         #\linefeed))
