;;;; Day 1

(ql:quickload :alexandria) ; https://quickref.common-lisp.net/alexandria.html#Exported-definitions
(ql:quickload :str)
(ql:quickload :cl-geometry) ; https://quickref.common-lisp.net/cl-geometry.html#Exported-functions
(ql:quickload :fset) ; https://common-lisp.net/project/fset/Site/FSet-Tutorial.html
(ql:quickload :array-operations) ; https://github.com/bendudson/array-operations
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :cl-arrows)

(defpackage :aoc
  (:use :cl :cl-arrows)
  (:local-nicknames (:a :alexandria)
                    (:geo :cl-geometry)
                    (:f :fset)
                    (:re :ppcre)))
                    

(in-package :aoc)

;;; CL-Geometry has a bad API

(defun point (x y)
  (make-instance 'geo:point
                 :x x
                 :y y))

(defun seg (x y x1 y1)
  (make-instance 'geo:line-segment
                 :start (point x y)
                 :end (point x1 y1)))

;;; Utils

(defmacro destr-lambda (bindings &body body)
  (let ((arg (gensym)))
    `(lambda (,arg)
       (destructuring-bind ,bindings ,arg
         ,@body))))

(defun lines (file)
  (str:lines (str:from-file file)))

(defun line-nums (file)
  (mapcar #'parse-integer (lines file)))

(defun line-many-nums (file)
  (mapcar (lambda (line)
            (mapcar #'parse-integer (str:words line)))
          (lines file)))


;;; Day 1

(defun day-1 (n &key (file "1.dat") (target 2020))
  (a:map-combinations (lambda (list)
                        (when (= target (reduce #'+ list))
                          (return-from day-1 (reduce #'* list))))
                      (mapcar #'parse-integer (lines file))
                      :length n))

;;; Day 2

(defun parse-pass (pass)
  (re:register-groups-bind (lower upper char pwd)
      ("^(\\d+)-(\\d+) (\\w): (\\w+)$" pass)
    (list (parse-integer lower) (parse-integer upper) (char char 0) pwd)))

(defun day-2-p1 (in)
  (destructuring-bind (min max char pass)
      in
    (<= min (count char pass) max)))

(defun day-2-p2 (in)
  (destructuring-bind (min max char pass)
      in
    (a:xor (eql (char pass (- min 1)) char)
           (eql (char pass (- max 1)) char))))

(defun day-2 ()
  (let ((in (mapcar #'parse-pass (lines "2.dat"))))
    (format t "part 1: ~A~%" (length (remove-if-not #'day-2-p1 in)))
    (format t "part 2: ~A~%" (length (remove-if-not #'day-2-p2 in)))))

;;; Day 3

;; These helpers ended up being unnecessary, I suspected the problem
;; would be more complicated than it was so I wrote these.
(defun angle (x y)
  (cons x y))

(defun pos (x y)
  (cons x y))

(defun move (pos angle)
  (cons (+ (car pos) (car angle))
        (+ (cdr pos) (cdr angle))))

(defun valid-pos (grid pos)
  (let* ((row (elt grid (cdr pos)))
         (grid-w (length (elt grid 0)))
         (col (mod (car pos) grid-w)))
    (eql (char row col) #\.)))

(defun day-3 (&optional (angle (angle 3 1)))
  (let* ((grid (lines "3.dat"))
         (len (length grid))
         (total-trees 0)
         (current (pos 0 0)))
    (loop :while t
          :do (progn
                (if (>= (cdr current) len)
                    (return-from day-3 total-trees))
                (if (not (valid-pos grid current))
                    (incf total-trees))
                (setf current (move current angle))))))

(defun day-3-p2 ()
  (let* ((slopes (list (angle 1 1)
                       (angle 3 1)
                       (angle 5 1)
                       (angle 7 1)
                       (angle 1 2)))
         (hits (mapcar #'day-3 slopes)))
    (reduce #'* hits)))

;;; Day 4 -- filtered ;-; ended up rewriting in python [pepehands]

(defun key-in-list (list pairs)
  (loop :for i :in list
        :do (loop :for p :in pairs)))

(defun list-key (list key)
  (loop :for i :in list
        :do (if (equal key (car i))
                (return-from list-key (cadr i))))
  nil)

(defun passport-valid (p)
  (let* ((required (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
         (keys (mapcar #'car p)))
    (loop :with found = nil
          :for r :in required
          :do (progn
                ;(format t "~A ~A~%" keys r)
                (unless (find r keys :test #'equal)
                  ;(format t "...no~%")
                  (return-from passport-valid nil))))
    (and (<= 1920 (parse-integer (list-key p "byr")) 2002)
         (<= 2010 (parse-integer (list-key p "iyr")) 2020)
         (<= 2020 (parse-integer (list-key p "eyr")) 2030)
         (re:register-groups-bind (len unit)
             ("^(\\d+)(\\w+)$" (list-key p "hgt"))
           (if (equal unit "cm")
               (<= 150 (parse-integer len) 193)
               (<= 59 (parse-integer len) 76)))
         (re:register-groups-bind (color)
             ("#([0-9a-f]{6})" (list-key p "hcl"))
           t)
         (find (list-key p "ecl") (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
         (re:register-groups-bind (id)
             ("(\d{9})" (list-key p "pid"))
           t))))

(defun day-4 ()
  (let* ((input (mapcar (lambda (x) (re:split "\\s" x))
                       (re:split "\\n\\n" (str:from-file "4-t.dat"))))
         (paired (mapcar (lambda (x)
                           (mapcar (lambda (y)
                                     (re:split ":" y))
                                   x))
                         input)))
;    (return-from day-4 paired)
    (loop :for p :in paired
          :with x = 0
          :do (progn
                (format t "~A~%" (passport-valid p))
                (when (passport-valid p)
                  (incf x)))
          :finally (return x))))

;;; Day 5

(defun bsp-step (min max key)
  (let ((range (/ (- max min) 2)))
    (if (or (eql key #\F) (eql key #\L))
        (values min (floor (+ range min)))
        (values (ceiling (+ range min)) max))))

(defun bsp-walk (str min max)
  (loop :for c :across str
        :with low = min
        :with high = max
        :do (multiple-value-bind (l h)
                (bsp-step low high c)
              (setf low l)
              (setf high h))
        :finally (return (values low high))))

(defun seat-id (row col)
  (+ (* 8 row ) col))

(defun get-seat-pos (str)
  (re:register-groups-bind (row-s col-s)
      ("^([FB]{7})([LR]{3})$" str)
    (let ((row (bsp-walk row-s 0 127))
          (col (bsp-walk col-s 0 7)))
      (values row col))))

(defun get-seat-id (str)
  (multiple-value-bind (row col)
      (get-seat-pos str)
    (seat-id row col)))

(defun find-open-seat (input)
  (loop :with ids = (mapcar #'get-seat-id input)
        :for r :from 0 :to 127
        :do (loop :for c :from 0 :to 7
                  :for this-id = (seat-id r c)
                  :do (when (and (not (find this-id ids))
                                 (find (- this-id 1) ids)
                                 (find (+ this-id 1) ids))
                        (format t "Found empty seat at ~A ~A (~A)~%" r c this-id)))))

(defun day-5 ()
  (let ((input (lines "5.dat")))
    (format t "Part 1 max: ~A~%" (reduce #'max (mapcar #'get-seat-pos input)))
    (find-open-seat input)))

;;; Day 6

(defmacro incf-or-one (val)
  `(if (numberp ,val)
       (incf ,val)
       (setf ,val 1)))

(defun customs-group (lines)
  (let* ((people (str:lines lines))
         (num-people (length people))
         (chosen (make-hash-table)))
    (loop :for person :in people
          :do (loop :for option :across person
                    :do (incf-or-one (gethash option chosen)))
          :finally (return (length (remove-if-not (lambda (value)
                                                    (= value num-people))
                                                  (a:hash-table-values chosen)))))))

(defun day-6 ()
  (let ((input (re:split "\\n\\n" (str:from-file "6.dat"))))
    (reduce #'+ (mapcar #'customs-group input))))

;;; Day 7 -- one week, let's hope its not still easy mode...

(defun bag-number (bag)
  (car bag))

(defun bag-color (bag)
  (cdr bag))

(defun parse-numbered-color (bag)
  (re:register-groups-bind (number color)
      ("(\\d+) (\\w+ \\w+) bags?" bag)
    (cons (parse-integer number) color)))

(defun parse-bag-rule (bag)
  (re:register-groups-bind (outer-color inner)
      ("(\\w+ \\w+) bags contain ([a-z\\s\\d,]+)\\." bag)
    (cons outer-color (remove-if-not #'identity
                                     (mapcar #'parse-numbered-color (re:split ",\\s*" inner))))))

(defun can-contain (table outer inner)
  (if (< 0 (count inner outer :test #'equal :key #'bag-color))
      t
      (loop :for i :in outer
            :do (if (can-contain table (gethash (bag-color i) table) inner)
                    (return-from can-contain t))
            :finally (return nil))))

(defun find-contains (table color)
  (loop :for k :being :each hash-key :of table
        :using (hash-value v)
        :if (can-contain table v color)
          :collect k
        :end))
        
(defun num-bags-required (table color)
  (->> (gethash color table)
       (mapcar (lambda (bag)
                 (+ (* (num-bags-required table (bag-color bag))
                       (bag-number bag))
                    (bag-number bag))))
       (reduce #'+)))

(defun day-7 ()
  (let ((table (-<> (lines "7.dat")
                    (mapcar #'parse-bag-rule <>)
                    (a:alist-hash-table <> :test #'equal))))
    (format t "Part 1: ~A~%" (find-contains table "shiny gold"))
    (format t "Part 2: ~A~%" (num-bags-required table "shiny gold"))))

;;; Day 8

(defclass inst ()
  ((op :accessor inst-op
       :initarg :op)
   (arg :accessor inst-arg
        :initarg :arg)))

(defclass vm ()
  ((acc :accessor vm-acc
        :initarg :acc)
   (pc :accessor vm-pc
       :initarg :pc)
   (visited :accessor vm-visited
            :initform (f:empty-set))
   (insts :accessor vm-insts
          :initarg :insts)))

(defun make-vm (inst-list)
  (make-instance 'vm :insts (make-array (list (length inst-list))
                                        :initial-contents inst-list)
                     :acc 0
                     :pc 0))

(defun parse-inst (line)
  (re:register-groups-bind (inst arg)
      ("(\\w+) ([+\\-]?\\d+)" line)
    (make-instance 'inst :op inst :arg (parse-integer arg))))

(defmacro addf (val num)
  `(setf ,val (+ ,val ,num)))

(defmethod vm-step ((vm vm))
  (let* ((inst (aref (vm-insts vm) (vm-pc vm)))
         (op (inst-op inst))
         (arg (inst-arg inst)))
    (cond ((equal op "acc")
           (progn (addf (vm-acc vm) arg)
                  (incf (vm-pc vm))))
          ((equal op "jmp")
           (addf (vm-pc vm) arg))
          ((equal op "nop")
           (incf (vm-pc vm))))))

(defmethod vm-run ((vm vm))
  (loop
     (if (f:contains? (vm-visited vm) (vm-pc vm))
         (return-from vm-run (values nil (vm-acc vm) (vm-pc vm)))
         (progn
           (setf (vm-visited vm) (f:with (vm-visited vm) (vm-pc vm)))
           (when (>= (vm-pc vm) (length (vm-insts vm)))
             (return-from vm-run (values t (vm-acc vm) (vm-pc vm))))
           (vm-step vm)))))

(defun replace-nth-nop-jmp (input n)
  (loop :for inst :in input
        :for op = (inst-op inst)
        :with i = 0
        :if (or (equal op "jmp")
                (equal op "nop"))
          :do (progn
                (when (= i n)
                  (setf (inst-op inst) (if (equal op "jmp")
                                           "nop"
                                           "jmp")))
                (incf i))
        :end
        :finally (return input)))

(defun try-nop-jmp (input)
  (loop :for i :from 0 :to (length (day-8)) ; max possible jmps/nops
        :do (multiple-value-bind (success acc pc)
                (vm-run (make-vm (replace-nth-nop-jmp input i)))
              (when success
                (return-from try-nop-jmp (values i acc)))
              (replace-nth-nop-jmp input i))))

(defun day-8 ()
  (->> (lines "8.dat")
       (mapcar #'parse-inst)))

(defun day-8-p1 ()
  (->> (day-8)
       (make-vm)
       (vm-run)))

(defun day-8-p2 ()
  (try-nop-jmp (day-8)))

;;; Day 9

(defun list-to-array (list)
  (make-array (list (length list))
              :initial-contents list))

(defun xmas-valid (from to input target)
  (loop :for i :from from :to to
        :for first-num = (aref input i)
        :do (loop :for j :from from :to to
                  :for second-num = (aref input j)
                  :if (and (not (= first-num second-num))
                           (= (+ first-num second-num) target))
                    :do (return-from xmas-valid (values t first-num second-num))
                  :end)
        :finally (return nil)))

(defun valid-loop (input step)
  (format t "Looping for i from ~A until ~A~%" step (length input))
  (loop :for i :from step :to (- (length input) 1)
        :for val = (aref input i)
        :do (multiple-value-bind (valid fst snd)
                (xmas-valid (- i step) i input val)
              (unless valid
                (format t "Invalid number at i=~A val=~A~%" i val)
                (return-from valid-loop val)))))

(defun sum-range (input from to)
  (loop :for i :from from :to to
        :for val = (aref input i)
        :with sum = 0
        :do (addf sum val)
        :finally (return sum)))

(defun range-starting-adds-to (input from target)
  (loop :for i :from (+ from 1) :to (- (length input) 1)
        :for val = (aref input i)
        :with sum = (aref input from)
        :do (when (= (sum-range input from i) target)
              (let* ((range (coerce (subseq input from i) 'list))
                     (min (apply #'min range))
                     (max (apply #'max range)))
                (format t "Range found from ~A to ~A = ~A min=~A max=~A~%" from i target min max)
                (return-from range-starting-adds-to (+ min max))))
        :finally (return nil)))

(defun find-set-adds-to (input target)
  (loop :for i :from 0 :to (- (length input) 1)
        :for range-sum = (range-starting-adds-to input i target)
        :if range-sum
          :do (progn
                (format t "Found range starting from ~A that adds to ~A~%" i target)
                (return-from find-set-adds-to range-sum))
        :end))

(defun day-9 ()
  (->> (lines "9.dat")
       (mapcar #'parse-integer)
       (list-to-array)))

(defun day-9-p1 (&optional (input (day-9)))
  (valid-loop input 25))

(defun day-9-p2 ()
  (let* ((input (day-9))
         (invalid (day-9-p1 input)))
    (format t "Invalid is ~A~%" invalid)
    (find-set-adds-to input invalid)))

;;; Day 10

(defmacro if-let (binding if-true &optional if-false)
  (let ((name (car binding))
        (val (cadr binding)))
    `(let ((,name ,val))
       (if ,name
           ,if-true
           ,if-false))))

(defmacro defmemoized (fn args &body body)
  (let ((cache-name (gensym))
        (arg-name (gensym))
        (cached-name (gensym)))
    `(progn
      (defvar ,cache-name (make-hash-table :test #'equal))
      (defun ,fn (&rest ,arg-name)
        (if-let (,cached-name (gethash ,arg-name ,cache-name))
          (progn
            (format t "Cache hit~%")
            ,cached-name)
          (apply (lambda (,@args)
                   ,@body)
                 ,arg-name))))))

(defun num-diffs-of (n input)
  (loop :for i :in input
        :with previous = 0
        :with num = 0
        :do (progn
              (when (= (- i previous) n)
                (incf num))
              (setf previous i))
        :finally (return num)))

(defvar arr-cache (make-hash-table :test #'equal))

(defun possible-arrangements (input)
  (when (gethash input arr-cache)
    (return-from possible-arrangements (gethash input arr-cache)))
  (loop :for i :in input
        :with len-1 = (- (length input) 1)
        :for index :from 0 :to len-1
        :with previous = nil
        :for has-next = (< index len-1)
        :do (progn
              (when (and previous has-next)
                (when (<= (- (elt input (+ index 1)) previous) 3)
                  ;; This can be removed
                  (let ((result  (+ (possible-arrangements (subseq input index))
                                    (possible-arrangements
                                     (cons previous
                                           (subseq input (+ index 1)))))))
                    (setf (gethash input arrangements-cache) result)
                    (return-from possible-arrangements result))))
              (setf previous i))
        :finally (return 1)))

(defun day-10 ()
  (let* ((input (-<> (lines "10.dat")
                     (mapcar #'parse-integer <>)
                     (sort <> #'<)))
         (by-1 (num-diffs-of 1 input))
         (by-3 (+ 1 (num-diffs-of 3 input)))
         (possible (possible-arrangements (cons 0 input))))
    (values (* by-1 by-3) possible)))

;;; Day 11

(defun string-chars (string)
  (loop :for c :across string
        :collect c))

(defun aref-or-else (else i j input)
  (destructuring-bind (h w)
      (array-dimensions input)
    (if (or (not (< -1 i h))
            (not (< -1 j w)))
        else
        (aref input i j))))

(defun first-seen-in-dir (i j i1 j1 input)
  (let* ((i2 (+ i i1))
         (j2 (+ j j1))
         (val (aref-or-else nil i2 j2 input)))
    (cond ((eql val #\.)
           (first-seen-in-dir i2 j2 i1 j1 input))
          ((eql val nil)
           #\.)
          (t
           val))))

(defvar +directions+ '((-1 -1)
                       (-1 0)
                       (0 -1)
                       (1 1)
                       (1 0)
                       (0 1)
                       (1 -1)
                       (-1 1))
  "Look directions")

(defun get-visible (i j input)
  (loop :for (i1 j1) :in +directions+
        :for val = (first-seen-in-dir i j i1 j1 input)
        :collect val))

(defun get-surrounding (i j input)
  (loop :for (i1 j1) :in +directions+
        :for val = (aref-or-else #\. (+ i i1) (+ j j1) input)
        :collect val))

(defun seat-becomes-occupied (i j input &key (finder #'get-surrounding))
  (= (length (remove-if-not (lambda (x)
                              (eql x #\#))
                            (funcall finder i j input)))
     0))

(defun seat-becomes-empty (i j input &key (finder #'get-surrounding) (threshold 4))
  (>= (length (remove-if-not (lambda (x)
                               (eql x #\#))
                             (funcall finder i j input)))
      threshold))

(defun next-state (input &key (finder #'get-surrounding) (empty-threshold 4))
  (destructuring-bind (h w)
      (array-dimensions input)
    (loop :for i :from 0 :to (- h 1)
          :with new-arr = (make-array (list h w))
          :do (loop :for j :from 0 :to (- w 1)
                    :for val = (aref input i j)
                    :do (progn
                          ;(format t "~A, ~A = ~A~%" i j val)
                          (cond ((eql val #\.)
                                 (setf (aref new-arr i j) val))
                                ((eql val #\L)
                                 (setf (aref new-arr i j) (if (seat-becomes-occupied i j input
                                                                                     :finder finder)
                                                              #\#
                                                              #\L)))
                                ((eql val #\#)
                                 (setf (aref new-arr i j) (if (seat-becomes-empty i j input
                                                                                  :finder finder
                                                                                  :threshold empty-threshold)
                                                              #\L
                                                              #\#))))))
          :finally (return new-arr))))

(defun count-in-array (val array &key (test #'eql))
  (destructuring-bind (h w)
      (array-dimensions array)
    (loop :with count = 0
          :for i :from 0 :to (- h 1)
          :do (loop :for j :from 0 :to (- w 1)
                    :if (funcall test (aref array i j) val)
                      :do (incf count)
                    :end)
          :finally (return count))))
  

(defun step-until-finishes (input &key (part 1))
  (loop :with previous = nil
        :with this = input
        :with finder = (if (= part 1)
                           #'get-surrounding
                           #'get-visible)
        :with threshold = (if (= part 1)
                              4
                              5)
        :while (not (equalp previous this))
        :do (progn
              (setf previous this)
              (setf this (next-state this :finder finder :empty-threshold threshold)))
        :finally (return this)))

(defun lists-to-array (input)
  (let ((top-len (length input))
        (side-len (length (car input))))
    (make-array (list top-len side-len)
                :initial-contents input)))

(defun day-11 ()
  (let ((input (-<> (lines "11-big.dat")
                    (mapcar #'string-chars <>)
                    (lists-to-array <>))))
    (values (count-in-array #\# (step-until-finishes input))
            (count-in-array #\# (step-until-finishes input :part 2)))))

;; Day 12
;; Did this in Julia (See "Day 12 Julia.ipynb")
;; Perhaps I will rewrite in lisp later?

;; Day 13

(defun first-after (target bus)
  (loop :with waited = 0
        :while (> target waited)
        :do (addf waited bus)
        :finally (return (values waited bus))))

(defun find-first-bus (target intervals)
  (loop :for bus :in intervals
        :for first-point = (first-after target bus)
        :collect (list first-point bus)))

(defun range (from to)
  (loop :for i :from from :to to
        :collect i))

(defun zipwith (a b)
  (loop :for i :from 0 :to (- (length a) 1)
        :for together = (cons (elt a i) (elt b i))
        :collect together))

(defun day-13 ()
  (let* ((input (lines "13.dat"))
         (target (parse-integer (car input)))
         (intervals (->> (str:split "," (cadr input))
                         (remove-if (lambda (c)
                                      (equal c "x")))
                         (mapcar #'parse-integer))))
    (-<> (find-first-bus target intervals)
         (sort <> #'< :key #'car)
         (car)
         (funcall (lambda (f)
                    (* (- (car f) target)
                       (cadr f)))
                  <>))))

(defun first-that-works (vals)
  (loop :for (rem . num) :in vals
        :with base = 0
        :with lcd = 1
        :do (progn
              (loop :while (not (= 0 (mod (+ base rem)
                                          num)))
                    :do (addf base lcd))
              (setf lcd (* lcd num)))
        :finally (return base)))

(defun day-13-p2()
  (let* ((input (lines "13.dat"))
         (times (str:split "," (cadr input)))
         (intervals (->> (zipwith (range 0 (- (length times) 1)) times)
                         (remove-if (lambda (c)
                                      (equal (cdr c) "x")))
                         (mapcar (lambda (x)
                                   (cons (car x) (parse-integer (cdr x)))))))
         (sorted (sort intervals #'> :key #'cdr)))
    (first-that-works sorted)))
                        
;;; Day 14

(defclass dock ()
  ((bm :accessor dock-bm
       :initarg :bm)
   (mem :accessor dock-mem
        :initarg :mem)))

(defun make-dock ()
  (make-instance 'dock :bm ""
                       :mem (make-hash-table :test #'equal)))

(defun bm-inst (line)
  (if (str:starts-with-p "mem" line)
      (re:register-groups-bind (addr val)
          ("^mem\\[(\\d+)\\]\\s+=\\s+(\\d+)$" line)
        (cons 'mem (cons (parse-integer addr) (parse-integer val))))
      (re:register-groups-bind (mask)
          ("^mask\\s+=\\s+([01X]{36})$" line)
        (cons 'mask mask))))

(defun parse-binary (str)
  (parse-integer str :radix 2))

(defun mask-1 (mask)
  (let* ((cleared (str:replace-all "X" "0" mask)))
    (parse-binary cleared)))

(defun mask-0 (mask)
  (let* ((cleared (str:replace-all "X" "1" mask)))
    (parse-binary cleared)))

(defun apply-bm (mask num)
  (let* ((mask-0 (mask-0 mask))
         (mask-1 (mask-1 mask))
         (ord (logior mask-1 num))
         (and (logand ord mask-0)))
    and))

(defun addr-bm (mask num)
  (loop :with m = mask
        :with table = (make-hash-table)
        :for first-opt = (->> (str:replace-first "X" "Y" m)
                              (str:replace-all "0" "1")
                              (str:replace-all "X" "1")
                              (str:replace-all "Y" "0"))
        :for second-opt = (str:replace-all "X" "0" (str:replace-first "X" "1" m))
        :for opt-a = (logand (parse-binary first-opt) (logior num
                                                              (->> (str:replace-all "X" "0" m)
                                                                   (parse-binary))))
        :for opt-b = (logior (parse-binary second-opt) num)
        :for next-m = (str:replace-first "X" "0" m)
        :do (progn
              ;(format t "~A and ~A~%" opt-a opt-b)
              (setf (gethash opt-a table) t)
              (setf (gethash opt-b table) t)
              (when (not (equal next-m m))
                (loop :for x :in (addr-bm next-m opt-a)
                      :do (setf (gethash x table) t))
                (loop :for x :in (addr-bm next-m opt-b)
                      :do (setf (gethash x table) t)))
              (return-from addr-bm (a:hash-table-keys table)))))
        

(defmethod dock-set-mem ((dock dock) mem)
  (let* ((addr (car mem))
         (val (cdr mem)))
    (setf (gethash addr (dock-mem dock))
          (apply-bm (dock-bm dock) val))))

(defmethod dock-set-mem2 ((dock dock) mem)
  (let* ((addrs (addr-bm (dock-bm dock) (car mem))))
    (loop :for addr :in addrs
          :do (setf (gethash addr (dock-mem dock))
                    (cdr mem)))))

(defmethod dock-set-mask ((dock dock) mask)
  (setf (dock-bm dock) mask))

(defmethod dock-step ((dock dock) inst)
  (let* ((type (car inst))
         (args (cdr inst)))
    (case type
      ('mem
       (dock-set-mem2 dock args))
      ('mask
       (dock-set-mask dock args)))))

(defmethod dock-run ((dock dock) input)
  (loop :for i :in input
        :do (dock-step dock i)))
   
(defun day-14 ()
  (let* ((input (-<> (lines "14.dat")
                     (mapcar #'bm-inst <>)))
         (dock (make-dock)))
    (format t "Input is ~A~%" input)
    (dock-run dock input)
    (reduce #'+ (a:hash-table-values (dock-mem dock)))))

;;; Day 17

