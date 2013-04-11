;;; Copyright (c) 2008, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

;;; BITVECTOR-GENEALOGY is a solution written in Common Lisp to the famous
;;; BitVector Genealogy puzzle[1] of ITA software.

;;; [1] http://www.itasoftware.com/careers/jlisting.html?jid=28

;;; ITA explains the puzzle with below paragraph:

;;;   The BitVectors are an ancient and immortal race of 10,000, each with a
;;;   10,000 bit genome. The race evolved from a single individual by the
;;;   following process: 9,999 times a BitVector chosen at random from amongst
;;;   the population was cloned using an error-prone process that replicates
;;;   each bit of the genome with 80% fidelity (any particular bit is flipped
;;;   from parent to child 20% of the time, independent of other bits).

;;;   Write a program to guess the reproductive history of BitVectors from their
;;;   genetic material.

;;; In this program, I follow below fundamental procedures to produce the list
;;; of genome genealogy.

;;; 1. Compare each genome with every other and find the one that is the most
;;;    similar to others. Found genome is called the progenitor.

;;; 2. Sort genomes according to their birth order. For this purpose, first find
;;;    the most similar genome to the progenitor. And then find the genome that
;;;    is most similar to the progenitor and previously found parent(s), and
;;;    repeat this operation until there remains a single child genome.

;;; 3. With list of genomes sorted according to their birth order, a child in
;;;    the middle of this list can only have a parent that was born before
;;;    itself. Using this information, for every child find the most similar
;;;    parent that was born before this child.

;;; My previous COMMUTATIVE-GENOME-HASH (which is used to supply unique
;;; indetifiers for the memoization of KINSHIP-PROBABILITY) was producing hashes
;;; (nearly) ranging from 0 to 2N for a list of N genomes. And this was causing
;;; x2 memory consumption and an access overhead of the memoization cache
;;; vector. After the 1-1 hash function suggestion[2] of Geoffrey Summerhayes,
;;; this problem is solved. On the other hand, previously used hash function
;;; computationally performs better[3].

;;; [2] http://groups.google.com/group/comp.lang.lisp/msg/9f1feab93d11ea58
;;; [3] http://groups.google.com/group/comp.lang.lisp/msg/7d81279e8255d3ef

;;; In the puzzle, it is also noted that "Balance performance against
;;; probability of mistakes as you see fit.", but because of I found overal
;;; runtime pretty acceptable, I didn't pollute the code with unneccessary
;;; optimizations. But for the records,

;;; - You can increase gene buffer granularity by decreasing +GENE-BUFFER-SIZE+
;;;   variable and make GENOME-SIMILARITY to just compare genes that fall in a
;;;   specific range. (For instance, compare 5 of every 6 gene buffer.)

;;; - In current implementation COMMUTATIVE-GENOME-HASH (which is used to supply
;;;   unique indetifiers for the memoization of KINSHIP-PROBABILITY) produces
;;;   hashes (nearly) ranging from 0 to 2N for a list of N genomes. And this
;;;   causes x2 memory consumption and an access overhead of the memoization
;;;   cache vector. See 1-1 hash function suggestion[2] of Geoffrey Summerhayes
;;;   to this problem and its drawbacks[3].

;;; - It is possible to increase the performance by introducing worker threads
;;;   to parallize, for instance, processing of gene buffers in
;;;   GENOME-SMILIARITY, loops in MAXIMIZE-PROBABILITY.)

;;; Here is a run of the program with a 500x500 exercising gene array[4] and its
;;; solution[5] supplied by ITA. (Platform was an AMD64 2.2Ghz notebook with a
;;; cute SLIME session powered by SBCL 1.0.16. Because of lacking of threads,
;;; execution just loads on a single core at a time.)

;;; [4] http://www.itasoftware.com/careers/puzzles/bitvectors-genes.data.small.gz
;;; [5] http://www.itasoftware.com/careers/puzzles/bitvectors-parents.data.small.txt

;;; CL-USER> (values bvg:+genome-size+
;;;                  bvg:+genome-vector-size+
;;;                  bvg:+gene-buffer-size+)
;;; 500
;;; 500
;;; 60

;;; CL-USER> (time
;;; 	      (defparameter *genomes*
;;; 	        (bvg:parse-genomes
;;;              "/home/vy/temp/bitvectors-genes.data.small")))
;;; Evaluation took:
;;;   0.073 seconds of real time
;;;   0.028002 seconds of user run time
;;;   0.0 seconds of system run time
;;;   [Run times include 0.008 seconds GC run time.]
;;;   0 calls to %EVAL
;;;   0 page faults and
;;;   1,689,856 bytes consed.
;;; *GENOMES*

;;; CL-USER> (time
;;;           (defparameter *genealogy*
;;; 	        (bvg:construct-genealogy *genomes* 0.8)))
;;; Evaluation took:
;;;   0.269 seconds of real time
;;;   0.272017 seconds of user run time
;;;   0.0 seconds of system run time
;;;   [Run times include 0.016 seconds GC run time.]
;;;   0 calls to %EVAL
;;;   0 page faults and
;;;   13,112,800 bytes consed.
;;; *GENEALOGY*

;;; CL-USER> (bvg:write-genealogy-in-ita-format
;;;           *genealogy* "/home/vy/temp/genealogy.small")
;;; NIL

;;; CL-USER> (bvg:genealogy-differences
;;;           "/home/vy/temp/genealogy.small"
;;;           "/home/vy/temp/bitvectors-parents.data.small.txt")
;;; ((74 17 18) (73 25 202) (72 11 190) (71 257 360) (70 68 119) (69 255 368)
;;;  (68 51 476) (67 273 5) (66 388 97) (65 311 80) (64 71 144) (63 342 189)
;;;  (62 116 127) (61 294 151) (60 143 437) (59 346 157) (58 132 347) (57 17 284)
;;;  (56 62 121) (55 89 275) (54 292 129) (53 148 353) (52 38 385) (51 71 7)
;;;  (50 457 494) (49 36 54) (48 422 423) (47 51 450) (46 97 453) (45 485 132)
;;;  (44 292 476) (43 18 460) (42 292 15) (41 137 364) (40 437 217) (39 222 94)
;;;  (38 286 325) (37 214 399) (36 57 241) (35 179 260) (34 17 187) (33 127 265)
;;;  (32 284 417) (31 125 62) (30 176 207) (29 63 192) (28 322 457) (27 496 132)
;;;  (26 32 357) (25 24 399) (24 151 47) (23 454 263) (22 458 326) (21 363 60)
;;;  (20 11 302) (19 52 355) (18 451 385) (17 76 420) (16 289 476) (15 195 435)
;;;  (14 260 20) (13 368 329) (12 417 330) (11 419 60) (10 137 320) (9 98 63)
;;;  (8 171 22) (7 42 179) (6 179 465) (5 479 60) (4 246 434) (3 362 278)
;;;  (2 183 317) (1 147 228) (0 208 84))

;;; As can be seen from the above list of (CHILD MY-PARENT ITA-PARENT) lists,
;;; there are 63 (out of 500) differing parents. But if you'd look closely,
;;; actually almost every difference is because of that some of the parent-child
;;; relationships in my implementation gets generated as

;;;   (... parent_{n} parent_{n+1} ...)

;;; where ITA tells that they actually are

;;;   (... parent_{n+1} parent_{n} ...)

;;; I think, reason of this difference lies in the used approaches. I suspect
;;; algorithm generated ITA's list of parents, uses a different approach than
;;; mine.

;;; And here is the actual solution to 10,000x10,000 gene array[6].

;;; [6] http://matrix.itasoftware.com/puzzles/bitvectors/bitvectors-genes.data.gz

;;; CL-USER> (values bvg:+genome-size+
;;;                  bvg:+genome-vector-size+
;;;                  bvg:+gene-buffer-size+)
;;; 10000
;;; 10000
;;; 60

;;; CL-USER> (time
;;;           (defparameter *genomes*
;;;             (bvg:parse-genomes "/home/vy/temp/bitvectors-genes.data")))
;;; Evaluation took:
;;;   5.997 seconds of real time
;;;   5.828364 seconds of user run time
;;;   0.172011 seconds of system run time
;;;   [Run times include 0.384 seconds GC run time.]
;;;   0 calls to %EVAL
;;;   0 page faults and
;;;   840,577,344 bytes consed.
;;; *GENOMES*

;;; CL-USER> (time
;;;           (defparameter *genealogy*
;;;             (bvg:construct-genealogy *genomes* 0.8)))
;;; Evaluation took:
;;;   145.139 seconds of real time
;;;   144.39702 seconds of user run time
;;;   0.672042 seconds of system run time
;;;   [Run times include 2.164 seconds GC run time.]
;;;   0 calls to %EVAL
;;;   0 page faults and
;;;   4,007,116,880 bytes consed.
;;; *GENEALOGY*

;;; CL-USER> (bvg:write-genealogy-in-ita-format
;;;           *genealogy* "/home/vy/temp/genealogy")
;;; NIL

(defpackage :bitvector-genealogy
  (:nicknames :bvg)
  (:use :cl)
  (:export :+genome-size+
           :+genome-vector-size+
           :+gene-buffer-size+
           :parse-genomes
           :construct-genealogy
           :write-genealogy-in-ita-format
           :genealogy-differences))

(in-package :bitvector-genealogy)


;;; Common Runtime Constants & Types

(defconstant +genome-size+ 10000)

(defconstant +genome-vector-size+ 10000)

(defconstant +gene-buffer-size+ (integer-length most-positive-fixnum)
  "How many genes will be stored in a single GENE-BUFFER object.")

(deftype gene-buffer ()
  "Basic storage cell for genes of size +GENE-BUFFER-SIZE+."
  `fixnum)

(deftype genome ()
  `(simple-array
    gene-buffer
    (,(ceiling (/ +genome-size+ +gene-buffer-size+)))))

(deftype genome-vector ()
  `(simple-array genome (,+genome-vector-size+)))

(deftype genome-index ()
  `(integer 0 ,+genome-vector-size+))

(deftype coefficient ()
  `(single-float 0.0 1.0))

(deftype cache ()
  "Memoization cache vector."
  `(simple-array coefficient (*)))


;;; Genome Abstraction Stuff

(defun string-to-genome (input)
  "Creates a GENOME object using supplied bit string (101011110...)."
  (coerce
   (loop for start below +genome-size+ by +gene-buffer-size+
         collect (parse-integer
                  input
                  :start start
                  :end (min (+ start +gene-buffer-size+) +genome-size+)
                  :radix 2))
   'genome))

(let (genome-vector)
  (defun parse-genomes (pathname)
    "Parses genomes in given PATHNAME and returns list of genome indexes."
    ;; Parse genomes.
    (setq
     genome-vector
     (coerce
      (with-open-file (in pathname)
        (loop for line = (read-line in nil nil)
              while line collect (string-to-genome line)))
      'genome-vector))
    ;; Return list of GENOME-VECTOR indexes.
    (loop for index below (length genome-vector)
          collect index))

  (declaim (ftype (function (genome-index) genome) genome-of))
  (defun genome-of (genome-index)
    (declare (optimize speed)
             (type genome-index genome-index))
    (aref genome-vector genome-index)))


;;; Similarity & Kinship Probability Routines

(defun genome-similarity (parent child)
  "# of common bits in the supplied genomes."
  (declare (optimize speed))
  (/ (loop for parent-genes across (genome-of parent)
           for child-genes across (genome-of child)
           sum (logcount (ldb (byte +gene-buffer-size+ 0)
                              (lognot (logxor parent-genes child-genes)))))
     +genome-size+))

(let ((shift-offset (integer-length +genome-vector-size+)))
  (defun commutative-genome-hash (parent child)
    "Computes a commutative hash value for the given PARENT and CHILD genomes."
    (declare (optimize speed)
             (type genome-index parent child))
    (flet ((hash (small great)
             (logior great (the fixnum (ash small shift-offset)))))
      (if (< parent child)
          (hash parent child)
          (hash child parent)))))

(defun commutative-genome-hash (parent child)
  "Computes a commutative hash value for the given PARENT and CHILD genomes."
  (declare (optimize speed)
           (type genome-index parent child))
  (flet ((hash (small great)
           (round (+ small (* 0.5 great (1+ great))))))
    (if (< parent child)
        (hash parent child)
        (hash child parent))))

(defun internal-kinship-probability (parent child fidelity)
  "Computes kinship probability of the supplied PARENT and CHILD genomes."
  (declare (optimize speed)
           (type coefficient fidelity))
  (let ((similarity (coerce (genome-similarity parent child) 'coefficient)))
    (* (expt fidelity similarity)
       (expt (- 1.0 fidelity) (- 1.0 similarity)))))

(defun kinship-probability (parent child fidelity cache)
  "Wrapper around INTERNAL-KINSHIP-PROBABILITY. (Exploits repetitive calls of
identical PARENT-CHILD couples using supplied CACHE.)"
  (declare (optimize speed)
           (type cache cache))
  (let* ((index (commutative-genome-hash parent child))
         (value (aref cache index)))
    (if (zerop value)
        (setf (aref cache index)
              (internal-kinship-probability parent child fidelity))
        value)))

(defun maximize-probability (probability-fn items)
  "Returns item with the best probability."
  (loop with best-item
        with best-probability = 0
        for item in items
        for probability = (funcall probability-fn item)
        when (< best-probability probability)
        do (setq best-item item
                 best-probability probability)
        finally (return best-item)))

(defun most-common-parent (parents fidelity cache)
  "Finds the most common parent in the supplied list of PARENTS."
  (maximize-probability
   (lambda (parent)
     (loop for child in (remove parent parents)
           sum (kinship-probability parent child fidelity cache)))
   parents))

(defun sort-genomes (genomes fidelity cache)
  "Sorts supplied GENOMES with respect to their birth order by searching for the
most similar child to previously found parents at each step."
  (declare (optimize speed))
  (let* ((progenitor (most-common-parent genomes fidelity cache))
         (genomes (remove progenitor genomes))
         (children-probabilities
          (make-array +genome-vector-size+ :element-type 'coefficient)))
    ;; Initialize CHILDREN-PROBABILITIES.
    (loop for genome in genomes
          do (setf (aref children-probabilities genome)
                   (kinship-probability progenitor genome fidelity cache)))
    ;; Walk through available CHILDREN, and find the most similar child to
    ;; previously found PARENTS.
    (labels ((construct (parents children)
               (if children
                   (let ((child
                          (maximize-probability
                           (lambda (child)
                             (incf (aref children-probabilities child)
                                   (the coefficient
                                     (kinship-probability
                                      (first parents) child fidelity cache))))
                         children)))
                     (construct (cons child parents) (remove child children)))
                   parents)))
      (reverse (construct (list progenitor) genomes)))))

(defun construct-genealogy (genomes fidelity)
  "After sorting genomes according to their birth order, finds parent of each
genome at each step by looking at previously found parents."
  (let* ((cache-size
          (1+ (commutative-genome-hash
               (1- +genome-vector-size+)
               (1- +genome-vector-size+))))
         (cache (make-array cache-size :element-type 'coefficient))
         ;; First, sort genomes according to their birth order.
         (genomes (sort-genomes genomes fidelity cache)))
    (labels ((probable-parent (parents child)
               (maximize-probability
                (lambda (parent)
                  (kinship-probability parent child fidelity cache))
                parents))
             ;; Collect PARENTS and PARENTS-OF-PARENTS until CHILDREN gets
             ;; exhausted. Because of there isn't a linear order between
             ;; birth-order and parents, we need to store PARENTS-OF-PARENTS as
             ;; a separate list.
             (construct (parents parents-of-parents children)
               (if children
                   (construct
                    (cons (first children) parents)
                    (cons (probable-parent parents (first children))
                          parents-of-parents)
                    (rest children))
                   ;; Finally return each CHILDREN with its found PARENT.
                   (mapcar #'cons parents parents-of-parents))))
      (construct (list (first genomes)) (list -1) (rest genomes)))))
    
(defun write-genealogy-in-ita-format (genealogy pathname)
  "Writes constructed GENEALOGY into specified file PATHNAME with respect to
rules described by ITA: For each input line, the 0-based line number of that
individual's parent, or -1 if it is the progenitor."
  (with-open-file (out pathname :direction :output :if-exists :supersede)
    (loop for parent in (mapcar #'cdr (sort genealogy #'< :key #'car))
          do (format out "~a~%" parent))))

(defun genealogy-differences (from-pathname to-pathname)
  "Finds parental differences between genealogies stored in ITA format (see
WRITE-GENEALOGY-IN-ITA-FORMAT) in specified pathnames."
  (with-open-file (from-in from-pathname)
    (with-open-file (to-in to-pathname)
      (flet ((read-parent (stream)
               (handler-case
                   (parse-integer (read-line stream nil nil))
                 (t () nil))))
        (loop with stack
              for child from 0
              for from-parent = (read-parent from-in)
              for to-parent = (read-parent to-in)
              while (and from-parent to-parent)
              do (unless (= from-parent to-parent)
                   (push (list child from-parent to-parent) stack))
              finally (return stack))))))