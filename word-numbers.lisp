;;; Copyright (c) 2007, Volkan YAZICI <volkan.yazici@gmail.com>
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

;;; WORD-NUMBERS is a solution written in Common Lisp for the famous word
;;; numbers puzzle[1] of ITA software. (Here[2] and here[3] are some other
;;; solutions I met. By the way, none of these three solutions produce same
;;; results, despite they are quite similar.)

;;; [1] http://www.itasoftware.com/careers/puzzles07.html
;;; [2] http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers4/
;;; [3] http://mignon.jottit.com/ita

;;; During program execution, numbers are handled as a list of atoms. There're
;;; two kinds of atoms: plain (one, two, three, ..., ninehundredninetynine) and
;;; pseudo (thousand, million, billion, ...). Atom informations are kept in
;;; +ATOM-DATA+ array. To reduce computation overhead, algorithm jumps over
;;; pseudo atom ranges. (See PSEUDO-BLOCK-LENGTH and PSEUDO-BLOCK-SUM.)
;;; Furthermore, we don't need the full string representation of an atom list to
;;; sort its merged cartesian products. Computing string representation of the
;;; freshly added atoms are enough. (See TRAVERSE-COMBINATIONS for further
;;; explanation.)

;;; CL-USER> (time (word-numbers:get-nth-character 51000000000))
;;; Evaluation took:
;;;   0.026 seconds of real time
;;;   0.028001 seconds of total run time (0.028001 user, 0.000000 system)
;;;   107.69% CPU
;;;   57,843,039 processor cycles
;;;   749,120 bytes consed
;;; (51000000055 413148547219342655 (706 0 244 1 677) 676243705
;;;  "sixhundredseventysixmilliontwohundredfortythreethousandsevenhundredfive")

;;; From left to right, list elements represent
;;; - Distance so far. (Including string representation of the last number.)
;;; - Number summation so far.
;;; - Atom list of last number.
;;; - Last number.
;;; - String representation of last number.

;;; There is no limitation on the will be computed character length target.
;;; Expand pseudo atoms in +ATOM-DATA+ as you wish.

(defpackage :word-numbers
  (:use :cl)
  (:export :get-nth-character))

(in-package :word-numbers)

(defun number-to-string (num)
  (remove-if-not #'alpha-char-p (format nil "~r" num)))

;;; Instead of storing string (or number) representation of a number while
;;; counting characters, I'll keep a list of atoms to represent the number.
;;; There'll be two kinds of atom: pseudo (thousand, million, billion, ...) and
;;; plain (one, two, three, ..., ninehundredninetynine). +ATOM-DATA+ dictionary
;;; will save us from the computing overhead of some of the properties (length,
;;; word, number) of these atoms in case of need.

(defvar +atom-data+
  ;; Order is important! First pseudo atoms.
  (concatenate
   'vector
   ;; Pseudo atoms. (Order is important. First pseudo atoms with lower levels.)
   ;; You can add as much (billion, trillion, quadrillion, ...) pseudo as you
   ;; want to this list. Remaining work will automatically get done by
   ;; WHOLE-PSEUDO-BLOCK-LENGTH, PSEUDO-BLOCK-LENGTH and PSEUDO-BLOCK-SUM
   ;; functions.
   #((8 "thousand" 1000)
     (7 "million" 1000000))
   ;; Plain atoms.
   (coerce
    (loop for i from 1 to 999
          for str = (number-to-string i)
          collect (list (length str) str i))
    '(simple-array t (*))))
  "List of (ATOM-LENGTH ATOM-WORD ATOM-NUMBER) triples.")

(defconstant +plain-atom-count+ 999)

(defconstant +pseudo-atom-count+ (- (length +atom-data+) +plain-atom-count+))

(defvar +plain-atoms+
  (loop for i below +plain-atom-count+
        collect (+ i +pseudo-atom-count+)))

(defvar +pseudo-atoms+
  (loop for i below +pseudo-atom-count+ collect i))

(defun pseudo-atom-p (atom)
  (< atom +pseudo-atom-count+))

(defun length-of (atom)
  (first (aref +atom-data+ atom)))

(defun word-of (atom)
  (second (aref +atom-data+ atom)))

(defun number-of (atom)
  (third (aref +atom-data+ atom)))

(defun atom-list-string (atom-list)
  "String representation of the supplied ATOM-LIST."
  (apply #'concatenate 'string (mapcar #'word-of (reverse atom-list))))

(let ((table (make-hash-table
              :test #'eq
              :size (* (length +plain-atoms+)
                       (length +pseudo-atoms+)))))
  (defun atom-list-short-string-internal (plain pseudo)
    "Concatenated string representation given plain and pseudo atoms."
    (let ((key (+ (number-of pseudo) plain)))
      (or (gethash key table)
          (setf (gethash key table)
                (concatenate
                 'string
                 (word-of plain)
                 (word-of pseudo)))))))

(defun atom-list-short-string (atom-list)
  "String representation of the last two atoms in the supplied ATOM-LIST."
  (if (pseudo-atom-p (first atom-list))
      (atom-list-short-string-internal (second atom-list) (first atom-list))
      (word-of (first atom-list))))

(defun atom-list-length (atom-list)
  "Length of the string representation of the supplied ATOM-LIST."
  (reduce #'+ atom-list :key #'length-of))

(defun atom-list-number (atom-list)
  "Number representation of the supplied ATOM-LIST."
  (loop for (plain pseudo) on (reverse atom-list) by #'cddr
        sum (* (number-of plain)
               (if pseudo (number-of pseudo) 1))))

;;; Total number of characters
;;; ... from 1 to 999: _18440_

;;; ... from 1,000 to 999,000 (just 3 zeros at the end):
;;;       18440 + (999 * 8) = 26432
;;; ... from 1,001 to 999,999 (no three zeros at the end):
;;;       (999 * 26432) + (999 * 18440) = 44,827,128

;;; ... from 1 to 999,999:
;;;       18440 + 26432 + 44,827,128 = _44,872,000_

;;; ... from 1,000,000 to 999,000,000 (just 6 zeros at the end):
;;;       18440 + (999 * 7) = 25,433
;;; ... from 1,000,001 to 999,999,999 (no 6 zeros at the end):
;;;       (999,999 * 25,433) + (999 * 44,872,000) = 70,260,102,567

;;; ... from 1 to 999,999,999:
;;;       44,872,000 + 25,433 + 70,260,102,567 = _70,305,000,000_

(let ((memory (make-array (1+ +pseudo-atom-count+)
                          :element-type 'integer
                          :initial-element 0)))
  (defun whole-pseudo-block-length (level)
    "Total number of characters in the specified level. (Level 0 represents 1 to
999; Level 1 represents 1 to 999,999, Level 3 represents 1 to 999,999,999, and
so on.)"
    ;; Calculation shows that, `L_k = 1000 L_{k-1} + (L_0 + 999 \rho_k) v_k'
    ;; where L_k represents the level, \rho_k represents the pseudo atom length
    ;; (e.g., for "thousand", `\rho_k = 8') and v_k represents the VALUE-OF the
    ;; pseudo atom (e.g., for "thousand", `v_k = 1000').
    (let ((found (aref memory level)))
      (if (> found 0)
          found
          (setf (aref memory level)
                (if (zerop level)
                    (reduce #'+ +plain-atoms+ :key #'length-of)
                    (let ((pseudo (1- level)))
                      (+ (* 1000 (whole-pseudo-block-length pseudo))
                         (* (+ (whole-pseudo-block-length 0)
                               (* 999 (length (word-of pseudo))))
                            (number-of pseudo))))))))))

(defun pseudo-block-length (plain pseudo)
  "Returns how many characters needed to traverse in specified pseudo block."
  (+ (* (1- (number-of pseudo))
        (+ (length-of plain)
           (length-of pseudo)))
     (whole-pseudo-block-length pseudo)))

(defun pseudo-block-sum (plain pseudo)
  "Returns the sum of integers in the supplied pseudo block."
  (let ((n (number-of plain))
        (p (number-of pseudo)))
    (* p (1- p) (+ n (/ 2)))))

(defun merged-cartesian-product (atom-lists pseudo-atoms)
  "Produces cartesian product of supplied ATOM-LISTS and PSEUDO-ATOMS, and
concatenates result with the ATOM-LISTS."
  (if (null pseudo-atoms)
      atom-lists
      (nconc
       (mapcar
        (lambda (atom) (cons (first pseudo-atoms) atom))
        atom-lists)
       (merged-cartesian-product atom-lists (rest pseudo-atoms)))))

(defun traverse-combinations (submit skip-block-p atom-list plain-atoms pseudo-atoms)
  "Traverse new combinations generated over supplied ATOM-LIST using given
PLAIN-ATOMS and PSEUDO-ATOMS."
  (mapc
   (lambda (atom-list)
     ;; Submit atom list.
     (funcall submit atom-list)
     (let ((pseudo (first atom-list))
           (plain (second atom-list)))
       ;; If this is a pseudo atom (which means we have a list ending with
       ;; "thousand" or "million" atoms), continue by processing this block
       ;; first.
       (if (and (pseudo-atom-p pseudo)
                ;; Am I allowed to skip this block instead of calculating every
                ;; number in the inner blocks?
                (not (funcall skip-block-p
                              (pseudo-block-length plain pseudo)
                              (pseudo-block-sum plain pseudo))))
           (traverse-combinations
            submit
            skip-block-p
            atom-list
            plain-atoms
            ;; Just keep pseudo atoms lower than current pseudo level. (For
            ;; instance, if we just used a "million", now passing "thousand" is
            ;; allowed only.)
            (remove-if-not
             (lambda (pseudo-atom) (< pseudo-atom pseudo))
             pseudo-atoms)))))
   ;; Sort generated atom lists according to their string order.
   (sort
    ;; First generate a new list of atom lists by appending plain atoms to the
    ;; current atom list. And then concatenate this list with the cartesian
    ;; product of same list on "thousand", "million" atoms; in other terms,
    ;; produce the merged cartesian product.
    (merged-cartesian-product
     (mapcar
      (lambda (atom) (cons atom atom-list))
      plain-atoms)
     pseudo-atoms)
    #'string<
    ;; While sorting atom lists, we don't need to get the string representation
    ;; of the whole atom list. Because of we just created the last two (or one)
    ;; atom(s) in the list, it is ok to just check their string representation.
    :key #'atom-list-short-string)))

(defun get-nth-character (n)
  "Get Nth character of the sorted list of string representation of numbers from
1 to 999,999,999."
  (let ((distance 0)
        (sum 0))
    (flet ((submit (atom-list)
             (incf distance (atom-list-length atom-list))
             (incf sum (atom-list-number atom-list))
             (if (>= distance n)
                 (return-from get-nth-character
                   (list distance
                         sum
                         atom-list
                         (atom-list-number atom-list)
                         (atom-list-string atom-list)))))
           (skip-block-p (block-length block-sum)
             ;; If will be skipped block length doesn't exceed the target
             ;; distance, let it skip the block.
             (when (not (> (+ block-length distance) n))
               (incf distance block-length)
               (incf sum block-sum))))
      (traverse-combinations
       #'submit
       #'skip-block-p
       nil
       +plain-atoms+
       +pseudo-atoms+))))