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

;;; SLING-BLADE-RUNNER is a solution written in Common Lisp for the famous
;;; Sling-Blade-Runner puzzle[1] of ITA software. (Here[2] and here[3] are some
;;; other solutions/comments I met on the subject.)

;;; [1] http://www.itasoftware.com/careers/puzzles07.html
;;; [2] http://stuffthathappens.com/blog/2007/10/03/sling-blade-runner/
;;; [3] http://www.danielharan.com/2007/10/19/sling-blade-runner/

;;; Here is the formal description of the puzzle from ITA: How long a chain of
;;; overlapping movie titles, like "Live and Let Die Another Day of the Dead
;;; Poet's Society", can you find? (As you can see, it isn't clear what they
;;; mean by _long_. I'm assuming puzzle refers the concatenated string length of
;;; the chain. On the other hand, it's quite easy to modify the current
;;; implementation to work the other way.)

;;; To summarize, Sling-Blade-Runner puzzle eventually transforms into a finding
;;; longest path in a set of graphs problem, but with runtime and memory usage
;;; constraints. In this solution, I used random path traversals with minor
;;; recursion constraints to bound runtime. (For more information, see
;;; documentation string of FIND-LONGEST-PATH function.)

;;; While looking at given MOVIES.LST[4] by ITA, I realized below problematic
;;; facts from the general shape of the graph.

;;; [4] http://www.itasoftware.com/careers/puzzles/MOVIES.LST

;;; 1. There isn't a single (directed) graph. There is a set (of size exceeding
;;;    6000) of graphs for every node. And you need to find longest path for
;;;    each node/graph. (Because of every node points to its own graph, there is
;;;    1-1 relation between graphs in the set and the nodes.)

;;; 2. When we'd think every graph in a up-down tree scheme, AFAIC, the
;;;    distribution of the nodes are quite heterogenous. I suspect that's the
;;;    reason why my attempts with ACS (Ant Colony System) algorithm resulted
;;;    with disappointing failures.

;;; While I tried to document the code at my best, below are some general points
;;; missing in the algorithm related comments in the code.

;;; 1. After parsing movie file line by line, I assign a unique identifier to
;;;    each unique word, and store list of these identifiers instead of list of
;;;    strings. Moreover, I store these identifier lists (actually movies) in an
;;;    array and later access movies by their indexes in that array.

;;; 2. After that, I create a DISTANCE-MATRIX whose cell located at (I,J) holds
;;;    the string length of the concatenated movie titles of I'th and J'th
;;;    movies. FIND-LONGEST-MOVIE-CHAIN passes generated DISTANCE-MATRIX to the
;;;    supplied FIND-LONGEST-PATH-FN.

;;; 3. In FIND-LONGEST-PATH, to ease the neighbour lookups I create a
;;;    NEIGHBOUR-ARRAY. For further details about finding longest path, see the
;;;    DOCSTRING and comments in FIND-LONGEST-PATH and TRAVERSE-PATHS functions.

;;; So how do we achieve needed randomization in FIND-LONGEST-PATH and
;;; TRAVERSE-PATHS.

;;; 1. TRAVERSE-PATHS gets called more than once to improve the chance of
;;;    reaching the possible maximum.

;;; 2. Available neighbour nodes are traversed according to randomized weights
;;;    which is configurable by constants user supplied to FIND-LONGEST-PATH.

;;; 3. Because of a recursion depth restriction, we also randomize traversing
;;;    order of the neighbour nodes to improve the chance of node selection
;;;    possibility distribution be homogeneous among repetitive calls.

;;; And below goes the showcase on my AMD Turion TL-64 2.2GHz notebook using
;;; SBCL 1.0.12:

;;; CL-USER> (asdf:oos 'asdf:load-op :cl-ppcre)
;;; ; loading system definition from /home/vy/.sbcl/systems/cl-ppcre.asd into
;;; ; #<PACKAGE "ASDF0">
;;; ; registering #<SYSTEM :CL-PPCRE {1002818A51}> as CL-PPCRE
;;; NIL

;;; CL-USER> (asdf:oos 'asdf:load-op :bordeaux-threads)
;;; ; loading system definition from
;;; ; /home/vy/.sbcl/systems/bordeaux-threads.asd into #<PACKAGE "ASDF0">
;;; ; registering #<SYSTEM BORDEAUX-THREADS {1003BE69C1}> as BORDEAUX-THREADS
;;; ; registering #<SYSTEM BORDEAUX-THREADS-TEST {10025D8C41}> as
;;; ; BORDEAUX-THREADS-TEST
;;; NIL

;;; CL-USER> (load "/home/vy/lisp/sbr-random.lisp")
;;; T

;;; CL-USER> (time
;;;           (sbr:find-longest-movie-chain
;;;           "/home/vy/temp/MOVIES.LST"
;;;           (lambda (distance-matrix)
;;;             (sbr:find-longest-path distance-matrix
;;;                                    101
;;;                                    8000
;;;                                    0.7
;;;                                    0.6
;;;                                    2))))
;;;     4: 4028.0
;;;     6: 4026.0
;;;     7: 3968.0
;;;     9: 4017.0
;;;    10: 47.0
;;;    11: 4334.0
;;;    13: 3843.0
;;;    17: 4021.0
;;;    21: 4050.0
;;;    23: 4030.0
;;;    24: 4076.0
;;;    25: 4211.0
;;;    27: 3946.0
;;;    29: 4277.0
;;; ...
;;;  6554: 3926.0
;;;  6557: 41.0
;;;  6561: 23.0
;;; IDEN-SPEC-INDEX List: (5191 1820 2482 1635 3523 675 1403 6289 1855 892
;;;                        2868 6176 1218 6506 6126 1950 1132 2064 3254 1451
;;;                        1457 3025 3294 6177 3448 3770 2183 1737 2074 2006
;;;                        1424 1135 3480 4372 325 1570 46 1861 1859 4299 2801
;;;                        4269 2922 674 1364 3851 1272 730 4236 1215 1226 513
;;;                        1229 1497 1219 2044 6332 1199 6512 967 4188 1773
;;;                        3239 1216 3247 3061 3240 1041 2469 3753 1122 2678
;;;                        1613 813 1738 2862 1656 4054 2117 4168 1238 3244
;;;                        2854 3173 166 6476 6048 1080 2374 6529 2964 6545
;;;                        3346 2676 2377 3252 1234 905 2860 2182 1409 2375
;;;                        1579 1805 1269 1262 3995 1696 2308 898 4172 3917
;;;                        592 2946 871 839 3824 3695 1559 1560 1874 6110 1728
;;;                        3385 4142 1693 2795 2673 2190 39 6272 3486 594 488
;;;                        3904 3798 4206 2140 223 1200 1034 3913 2785 1991
;;;                        2943 2395 2720 1328 1329 1325 2000 1471 6508 2810
;;;                        2326 2954 1956 3949 6089 2691 1296 2042 374 3311
;;;                        1377 2575 3324 668 6439 2038 1096 3698 3766 3730
;;;                        2103 1140 3460 1295 720 923 6530 6532 2675 549 1856
;;;                        1857 1792 1753 2142 38 489 545 3456 3034 780 2683
;;;                        1217 1236 6340 4371 3952 2861 6523 1411 3926)
;;; Movie Title Chain : THE KILLING OF SISTER GEORGE+GEORGE OF THE JUNGLE+JUNGLE
;;; FEVER+FEVER PITCH+PITCH BLACK+BLACK HAWK DOWN+DOWN IN THE VALLEY+VALLEY
;;; GIRL+GIRL IN THE CADILLAC+CADILLAC MAN+MAN TROUBLE+TROUBLE EVERY DAY+DAY OF
;;; THE WOMAN+WOMAN ON TOP+TOP GUN+GUN CRAZY+CRAZY AS HELL+HELL NIGHT+NIGHT ON
;;; EARTH+EARTH GIRLS ARE EASY+EASY MONEY+MONEY FOR NOTHING+NOTHING BUT
;;; TROUBLE+TROUBLE IN PARADISE+PARADISE ROAD+ROAD HOUSE+HOUSE OF
;;; FRANKENSTEIN+FRANKENSTEIN AND THE MONSTER FROM HELL+HELL UP IN HARLEM+HARLEM
;;; RIVER DRIVE+DRIVE ME CRAZY+CRAZY PEOPLE+PEOPLE WILL TALK+TALK OF
;;; ANGELS+ANGELS WITH DIRTY FACES+FACES OF DEATH 4+4 LITTLE GIRLS+GIRLS WILL BE
;;; GIRLS+GIRLS OF SUMMER+SUMMER LOVERS+LOVERS AND OTHER STRANGERS+STRANGERS
;;; WHEN WE MEET+MEET JOE BLACK+BLACK DOG+DOG RUN+RUN SILENT RUN DEEP+DEEP
;;; BLUE+BLUE STEEL+STEEL DAWN+DAWN OF THE DEAD+DEAD BANG+BANG BANG YOURE
;;; DEAD+DEAD END+END OF DAYS+DAYS OF HEAVEN+HEAVEN CAN WAIT+WAIT UNTIL
;;; DARK+DARK BLUE WORLD+WORLD TRADE CENTER+CENTER STAGE+STAGE FRIGHT+FRIGHT
;;; NIGHT+NIGHT AND DAY+DAY FOR NIGHT+NIGHT MOTHER+MOTHER NIGHT+NIGHT AND THE
;;; CITY+CITY OF JOY+JOY RIDE+RIDE THE HIGH COUNTRY+COUNTRY LIFE+LIFE WITH
;;; FATHER+FATHER OF THE BRIDE+BRIDE OF FRANKENSTEIN+FRANKENSTEIN MEETS THE WOLF
;;; MAN+MAN ON FIRE+FIRE IN THE SKY+SKY HIGH+HIGH SPIRITS+SPIRITS OF THE
;;; DEAD+DEAD OF NIGHT+NIGHT FALLS ON MANHATTAN+MANHATTAN MURDER MYSTERY+MYSTERY
;;; ALASKA+ALASKA SPIRIT OF THE WILD+WILD THINGS+THINGS TO COME+COME AND GET
;;; IT+IT HAD TO BE YOU+YOU CAN COUNT ON ME+ME WITHOUT YOU+YOU ONLY LIVE
;;; ONCE+ONCE IN THE LIFE+LIFE OR SOMETHING LIKE IT+IT HAPPENED ONE NIGHT+NIGHT
;;; OF THE LIVING DEAD+DEAD MAN ON CAMPUS+CAMPUS MAN+MAN OF THE HOUSE+HOUSE OF
;;; DRACULA+DRACULA DEAD AND LOVING IT+IT HAPPENED AT THE WORLDS FAIR+FAIR
;;; GAME+GAME OF DEATH+DEATH WISH V THE FACE OF DEATH+DEATH SHIP+SHIP OF
;;; FOOLS+FOOLS RUSH IN+IN OLD CALIFORNIA+CALIFORNIA SPLIT+SPLIT SECOND+SECOND
;;; BEST+BEST MEN+MEN CRY BULLETS+BULLETS OVER BROADWAY+BROADWAY DANNY ROSE+ROSE
;;; RED+RED EYE+EYE FOR AN EYE+EYE OF GOD+GOD TOLD ME TO+TO DIE FOR+FOR YOUR
;;; EYES ONLY+ONLY THE STRONG SURVIVE A CELEBRATION OF SOUL+SOUL FOOD+FOOD OF
;;; LOVE+LOVE LIFE+LIFE AS A HOUSE+HOUSE PARTY 3+3 NINJAS KNUCKLE UP+UP CLOSE
;;; AND PERSONAL+PERSONAL BEST+BEST OF THE BEST 3 NO TURNING BACK+BACK TO
;;; SCHOOL+SCHOOL OF ROCK+ROCK STAR+STAR TREK IV THE VOYAGE HOME+HOME
;;; ALONE+ALONE IN THE DARK+DARK CITY+CITY BY THE SEA+SEA OF LOVE+LOVE
;;; HAPPY+HAPPY BIRTHDAY TO ME+ME MYSELF I+I WANT TO LIVE+LIVE AND LET DIE+DIE
;;; MOMMIE DIE+DIE MONSTER DIE+DIE HARD+HARD EIGHT+EIGHT AND A HALF WOMEN+WOMEN
;;; IN LOVE+LOVE WALKED IN+IN THE COMPANY OF MEN+MEN WITH GUNS+GUNS OF THE
;;; MAGNIFICENT SEVEN+SEVEN YEARS IN TIBET+TIBET CRY OF THE SNOW LION+LION OF
;;; THE DESERT+DESERT HEARTS+HEARTS OF DARKNESS A FILMMAKERS
;;; APOCALYPSE+APOCALYPSE NOW+NOW YOU SEE HIM NOW YOU DONT+DONT BOTHER TO
;;; KNOCK+KNOCK OFF+OFF THE BLACK+BLACK AND WHITE+WHITE HUNTER BLACK HEART+HEART
;;; CONDITION+CONDITION RED+RED RIVER+RIVER OF NO RETURN+RETURN TO HORROR
;;; HIGH+HIGH CRIMES+CRIMES OF PASSION+PASSION IN THE DESERT+DESERT BLUE+BLUE
;;; CAR+CAR 54 WHERE ARE YOU+YOU CANT TAKE IT WITH YOU+YOU LIGHT UP MY LIFE+LIFE
;;; IS BEAUTIFUL+BEAUTIFUL GIRLS+GIRLS GIRLS GIRLS+GIRLS JUST WANT TO HAVE
;;; FUN+FUN AND FANCY FREE+FREE WILLY 2 THE ADVENTURE HOME+HOME ALONE 3+3 NINJAS
;;; KICK BACK+BACK TO THE BEACH+BEACH PARTY+PARTY MONSTER+MONSTER IN A BOX+BOX
;;; OF MOON LIGHT+LIGHT OF DAY+DAY OF THE DEAD+DEAD MAN WALKING+WALKING AND
;;; TALKING+TALKING ABOUT SEX+SEX AND THE OTHER MAN+MAN OF THE YEAR+YEAR OF THE
;;; DRAGON+DRAGON SEED+SEED OF CHUCKY
;;; Raw Length          : 3486
;;; Evaluation took:
;;;   5616.771 seconds of real time
;;;   9912.863 seconds of user run time
;;;   94.95393 seconds of system run time
;;;   [Run times include 527.119 seconds GC run time.]
;;;   0 calls to %EVAL
;;;   0 page faults and
;;;   337,476,660,512 bytes consed.
;;; NIL

;;; By the way, there is a nother solution[5] I proposed for this puzzle. If
;;; we'd compare two puzzles, while the other completes in relatively longer
;;; cycles (thus finalizes in a longer runtime), randomized approach used in
;;; current implementation seems to produce good results in shorter
;;; cycles. Other implementation benefits from cached footprint heuristics to
;;; calculate selection probabilities of neighbour nodes to select in next
;;; cycles. But it should also be noted that, footprint heuristics produces
;;; better results in relatively longer runtimes. You must make your trade
;;; between runtime & quality.

;;; [5] http://www.students.itu.edu.tr/~yazicivo/sbr-footprint.lisp


(defpackage :sling-blade-runner
  (:nicknames :sbr)
  (:use :cl :cl-ppcre :bordeaux-threads)
  (:export :find-longest-movie-chain
           :find-longest-path))

(in-package :sling-blade-runner)


;;; IDEN/WORD Transition Routines

;;; We'll use identifiers (formerly IDEN) to represent words and IDEN-SPECs to
;;; represent the title of a movie as a list of identifiers. This will reduce
;;; the problem to finding longest path in a set of graphs problem where
;;; concatenated string length of movie titles represents the length of related
;;; edges.

(defstruct iden-spec
  "Struct for holding string LENGTH and IDEN-LIST of the related movie title."
  (length 0 :type fixnum :read-only t)
  (iden-list nil :read-only t))

(let (word-count word-table iden-table iden-length-table)
  (defun reset-iden/word-initials ()
    "Resets IDEN/WORD initials to parse a new file from scratch. (Not necessary,
but saves from memory space.)"
    (setq word-count 0
          word-table (make-hash-table :test #'equal)
          iden-table (make-hash-table)
          iden-length-table (make-hash-table)))
  
  (defun word-to-iden (word)
    "Returns unique number identifier of the given word."
    (or (gethash word word-table)
        (let ((iden (incf word-count)))
          (setf (gethash iden iden-table) word
                (gethash iden iden-length-table) (length word)
                (gethash word word-table) iden))))

  (defun iden-to-word (iden)
    "Returns text representation of the given word identifier."
    (or (gethash iden iden-table)
        (error "No such identifier: ~a" iden)))

  (defun movie-list-to-iden-spec-array (pathname)
    "Parses movie list (MOVIES.LST file ITA supplies) in the specified PATHNAME
into an array of IDENs."
    (coerce
     (with-open-file (in pathname)
       (loop for line = (read-line in nil nil)
             while line
             for words = (remove-if (lambda (word) (string= "" word))
                                    (split "[ \\t]+" line))
             collect (make-iden-spec :length (reduce #'+ words :key #'length)
                                     :iden-list (mapcar #'word-to-iden words))))
     '(simple-array iden-spec (*))))

  (defun iden-length (iden)
    "Returns length of string representation of word opposing supplied IDEN."
    (gethash iden iden-length-table)))


;;; Find Longest Path via Randomized Brute-Force

(defun maximize (generator-fn call-count &key (key #'identity)
                 (test-fn (constantly t)) (size 1 size-p))
  "Maximize value returned from GENERATOR-FN after CALL-COUNT repetitive calls."
  (if (null size-p)
      ;; If user didn't supply a SIZE, return the maximum.
      (loop with best-result
            with best-key
            repeat call-count
            for result = (funcall generator-fn)
            when (funcall test-fn result)
            do (when (or (null best-key)
                         (< best-key (funcall key result)))
                 (setq best-result result
                       best-key (funcall key result)))
            finally (return best-result))
      ;; Else, return a list of size SIZE of maximums.
      (subseq
       (sort
        (loop repeat call-count
              for result = (funcall generator-fn)
              when (funcall test-fn result)
              collect result)
        #'>
        :key key)
       0
       size)))

(let (distance-sum score-fraction)
  (defun initialize-constants (distance-matrix n-nodes random-effect-ratio)
    "Initializes DISTANCE-SUM and SCORE-FRACTION constants."
    (setq
     ;; Initialize DISTANCE-SUM.
     distance-sum (loop for i below n-nodes
                        sum (loop for j below n-nodes
                                  sum (aref distance-matrix i j)))
     ;; Initialize SCORE-FRACTION.
     score-fraction (/ (1- random-effect-ratio) distance-sum)))
  
  (defun traverse-paths (distance-matrix neighbour-array start-node
                         recursion-limit select-ratio random-effect-ratio)
    "Starting from START-NODE, traverse available paths using supplied
probability measures. (See FIND-LONGEST-PATH for details.)"
    (let ((recursion-count 0)
          (best-path-length 0)
          best-path)
      (labels ((submit-path (path path-length)
                 ;; See if we just find a better path.
                 (when (< best-path-length path-length)
                   ;; Update BEST-PATH and BEST-PATH-LENGTH.
                   (setq best-path path
                         best-path-length path-length)))
               (selection-score (from to)
                 ;; Assign a score to the edge between supplied FROM and TO
                 ;; nodes.
                 (+ (* random-effect-ratio (random 1.0))
                    (* (aref distance-matrix from to)
                       score-fraction)))
               (traverse (path path-length)
                 ;; Validate RECURSION-COUNT first.
                 (when (< (incf recursion-count) recursion-limit)
                   (let* ((current-node (first path))
                          (neighbour-nodes
                           (set-difference (aref neighbour-array current-node)
                                           path))
                          (neighbour-nodes-count (length neighbour-nodes)))
                     (if (zerop neighbour-nodes-count)
                         ;; If we don't have any neighbour nodes, submit this
                         ;; path.
                         (submit-path path path-length)
                         ;; Else, traverse neighbour nodes.
                         (let ((selected-neighbour-nodes
                                (maximize (lambda () (pop neighbour-nodes))
                                          neighbour-nodes-count
                                          :key (lambda (neighbour-node)
                                                 (selection-score
                                                  current-node
                                                  neighbour-node))
                                          :size (ceiling
                                                 (* select-ratio
                                                    neighbour-nodes-count)))))
                           (loop for neighbour-node
                                 ;; Randomize traversing order of the neighbour
                                 ;; nodes.
                                 in (sort selected-neighbour-nodes
                                          #'>
                                          :key (lambda (node)
                                                 (declare (ignore node))
                                                 (random 1.0)))
                                 do (traverse (cons neighbour-node path)
                                              (+ path-length
                                                 (aref distance-matrix
                                                       current-node
                                                       neighbour-node))))))))))
        ;; Traverse available neighbour nodes.
        (traverse (list start-node) 0.0)
        ;; Return best path found-so-far, if any.
        (unless (zerop best-path-length)
          (cons best-path best-path-length))))))


(defun find-longest-path (distance-matrix iteration-count recursion-limit
                          select-ratio random-effect-ratio worker-count)
  "Finds longest path in the supplied set of graphs defined by DISTANCE-MATRIX
via randomized brute-force.

ITERATION-COUNT is the number of iterations will be taken to randomly traverse
available paths.

RECURSION-DEPTH is the maximum number of recursive calls while traversing
through in the branches of the graph.

At each step SELECT-RATIO (0..1) portion of available nodes will be
selected. (If SELECT-RATIO is equal to 1, function will traverse through as
plain brute-force.)

RANDOM-EFFECT-RATIO is the randomization multiplier used to compute a neighbour
node's selection probability score.

  (+ (* RANDOM-EFFECT-RATIO (RANDOM 1.0))
     (* (1- RANDOM-EFFECT-RATIO)
        (/ NODE-DISTANCE DISTANCE-SUM)))

Iterations will get divided among WORKER-COUNT threads."
  (check-type iteration-count (integer 1 *))
  (check-type recursion-limit (integer 1 *)) 
  (check-type select-ratio (float 0.0 1.0))
  (check-type random-effect-ratio (float 0.0 1.0))
  (let* ((n-nodes (array-dimension distance-matrix 0))
         (neighbour-array (make-array n-nodes
                                      :element-type 'list
                                      :initial-element nil)))
    ;; Fill NEIGHBOUR-ARRAY.
    (loop for current-node below n-nodes
          do (loop for neighbour-node below n-nodes
                   unless (zerop (aref distance-matrix
                                       current-node
                                       neighbour-node))
                   do (push neighbour-node
                            (aref neighbour-array current-node))))
    ;; Initialize graph constants.
    (initialize-constants distance-matrix n-nodes random-effect-ratio)
    (let* ((node-stack
            (loop for node below n-nodes
                  when (aref neighbour-array node)
                  collect node))
           (result
            ;; Maximize search for NODE-STACK.
            (maximize
             (lambda ()
               ;; Maximize search for (POP NODE-STACK).
               (let ((start-node (pop node-stack))
                     (result-stack-lock (make-lock "RESULT-STACK Lock"))
                     result-stack)
                 (format t "~5d: " start-node) ; DEBUG
                 (flet ((submit-result (result)
                          ;; Push submitted RESULT to RESULT-STACK.
                          (with-lock-held (result-stack-lock)
                            (push result result-stack))))
                   (let ((workers
                          ;; Run workers.
                          (loop with iteration-offset
                                = (truncate (/ iteration-count worker-count))
                                for iteration = 0
                                then (+ iteration iteration-offset)
                                while (<= iteration iteration-count)
                                collect (make-thread
                                         (lambda ()
                                           (submit-result
                                            (maximize
                                             (lambda ()
                                               (traverse-paths distance-matrix
                                                               neighbour-array
                                                               start-node
                                                               recursion-limit
                                                               select-ratio
                                                               random-effect-ratio))
                                             iteration-offset
                                             :key #'cdr
                                             :test-fn #'identity)))
                                         :name (format nil "W#~a" iteration)))))
                     ;; Wait workers to finish their work.
                     (loop while (some #'thread-alive-p workers)
                           do (sleep 0.1))))
                 ;; Maximize search for (POP RESULT-STACK).
                 (maximize
                  (lambda () (pop result-stack))
                  (length result-stack)
                  :key #'cdr
                  :test-fn #'identity)))
             (length node-stack)
             :key #'cdr
             :test-fn (lambda (result)
                        (when result
                          (format t "~7d~%" (cdr result)) ; DEBUG
                          t)))))
      (values (nreverse (car result)) (cdr result)))))


;;; Main Stuff

(defun find-longest-movie-chain (pathname find-longest-path-fn)
  "Finds longest movie chain (e.g. Sling Blade Runner) in the specified movie
database saved in PATHNAME."
  ;; Reset previously computed IDEN/WORD data, if any.
  (reset-iden/word-initials)
  (let* ((iden-spec-array (movie-list-to-iden-spec-array pathname))
         (iden-spec-count (length iden-spec-array))
         ;; Storing list of IDEN-SPEC indexes sharing same first words in
         ;; HEAD-IDEN-TABLE to easy the build of DISTANCE-MATRIX. (IDEN of first
         ;; word forms the hash keys.)
         (head-iden-table (make-hash-table))
         ;; Our relations matrix to form graph we will traverse through.
         (distance-matrix (make-array (list iden-spec-count iden-spec-count)
                                      :element-type 'fixnum
                                      :initial-element 0)))
    ;; Fill HEAD-IDEN-TABLE.
    (loop for iden-spec across iden-spec-array
          for iden-spec-index from 0
          for head-iden = (first (iden-spec-iden-list iden-spec))
          do (if (gethash head-iden head-iden-table)
                 (push iden-spec-index (gethash head-iden head-iden-table))
                 (setf (gethash head-iden head-iden-table)
                       (list iden-spec-index))))
    ;; Using HEAD-IDEN-TABLE, fill DISTANCE-MATRIX.
    (labels ((iden-list-length (iden-list)
               ;; Length of string representation of the supplied IDEN-LIST.
               (reduce #'+
                       iden-list
                       :key (lambda (iden) (iden-length iden))))
             (chain-length (from-iden-spec to-iden-spec)
               ;; Length of string representation of supplied IDEN-SPEC chain.
               (+ (iden-list-length (iden-spec-iden-list from-iden-spec))
                  ;; Beware that chain length for "FOO BAR + BAR BAZ" is
                  ;; calculated as (3 + 3) + 3 = 9, instead of 6 or 12. While
                  ;; this information isn't actually true, it eases distance
                  ;; calculation while finding longest path.
                  (iden-list-length (rest (iden-spec-iden-list to-iden-spec))))))
      (loop for iden-spec-index from 0
            for iden-spec across iden-spec-array
            do (loop for neighbour-iden-spec-index
                     ;; Find movie titles starting with current movie title's
                     ;; last word.
                     in (gethash (first (last (iden-spec-iden-list iden-spec)))
                                 head-iden-table)
                     for neighbour-iden-spec = (aref iden-spec-array
                                                     neighbour-iden-spec-index)
                     unless (= iden-spec-index neighbour-iden-spec-index)
                     do (setf (aref distance-matrix
                                    iden-spec-index
                                    neighbour-iden-spec-index)
                              (chain-length iden-spec neighbour-iden-spec)))))
    ;; Find longest path in the graph defined by DISTANCE-MATRIX.
    (let* ((solution-iden-spec-index-list
            (funcall find-longest-path-fn distance-matrix))
           (movie-chain-string
            (format nil
                    "~{~a~^+~}"
                    (mapcar
                     (lambda (iden-spec-index)
                       (format nil
                               "~{~a~^ ~}"
                               (mapcar #'iden-to-word
                                       (iden-spec-iden-list
                                        (aref iden-spec-array
                                              iden-spec-index)))))
                     solution-iden-spec-index-list))))
      (format t
              (concatenate 'string
                           "IDEN-SPEC-INDEX List: ~a~%"
                           "Movie Title Chain   : ~a~%"
                           "Raw Length          : ~a~%")
              solution-iden-spec-index-list
              movie-chain-string
              (length movie-chain-string)))))