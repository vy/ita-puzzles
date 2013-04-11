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
;;; constraints. In this solution, I tried to exploit random walks by
;;; encouraging traversals to follow paths previously guaranteed to be
;;; long. (For more information, see documentation string of FIND-LONGEST-PATH
;;; function.)

;;; This is a very simple and plain implementation without any Common Lisp
;;; specific compiler optimizations. While you can try your chance for further
;;; optimizations, as a next step, it is possible to increase the performance by
;;; distributing read-only tasks among worker threads. Furthermore, calls with
;;; different START-NODEs supplied to FIND-LONGEST-PATH can be distributed
;;; between threads to gain performance from parallel processing.

;;; While looking at given MOVIES.LST[4] by ITA, I realized below problematic
;;; facts from the general shape of the whole picture.

;;; [4] http://www.itasoftware.com/careers/puzzles/MOVIES.LST

;;; 1. There isn't a single (directed) graph. There is a set (of size exceeding
;;;    6000) of graphs for every node. And you need to find longest path for
;;;    each node/graph. (Because of every node points to its own graph, there is
;;;    1-1 relation between graphs in the set and the nodes.)

;;; 2. When we'd think every graph in a up-down tree scheme, AFAIC, the
;;;    distribution of the nodes are quite heterogenous. I suspect that's the
;;;    reason why my attempts with ACS (Ant Colony System) algorithm resulted
;;;    with disappointing failures.

;;; To use the code, you must supply pathname for file holding movie titles and
;;; a function to find longest path to FIND-LONGEST-MOVIE-CHAIN. That's all, and
;;; then you're ready to go. Of course batteries are included! See
;;; FIND-LONGEST-PATH-WRAPPER.

;;; And below goes the showcase on my AMD Turion TL-64 2.2GHz notebook using
;;; SBCL 1.0.12:

;;; CL-USER> (asdf:oos 'asdf:load-op :cl-ppcre)
;;; ; loading system definition from /home/vy/.sbcl/systems/cl-ppcre.asd into
;;; ; #<PACKAGE "ASDF0">
;;; ; registering #<SYSTEM :CL-PPCRE {1002818A51}> as CL-PPCRE
;;; NIL

;;; CL-USER> (load "/home/vy/lisp/sbr-footprint.lisp")
;;; T

;;; CL-USER> (time
;;;           (sbr:find-longest-movie-chain
;;;            "/home/vy/temp/MOVIES.LST"
;;;            #'sbr:find-longest-path-wrapper))
;;;     4:  3111.0
;;;     6:  4258.0
;;;     7:  3987.0
;;;     9:  3968.0
;;;   ...:   ....0
;;;  6550:  3890.0
;;;  6551:  3791.0
;;;  6553:  3998.0
;;;  6554:  4027.0
;;;  6557:    41.0
;;;  6561:    23.0
;;; Length: 4557.0
;;; Nodes: (4121 6469 650 2468 1451 1457 3025 3294 6177 3448 3770 2183 1738
;;;         2868 6176 1216 3252 1238 3239 1218 6506 6126 1950 1132 2074 2006
;;;         1424 1135 3480 4373 3652 1219 2044 6332 1205 4218 489 545 3455
;;;         1855 892 2860 2190 39 6272 3486 593 594 488 3904 3798 4208 3508
;;;         815 3034 780 2683 1217 1226 513 1236 6340 4371 3952 2862 1656 4054
;;;         2114 2103 1140 3460 1295 720 923 6530 6545 3346 2676 2375 1579
;;;         1805 1269 1252 2089 854 4293 6145 1122 2678 1613 813 1737 2064
;;;         3244 2854 3174 1210 319 1564 320 2038 1096 3698 3766 3731 2964
;;;         6532 2673 2188 3456 3033 2182 1409 2372 6512 967 4188 1773 3240
;;;         1038 325 1570 46 1856 1859 4299 2801 4269 2922 675 1407 2810 2326
;;;         2954 1956 3949 6089 2691 1296 2042 374 3311 1377 2575 3324 676
;;;         2943 2395 2720 1325 2000 1475 3423 3462 2977 3851 1273 3913 2777
;;;         1262 3995 1696 2308 898 4172 3917 592 2946 871 839 3824 3695 1559
;;;         1560 1874 6110 1728 3385 4142 1693 2795 2675 549 1861 1857 1792
;;;         1753 2140 223 1198 730 4236 1215 1234 905 2861 6523 1411 3926)
;;; String Representation: SOMETHING WILD+WILD BILL+BILL AND TEDS BOGUS
;;; JOURNEY+JOURNEY TO THE CENTER OF THE EARTH+EARTH GIRLS ARE EASY+EASY
;;; MONEY+MONEY FOR NOTHING+NOTHING BUT TROUBLE+TROUBLE IN PARADISE+PARADISE
;;; ROAD+ROAD HOUSE+HOUSE OF FRANKENSTEIN+FRANKENSTEIN MEETS THE WOLF MAN+MAN
;;; TROUBLE+TROUBLE EVERY DAY+DAY FOR NIGHT+NIGHT OF THE LIVING DEAD+DEAD OF
;;; NIGHT+NIGHT AND DAY+DAY OF THE WOMAN+WOMAN ON TOP+TOP GUN+GUN CRAZY+CRAZY AS
;;; HELL+HELL UP IN HARLEM+HARLEM RIVER DRIVE+DRIVE ME CRAZY+CRAZY PEOPLE+PEOPLE
;;; WILL TALK+TALK RADIO+RADIO DAYS+DAYS OF HEAVEN+HEAVEN CAN WAIT+WAIT UNTIL
;;; DARK+DARK STAR+STAR WARS EPISODE V THE EMPIRE STRIKES BACK+BACK TO THE
;;; BEACH+BEACH PARTY+PARTY GIRL+GIRL IN THE CADILLAC+CADILLAC MAN+MAN OF THE
;;; HOUSE+HOUSE PARTY 3+3 NINJAS KNUCKLE UP+UP CLOSE AND PERSONAL+PERSONAL
;;; BEST+BEST OF THE BEST+BEST OF THE BEST 3 NO TURNING BACK+BACK TO
;;; SCHOOL+SCHOOL OF ROCK+ROCK STAR+STAR TREK THE MOTION PICTURE+PICTURE
;;; BRIDE+BRIDE OF THE MONSTER+MONSTER IN A BOX+BOX OF MOON LIGHT+LIGHT OF
;;; DAY+DAY OF THE DEAD+DEAD BANG+BANG BANG YOURE DEAD+DEAD MAN WALKING+WALKING
;;; AND TALKING+TALKING ABOUT SEX+SEX AND THE OTHER MAN+MAN ON FIRE+FIRE IN THE
;;; SKY+SKY HIGH+HIGH SCHOOL HIGH+HIGH CRIMES+CRIMES OF PASSION+PASSION IN THE
;;; DESERT+DESERT BLUE+BLUE CAR+CAR 54 WHERE ARE YOU+YOU CANT TAKE IT WITH
;;; YOU+YOU ONLY LIVE ONCE+ONCE IN THE LIFE+LIFE OR SOMETHING LIKE IT+IT
;;; HAPPENED AT THE WORLDS FAIR+FAIR GAME+GAME OF DEATH+DEATH WISH V THE FACE OF
;;; DEATH+DEATH BECOMES HER+HER MAJESTY MRS BROWN+BROWN SUGAR+SUGAR TOWN+TOWN
;;; AND COUNTRY+COUNTRY LIFE+LIFE WITH FATHER+FATHER OF THE BRIDE+BRIDE OF
;;; FRANKENSTEIN+FRANKENSTEIN AND THE MONSTER FROM HELL+HELL NIGHT+NIGHT FALLS
;;; ON MANHATTAN+MANHATTAN MURDER MYSTERY+MYSTERY DATE+DATE WITH AN ANGEL+ANGEL
;;; EYES+EYES OF AN ANGEL+ANGEL HEART+HEART CONDITION+CONDITION RED+RED
;;; RIVER+RIVER OF NO RETURN+RETURN TO ME+ME WITHOUT YOU+YOU LIGHT UP MY
;;; LIFE+LIFE AS A HOUSE+HOUSE PARTY+PARTY MONSTER+MONSTER HOUSE+HOUSE OF
;;; DRACULA+DRACULA DEAD AND LOVING IT+IT CONQUERED THE WORLD+WORLD TRADE
;;; CENTER+CENTER STAGE+STAGE FRIGHT+FRIGHT NIGHT+NIGHT AND THE CITY+CITY OF
;;; ANGELS+ANGELS WITH DIRTY FACES+FACES OF DEATH 4+4 LITTLE GIRLS+GIRLS GIRLS
;;; GIRLS+GIRLS OF SUMMER+SUMMER LOVERS+LOVERS AND OTHER STRANGERS+STRANGERS
;;; WHEN WE MEET+MEET JOE BLACK+BLACK HAWK DOWN+DOWN WITH LOVE+LOVE WALKED IN+IN
;;; THE COMPANY OF MEN+MEN WITH GUNS+GUNS OF THE MAGNIFICENT SEVEN+SEVEN YEARS
;;; IN TIBET+TIBET CRY OF THE SNOW LION+LION OF THE DESERT+DESERT HEARTS+HEARTS
;;; OF DARKNESS A FILMMAKERS APOCALYPSE+APOCALYPSE NOW+NOW YOU SEE HIM NOW YOU
;;; DONT+DONT BOTHER TO KNOCK+KNOCK OFF+OFF THE BLACK+BLACK LIKE ME+ME MYSELF
;;; I+I WANT TO LIVE+LIVE AND LET DIE+DIE HARD+HARD EIGHT+EIGHT MEN OUT+OUT OF
;;; THE PAST+PAST MIDNIGHT+MIDNIGHT RUN+RUN SILENT RUN DEEP+DEEP BLUE SEA+SEA OF
;;; LOVE+LOVE AND DEATH+DEATH SHIP+SHIP OF FOOLS+FOOLS RUSH IN+IN OLD
;;; CALIFORNIA+CALIFORNIA SPLIT+SPLIT SECOND+SECOND BEST+BEST MEN+MEN CRY
;;; BULLETS+BULLETS OVER BROADWAY+BROADWAY DANNY ROSE+ROSE RED+RED EYE+EYE FOR
;;; AN EYE+EYE OF GOD+GOD TOLD ME TO+TO DIE FOR+FOR YOUR EYES ONLY+ONLY THE
;;; STRONG SURVIVE A CELEBRATION OF SOUL+SOUL FOOD+FOOD OF LOVE+LOVE LIFE+LIFE
;;; IS BEAUTIFUL+BEAUTIFUL GIRLS+GIRLS WILL BE GIRLS+GIRLS JUST WANT TO HAVE
;;; FUN+FUN AND FANCY FREE+FREE WILLY 2 THE ADVENTURE HOME+HOME ALONE+ALONE IN
;;; THE DARK+DARK BLUE+BLUE STEEL+STEEL DAWN+DAWN OF THE DEAD+DEAD MAN ON
;;; CAMPUS+CAMPUS MAN+MAN OF THE YEAR+YEAR OF THE DRAGON+DRAGON SEED+SEED OF
;;; CHUCKY
;;; Evaluation took:
;;;   14809.012 seconds of real time
;;;   13833.116 seconds of user run time
;;;   951.6555 seconds of system run time
;;;   [Run times include 6497.46 seconds GC run time.]
;;;   0 calls to %EVAL
;;;   0 page faults and
;;;   165,995,908,560 bytes consed.
;;; NIL

;;; By the way, there is another solution[5] I proposed for this puzzle. If we'd
;;; compare two implementations, while the other completes in relatively shorter
;;; cycles (thus finalizes in a shorter runtime), heuristic approach used in
;;; current implementation seems to produce better results. And in current
;;; method, instead of blindly relying on random traversals, algorithm benefits
;;; from previous traversals designated by footprints. But it should also be
;;; noted that, random traversals produces quite good results too. You must make
;;; your trade between runtime & quality.

;;; [5] http://www.students.itu.edu.tr/~yazicivo/files/sbr-random.lisp


(defpackage :sling-blade-runner
  (:nicknames :sbr)
  (:use :cl :cl-ppcre)
  (:export :find-longest-movie-chain
           :find-longest-path-wrapper))

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


;;; Utility Routines

(defun collect-extremums (source &key input-size (key #'identity)
                          (test (constantly t)) (output-size 1)
                          (compar #'>))
  "If SOURCE is of type function, return list of extremums of size OUTPUT-SIZE by
calling SOURCE function INPUT-SIZE times. If INPUT-SIZE is not specified,
return on NIL.

If SOURCE is of type list, OUTPUT-SIZE extremums collected within the list will
be returned. If INPUT-SIZE is specified, first INPUT-SIZE elements of supplied
SOURCE will get processed, otherwise SOURCE will get walked till its end.

TEST function is used to test which items to collect and COMPAR function is used
to order collected elements."
  (let ((extremums
         (sort
          (cond
            ((listp source)
             (loop for n-iter below (or input-size (length source))
                   for item in source
                   when (funcall test item)
                   collect item))
            ((functionp source)
             (labels ((walk-after-test (accum n-iter item)
                        (walk
                         (if (funcall test item)
                             (cons item accum)
                             accum)
                         (1+ n-iter)))
                      (walk (accum n-iter)
                        (if input-size
                            (if (< n-iter input-size)
                                (walk-after-test accum n-iter (funcall source))
                                accum)
                            (let ((item (funcall source)))
                              (if item
                                  (walk-after-test accum n-iter item)
                                  accum)))))
               (walk nil 0))))
          compar
          :key key)))
    (if (< (length extremums) output-size)
        extremums
        (subseq extremums 0 output-size))))


;;; PATH Type and Related Routines

(defstruct path
  "Struct to hold information of a path."
  (nodes nil)
  (length 0.0 :type float))

;;; During path traversals, millions (billions?) of MAKE-PATH calls will quickly
;;; exhaust available memory. For this purpose, there is a PATH store reseted
;;; just before every path traversal for a specific START-NODE. (Said garbage
;;; collection? In SBCL 1.0.12 forcing garbage collection after every path
;;; traversal takes %70 of the runtime.)

(defun valid-path-p (path)
  (and path (< 0.0 (path-length path))))

(defun reset-path (path)
    "Resets supplied PATH."
    (setf (path-length path) 0.0
          (path-nodes path) nil)
    path)

(let (storage-array free-slots used-slots storage-size extension-size)
  (defun path-store-initialized-p ()
    "Checks if path store is previously initialized."
    (arrayp storage-array))

  (defun initialize-path-store-in-range (start offset &key force-new)
    "Initializes STORAGE-ARRAY elements between given range."
    (loop with end = (+ start offset)
          for slot from start below end
          do (progn
               (if (and (typep (aref storage-array slot) 'path)
                        (not force-new))
                   ;; Reset PATH.
                   (reset-path (aref storage-array slot))
                   ;; Create PATH from scratch.
                   (setf (aref storage-array slot) (make-path)))
               (push (- end slot 1) free-slots))))

  (defun reset-path-store ()
    "Resets all available PATHs in the store."
    (setq used-slots nil
          free-slots (loop for slot below storage-size
                           collect (progn
                                     (reset-path (aref storage-array slot))
                                     slot))))

  (defun initialize-path-store (size &optional (extension 10))
    "Initializes PATH store."
    ;; Initialize array.
    (unless (and (arrayp storage-array)
                 (= storage-size size))
      (setf storage-array (make-array size
                                      :element-type 'path
                                      :initial-element (make-path)
                                      :adjustable t
                                      :fill-pointer 0)
            ;; Set STORAGE-SIZE and EXTENSION-SIZE.
            storage-size size
            extension-size extension))
    ;; Fill & initialize slots.
    (setq used-slots nil)
    (initialize-path-store-in-range 0 size :force-new t)
    ;; Place cursor.
    (setf (fill-pointer storage-array) 0))

  (defun allocate-path (&key (length 0.0) nodes)
    "Allocate a fresh PATH from store."
    ;; Sustain compiler warnings first.
    (declare (type (array path (*)) storage-array)
             (type fixnum extension-size))
    ;; Ensure that there are free slots in the store.
    (when (endp free-slots)
      (adjust-array storage-array
                    (+ storage-size extension-size)
                    :fill-pointer storage-size)
      (initialize-path-store-in-range storage-size extension-size)
      (incf storage-size extension-size))
    ;; Return next available slot.
    (let* ((free-slot (pop free-slots))
           (path (aref storage-array free-slot)))
      (push free-slot used-slots)
      (setf (path-length path) length
            (path-nodes path) nodes)
      path)))


;;; Longest Path Algorithm

(defconstant +initial-footprint+ 0.1
  "Minimum amount of footprint an edge can have.")

(let (footprint-matrix footprint-limit neighbour-array visited-edges
      path-length-limit)
  (defun traverse-paths (distance-matrix distance-limit start-node n-recursions
                         neighbour-select-ratio random-effect-weight
                         footprint-effect-weight distance-effect-weight)
    (let ((recursion-count 0)
          (best-path))
      (labels ((submit-path (path)
                 ;; Update BEST-PATH and BEST-PATH-LENGTH, if needed.
                 (when (or (null best-path)
                           (< (path-length best-path) (path-length path)))
                   (setq best-path path)))
               (selection-score (i j)
                 ;; Assign a selection score to the edge between nodes I and J.
                 (+ (* random-effect-weight
                       (random 1.0))
                    (* footprint-effect-weight
                       (- 1
                          (/ (aref footprint-matrix i j)
                             footprint-limit)))
                    (* distance-effect-weight
                       (aref distance-matrix i j)
                       (/ distance-limit))))
               (traverse (path)
                 ;; Validate RECURSION-COUNT first.
                 (when (< (incf recursion-count) n-recursions)
                   (let* ((current-node (first (path-nodes path)))
                          (neighbour-nodes
                           (set-difference (aref neighbour-array current-node)
                                           (path-nodes path)))
                          (neighbour-nodes-count (length neighbour-nodes)))
                     (if (zerop neighbour-nodes-count)
                         ;; If there is no neighbour nodes left to visit, submit
                         ;; this path.
                         (submit-path path)
                         ;; Else, traverse available neighbour nodes.
                         (loop for neighbour-node
                               in (collect-extremums
                                   neighbour-nodes
                                   :key (lambda (neighbour-node)
                                          (selection-score
                                           current-node
                                           neighbour-node))
                                   :output-size (ceiling
                                                 (* neighbour-select-ratio
                                                    neighbour-nodes-count)))
                               do (traverse
                                   (allocate-path
                                    :nodes (cons neighbour-node (path-nodes path))
                                    :length (+ (path-length path)
                                               (aref distance-matrix
                                                     current-node
                                                     neighbour-node))))))))))
        ;; Traverse available neighbour nodes.
        (traverse (allocate-path :nodes (list start-node))))
      ;; Return best path found-so-far, if any.
      best-path))

  (defun traverse-paths-repetitively (distance-matrix distance-limit
                                      n-iterations n-recursions start-node
                                      footprint-update-frequency
                                      neighbour-select-ratio
                                      random-effect-weight
                                      footprint-effect-weight
                                      distance-effect-weight
                                      footprint-decrement-ratio)
    "Calls TRAVERSE-PATHS over and over again N-ITERATIONS times to reach the
best possible result by exploiting footprints and random behaviours."
    (let ((iteration-offset
           (max 1 (truncate (* n-iterations footprint-update-frequency)))))
      ;; Start major turns.
      (first
       (collect-extremums
        (lambda ()
          (let* ((paths
                  ;; Start minor turns.
                  (collect-extremums
                   (lambda ()
                     (traverse-paths
                      distance-matrix
                      distance-limit
                      start-node
                      n-recursions
                      neighbour-select-ratio
                      random-effect-weight
                      footprint-effect-weight
                      distance-effect-weight))
                   :input-size iteration-offset
                   :output-size iteration-offset
                   :test #'valid-path-p
                   :key #'path-length))
                 (shortest-path (first (last paths)))
                 (longest-path (first paths)))
            ;; Perform local footprint updates.
            (labels ((update-footprint-for-edge (i j path-length)
                       ;; Update FOOTPRINT-LIMIT and FOOTPRINT-MATRIX.
                       (setq
                        footprint-limit
                        (max
                         footprint-limit
                         (setf
                          (aref footprint-matrix i j)
                          (+ 1
                             (- 1 (/ path-length path-length-limit))
                             (* (- 1 footprint-decrement-ratio)
                                (aref distance-matrix i j)
                                (/ distance-limit))))))
                       ;; Update VISITED-EDGES.
                       (setf (gethash (cons i j) visited-edges) t))
                     (update-footprint-for-path (path)
                       (loop with path-length = (path-length path)
                             for (i j) on (path-nodes path)
                             ;; Update PATH-LENGTH-LIMIT.
                             initially (setq path-length-limit
                                             (max path-length-limit path-length))
                             while (and i j)
                             do (update-footprint-for-edge i j path-length))))
              ;; Perform local footprint update.
              (loop for path in paths
                    do (update-footprint-for-path path))
              ;; Perform a second update over the shortest path found.
              (when (and shortest-path
                         (not (eq longest-path shortest-path)))
                (update-footprint-for-path shortest-path)))
            ;; Perform global footprint and FOOTPRINT-LIMIT updates.
            (loop for (i . j) being each hash-key of visited-edges
                  ;; Update FOOTPRINT-MATRIX for current edge.
                  for footprint = (setf
                                   (aref footprint-matrix i j)
                                   (max +initial-footprint+
                                        (* (aref footprint-matrix i j)
                                           (- 1
                                              (* footprint-decrement-ratio
                                                 (aref distance-matrix i j)
                                                 (/ distance-limit))))))
                  ;; If current edge reaches its minimum, remove it from
                  ;; VISITED-EDGES cache.
                  when (= footprint +initial-footprint+)
                  do (remhash (cons i j) visited-edges))
            ;; Return longest path found in minor turns.
            longest-path))
        :input-size iteration-offset
        :key #'path-length
        :test #'valid-path-p))))

  (defun find-longest-path (distance-matrix distance-limit n-iterations
                            n-recursions n-nodes start-node
                            footprint-update-frequency neighbour-select-ratio
                            random-effect-weight footprint-effect-weight
                            distance-effect-weight footprint-decrement-ratio)
    "Finds longest path in the given DISTANCE-MATRIX starting from START-NODE of
N-NODES in a heurisitc way exploting footprints (similar to pheromone matrix in
ACO algorithms) and random behaviours.

DISTANCE-LIMIT is the maximum distance an edge can have.

N-ITERATIONS is the number of iterations will be repeated while estimating to
best possible result by powering footprints periodically and exploting random
paths.

N-RECURSIONS is the maximum number of recursion depth while traversing through
neighbour nodes to construct paths.

FOOTPRINT-UPDATE-FREQUENCY will be used along with N-ITERATIONS to compute the
footprint update periods:

  (* FOOTPRINT-UPDATE-FREQUENCY N-ITERATIONS)

NEIGHBOUR-SELECT-RATIO is the ratio of number of neighbours will be selected
during path traversals.

RANDOM-EFFECT-WEIGHT, FOOTPRINT-EFFECT-WEIGHT and DISTANCE-EFFECT-WEIGHT will be
used to compute selection probability of a neighbour node:

  (+ (* RANDOM-EFFECT-WEIGHT
        (RANDOM 1.0))
     (* FOOTPRINT-EFFECT-WEIGHT
        (- 1 (/ <NEIGHBOUR-NODE-FOOTPRINT> <FOOTPRINT-LIMIT>)))
     (* DISTANCE-EFFECT-WEIGHT
        <NEIGHBOUR-NODE-DISTANCE>
        (/ DISTANCE-LIMIT)))

FOOTPRINT-DECREMENT-RATIO will be used to compute the amount of footprint will
get decremented from related edge using below equation:

  (MAX +INITIAL-FOOTPRINT+
       (* <EDGE-FOOTPRINT>
          (- 1
             (* FOOTPRINT-DECREMENT-RATIO
                <EDGE-DISTANCE>
                (/ <EDGE-DISTANCE-LIMIT>)))))

Likewise, footprints will get incremented for each found path with respect to
path's length via below equation:

  (+ 1
     (- 1 (/ <PATH-LENGTH> <PATH-LENGTH-LIMIT>))
     (* (- 1 FOOTPRINT-DECREMENT-RATIO)
        <EDGE-DISTANCE>
        (/ <EDGE-DISTANCE-LIMIT>)))

It's clear that calculated footprint amount to increment must be greater than
footprint amount to decrement.

During every footprint update phase, footprint of edges located between nodes of
found paths will get incremented _directly_ with respect to their path
lengths. And then there will be a second global footprint update on whole
footprint matrix to decrement the available footprints _indirectly_ with respect
to their edge lengths."
    ;; Don't waste time if there is no neighbour to test.
    (unless (zerop (loop for neighbour-node below n-nodes
                         sum (aref distance-matrix start-node neighbour-node)))
      ;; Initialize PATH pool.
      (if (path-store-initialized-p)
          (reset-path-store)
          (initialize-path-store n-recursions n-recursions))
      ;; Initialize NEIGHBOUR-ARRAY.
      (unless (and (arrayp neighbour-array)
                   (equal (array-dimensions neighbour-array) (list n-nodes)))
        (setq neighbour-array (make-array n-nodes
                                          :element-type 'list
                                          :initial-element nil))
        (loop for current-node below n-nodes
              do (loop for neighbour-node below n-nodes
                       initially (setf (aref neighbour-array current-node) nil)
                       unless (zerop (aref distance-matrix
                                           current-node
                                           neighbour-node))
                       do (push neighbour-node
                                (aref neighbour-array current-node)))))
      ;; Initialize FOOTPRINT-MATRIX, FOOTPRINT-LIMIT and PATH-LENGTH-LIMIT.
      (if (and (arrayp footprint-matrix)
               (equal (array-dimensions footprint-matrix)
                      (list n-nodes n-nodes)))
          (loop for i below n-nodes
                do (loop for j below n-nodes
                         do (setf (aref footprint-matrix i j)
                                  +initial-footprint+)))
          (setq footprint-matrix
                (make-array (list n-nodes n-nodes)
                            :element-type 'float
                            :initial-element +initial-footprint+)))
      (setq footprint-limit +initial-footprint+
            path-length-limit 0.0)
      ;; Initialize VISITED-EDGES.
      (if (hash-table-p visited-edges)
          (clrhash visited-edges)
          (setq visited-edges
                (make-hash-table :test #'equal
                                 ;; We need astronomic numbers in
                                 ;; here. (Performance? Just cross your
                                 ;; fingers.)
                                 :size (* n-recursions n-iterations)
                                 :rehash-size n-recursions)))
      ;; Start iterating over TRAVERSE-PATHS.
      (with-slots (nodes length)
          (traverse-paths-repetitively
           distance-matrix
           distance-limit
           n-iterations
           n-recursions
           start-node
           footprint-update-frequency
           neighbour-select-ratio
           random-effect-weight
           footprint-effect-weight
           distance-effect-weight
           footprint-decrement-ratio)
        ;; Create a fresh PATH for found nodes. Pay attention that, until this
        ;; point all functions called by FIND-LONGEST-PATH assumed that nodes
        ;; are in reverse order. But in here, we put them into their right order
        ;; while returning from FIND-LONGEST-PATH.
        (make-path :length length
                   :nodes (reverse nodes))))))

(defun find-longest-path-wrapper (distance-matrix &key
                                  (n-iterations 120) (n-recursions 8000)
                                  (footprint-update-frequency 0.05)
                                  (neighbour-select-ratio 0.7)
                                  (random-effect-weight 0.35)
                                  (footprint-effect-weight 0.4)
                                  (distance-effect-weight 0.25)
                                  (footprint-decrement-ratio 0.25)
                                  (debug t))
  "Convenient shortcut wrapper for FIND-LONGEST-PATH."
  (let* ((n-nodes (array-dimension distance-matrix 0))
         (nodes (loop for node below n-nodes collect node))
         (distance-limit
          (loop for i below n-nodes
                maximize (loop for j below n-nodes
                               maximize (aref distance-matrix i j)))))
    (first
     (collect-extremums
      (lambda ()
        (find-longest-path
         distance-matrix
         distance-limit
         n-iterations
         n-recursions
         n-nodes
         (pop nodes)
         footprint-update-frequency
         neighbour-select-ratio
         random-effect-weight
         footprint-effect-weight
         distance-effect-weight
         footprint-decrement-ratio))
      :input-size n-nodes
      :key #'path-length
      :test (if debug
                (lambda (path)
                  (when (valid-path-p path)
                    (format t
                            "~5d:~8f~%"
                            (first (path-nodes path))
                            (path-length path))
                    t))
                #'valid-path-p)))))


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
                                      :element-type 'float
                                      :initial-element 0.0)))
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
    ;; Find longest path in the set of graphs defined by DISTANCE-MATRIX.
    (let ((path (funcall find-longest-path-fn distance-matrix)))
      (if (valid-path-p path)
          (format t
                  "Length: ~a~%Nodes: ~a~%String Representation: ~a~%"
                  (path-length path)
                  (path-nodes path)
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
                           (path-nodes path))))
          (warn "No valid paths returned!")))))