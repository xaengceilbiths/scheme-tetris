#lang racket

(require rackunit
         2htdp/image
         (rename-in 2htdp/universe
                    [left left-arrow]
                    [right right-arrow]
                    [up up-arrow]
                    [down down-arrow]))

(define-values (xxx yyy xxyy *tetris-num*)
  (values 10 20 200 7))
(define-values (*magnification* *tile-side* *grid-spacing*)
  (values 2 20 2))
(define-values (*gridx* *gridy*)
  (values (+ (* xxx *tile-side*) (* (add1 xxx) *grid-spacing*))
          (+ (* yyy *tile-side*) (* (add1 yyy) *grid-spacing*))))
(define *background-color* (color #xcd #x95 #x75))
(define tetris-graph
  '(("...."
     "babb"
     "...."
     "....")
    ("ab"
     "bb")
    ("..."
     "ba*"
     "*bb")
    ("..."
     "*ab"
     "bb*")
    ("b**"
     "bab"
     "...")
    ("**b"
     "bab"
     "...")
    ("*b*"
     "bab"
     "...")))
;;currently use center of graph to rotate 
(define tetris-rotate-scheme
  '((l r) (id) (r l) (l r) (r r r r) (l l l l) (r r r r)))
(define (grid-color n)
  (case n
    [(-1) (color #x00 #x22 #x44)]
    [(0) (color #x55 #x44 #x66)]
    [else (color #x00 #xbb #x00)]))
(define-struct dot (x y) #:mutable #:transparent)
(define-struct base-TETRIS (vov size innernw-dot) #:mutable #:transparent)
(define-struct TETRIS-group (varient size nom) #:mutable #:transparent)
(define-struct active-TETRIS (group tid boxnw actv) #:mutable #:transparent)
(define (create-tetris-group base-graph rotate-scheme)
  (define (fold kons knil l)
    (if (null? l)
        knil
        (fold kons (kons (car l) knil) (cdr l))))
  (define (findnwdot vov)
    (let ([n (vector-length vov)])
      (let ([x (for*/or ([i (in-range n)]
                         [j (in-range n)])
                 (if (> (vector-ref (vector-ref vov j) i) 0)
                     i
                     #f))]
            [y (for*/or ([j (in-range n)]
                         [i (in-range n)])
                 (if (> (vector-ref (vector-ref vov j) i) 0)
                     j
                     #f))])
        (make-dot x y))))
  (define (rotate lr vovl)
    (let* ([l (eq? 'l lr)]
           [vov (car vovl)]
           [size (vector-length vov)]
           [nvov (build-vector size (lambda _ (make-vector size)))])
      (if l
          (for* ([i (in-range size)]
                 [j (in-range size)])
            (vector-set! (vector-ref nvov (- (- size i) 1)) j
                         (vector-ref (vector-ref vov j) i)))
          (for* ([i (in-range size)]
                 [j (in-range size)])
            (vector-set! (vector-ref nvov i) (- (- size j) 1)
                         (vector-ref (vector-ref vov j) i))))
      (cons nvov vovl)))
  (let* ([size (length base-graph)]
         [str2v (lambda (str)
                  (vector-map (lambda (c) (if (or (eq? c #\a) (eq? c #\b)) 1 0))
                              (list->vector (string->list str))))]
         [vovs (list->vector (reverse (fold rotate (list (list->vector (map str2v base-graph))) (take rotate-scheme (- (length rotate-scheme) 1)))))]
         [nws (vector-map findnwdot vovs)]
         [makebase (lambda (vov nw) (make-base-TETRIS vov size nw))]
         [bases (vector-map makebase vovs nws)])
    (make-TETRIS-group bases size (length rotate-scheme))))
(define tetris-groups (list->vector (map create-tetris-group tetris-graph tetris-rotate-scheme)))
(define-struct world (state graph active score winning-total frames start-time) #:mutable #:transparent)
(define (place-tile/ij tile i j grid-image)
  (define (pos k)
    (+ (* (add1 k) *grid-spacing*)
       (* k *tile-side*)))
  (underlay/xy grid-image (pos i) (pos j) tile))
(define (new-tetris w)
  (match-define (world state graph act score wt frames start-time) w)
  ;; allow force new tetris for testing
  (when act (vector-map (lambda (i) (vector-set! state i 0)) (active-TETRIS-actv act)))
  (let* ([r (random *tetris-num*)]
         [g (vector-ref tetris-groups r)]
         [base (vector-ref (TETRIS-group-varient g) 0)])
    (match-define (base-TETRIS vov size nwd) base)
    (match-define (dot x y) nwd)
    (define actl '())
    (for* ([j (in-range (- size y))]
           [i (in-range (- size x))])
      (when (= (vector-ref (vector-ref vov (+ y j)) (+ x i)) 1)
        (let ([ii (+ i 3)])
          (vector-set! state (+ (* j xxx) ii) 1)
          (set! actl (cons (+ (* j xxx) ii) actl))))
      (let ([active-tetris (make-active-TETRIS r 0 (- (- 3 x) (* xxx y)) (list->vector actl))])
        (set-world-active! w active-tetris))))
  (set-world-state! w state)
  w)
(define (transform-tetris w)
  (match-define (world state graph act score wt frames start-time) w)
  (match-define (active-TETRIS group tid boxnw actv)
    (if act act (make-active-TETRIS #f #f #f #f)))
  (define (calc-actv boxnw baset)
    (match-define (base-TETRIS vov size innernwdot) baset)
    (call/cc
     (lambda (k)
       (define (assert x) (unless x (k #f)))
       (list->vector
        (for*/fold ([ans '()])
                   ([i size]
                    [j size])
          (if (> (vector-ref (vector-ref vov j) i) 0)
              (let* ([_ (assert (< (+ (modulo boxnw xxx) i) xxx))]
                     [t (+ boxnw i (* xxx j))]
                     [_ (assert (>= t 0))]
                     [_ (assert (< t xxyy))])
                (cons t ans))
              ans))))))
  (when actv
    (match-define (TETRIS-group varient size nom) (vector-ref tetris-groups group))
    (let* ([old-base (vector-ref varient tid)]
           [new-base (vector-ref varient (modulo (+ tid 1) nom))]
           [new-actv (calc-actv boxnw new-base)]
           [able? (and new-actv
                       (andmap (lambda (i) (>= (vector-ref state i) 0)) (vector->list new-actv)))])
      (when able?
        (set-active-TETRIS-tid! act (modulo (+ tid 1) nom))
        (set-active-TETRIS-actv! act new-actv)
        (vector-map (lambda (i) (vector-set! state i 0)) actv)
        (vector-map (lambda (i) (vector-set! state i 1)) new-actv))))
  w)
(define (left-tetris w)
  (match-define (world state graph act score wt frames start-time) w)
  (match-define (active-TETRIS group tid boxnw actv)
    (if act act (make-active-TETRIS #f #f #f #f)))
  (when actv
    (let* ([new-actv (vector-map (lambda (i) (- i 1)) actv)]
           [new-boxnw (- boxnw 1)]
           [able?
            (and (andmap (lambda (i) (> (modulo i xxx) 0)) (vector->list actv))
                 (andmap (lambda (i) (>= (vector-ref state i) 0)) (vector->list new-actv)))])
      (when able?
        (set-active-TETRIS-actv! act new-actv)
        (set-active-TETRIS-boxnw! act new-boxnw)
        (vector-map (lambda (i) (vector-set! state i 0)) actv)
        (vector-map (lambda (i) (vector-set! state i 1)) new-actv))))
  w)
(define (right-tetris w)
  (match-define (world state graph act score wt frames start-time) w)
  (match-define (active-TETRIS group tid boxnw actv)
    (if act act (make-active-TETRIS #f #f #f #f)))
  (when actv
    (let* ([new-actv (vector-map (lambda (i) (+ i 1)) actv)]
           [new-boxnw (+ boxnw 1)]
           [able?
            (and (andmap (lambda (i) (> (modulo i xxx) 0)) (vector->list new-actv))
                 (andmap (lambda (i) (>= (vector-ref state i) 0)) (vector->list new-actv)))])
      (when able?
        (set-active-TETRIS-actv! act new-actv)
        (set-active-TETRIS-boxnw! act new-boxnw)
        (vector-map (lambda (i) (vector-set! state i 0)) actv)
        (vector-map (lambda (i) (vector-set! state i 1)) new-actv))))
  w)
(define (down-tetris w)
  (match-define (world state graph act score wt frames start-time) w)
  (match-define (active-TETRIS group tid boxnw actv)
    (if act act (make-active-TETRIS #f #f #f #f)))
  (when actv
    (let* ([new-actv (vector-map (lambda (i) (+ i xxx)) actv)]
           [new-boxnw (+ boxnw xxx)]
           [able? (andmap (lambda (i)
                            (and (< i xxyy)
                                 (>= (vector-ref state i) 0)))
                          (vector->list new-actv))])
      (cond
        [able?
         (set-active-TETRIS-actv! act new-actv)
         (set-active-TETRIS-boxnw! act new-boxnw)
         (vector-map (lambda (i) (vector-set! state i 0)) actv)
         (vector-map (lambda (i) (vector-set! state i 1)) new-actv)]
        [else
         (set-world-active! w #f)
         (vector-map (lambda (i) (vector-set! state i -1)) actv)
         (set-world-graph! w
                           (for*/fold ([im graph])
                                      ([x actv])
                             (let ([i (remainder x xxx)]
                                   [j (quotient x xxx)])
                               (place-tile/ij (square *tile-side* "solid" (grid-color -1))
                                              i j
                                              im))))
         (let* ([affected (remove-duplicates (vector->list (vector-map (lambda (x) (quotient x xxx)) actv)))]
                ;;Currently all affected lines >=0
                [f (lambda (j) (for*/and ([i (in-range (* j xxx) (* (+ j 1) xxx))]) (= (vector-ref state i) -1)))]
                [toCut (filter f affected)]
                [score+ (* 5 (expt 2 (length toCut)))]
                [new-state (make-vector xxyy 0)])
           (unless (null? toCut)
             (let loop ([i 0]
                        [j (* (length toCut) xxx)])
               (when (< i xxyy)
                 (cond
                   [(member (quotient i xxx) toCut) (loop (+ i 1) j)]
                   [else
                    (vector-set! new-state j (vector-ref state i))
                    (loop (+ i 1) (+ j 1))])))
             (set-world-state! w new-state)
             (set-world-score! w (+ score score+))
             (let* ([g0 (rectangle *gridx* *gridy* "solid" *background-color*)]
                    [g1 (for*/fold ([im g0])
                                   ([i (in-range xxx)]
                                    [j (in-range yyy)])
                          (place-tile/ij (square *tile-side* "solid"
                                                 (grid-color (vector-ref new-state (+ i (* j xxx)))))
                                         i j
                                         im))])
               (set! graph g1)
               (set-world-graph! w graph))))])))
  w)

(define (kara-state) (make-vector xxyy 0))
(define (test-state) (list->vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 -1 -1 -1 0 0 -1 0 0 -1 -1 -1 -1 -1 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 0)))
(define (konran-state)
  ;TODO: avoid darking a single line
  (define (f i)
    (if (< i (* xxx (- yyy 8)))
        0
        (- (random 2) 1)))
  (build-vector xxyy f))
(define (handle-key w a-key)
  (define (key->op a-key)
    (cond
      [(key=? a-key "left")  left-tetris]
      [(key=? a-key "right") right-tetris]
      [(key=? a-key "up") transform-tetris]
      ;[(key=? a-key "down")  (list my-down moves-grid-down)]
      [(key=? a-key "n") new-tetris]
      [(key=? a-key "d") down-tetris]
      [(key=? a-key "c") (lambda (w) (make-world (kara-state) #f #f
                                                 ;(konran-state) 
                                                 0 #f null (current-seconds)))]
      [(key=? a-key "k") (lambda (w) (make-world (konran-state) #f #f
                                                 0 #f null (current-seconds)))]
      [(key=? a-key "t") (lambda (w) (make-world (test-state) #f #f
                                                 0 #f null (current-seconds)))]
      [else #f]))
  (match-let ([op (key->op a-key)]
              [(world st graph act score wt frames start-time) w])
    (cond [op (op w)]
          [else w])))

(define (show-world w)
  (match-define (world state graph act score wt frames start-time) w)
  (match-define (active-TETRIS group tid boxnw actv)
    (if act act (make-active-TETRIS #f #f #f #f)))
  (define (number->time-string s)
    (define hrs (quotient s 3600))
    (define mins (quotient (remainder s 3600) 60))
    (define secs (remainder s 60))
    (define (xx n)
      (cond [(<= n 0) "00"]
            [(<= n 9) (format "0~a" n)]
            [else (remainder n 60)]))
    (if (>= s 3600)
        (format "~a:~a:~a" hrs (xx mins) (xx secs))
        (format "~a:~a" mins (xx secs))))
  (define (time-elapsed start)
    (- (current-seconds) start))
  (unless graph
    (let* ([g0 (rectangle *gridx* *gridy* "solid" *background-color*)]
           [g1 (for*/fold ([im g0])
                          ([i (in-range xxx)]
                           [j (in-range yyy)])
                 (place-tile/ij (square *tile-side* "solid"
                                        (grid-color (vector-ref state (+ i (* j xxx)))))
                                i j
                                im))])
      (set! graph g1)
      (set-world-graph! w graph)))
  (let* ([graph-with-act (if actv
                             (for*/fold ([im graph])
                                        ([x actv])
                               (let ([i (remainder x xxx)]
                                     [j (quotient x xxx)])
                                 (place-tile/ij (square *tile-side* "solid" (grid-color 1))
                                                i j
                                                im)))
                             graph)]
         [score-text (text (format "Score: ~a" score) 16 'dimgray)]
         [seconds (time-elapsed start-time)]
         [time-text (text (format "Time: ~a" 
                                  (number->time-string seconds)) 
                          16
                          (cond ;[(or (> seconds *amber-alert*) (not *time-limit*)) 'gray]
                            ;[(> seconds *red-alert*) 'orange] 
                            [else 'red]))])        
    (scale *magnification*
           (above
            graph-with-act
            (rectangle 0 5 'solid 'white)
            (beside
             score-text
             (rectangle (- (image-width graph)
                           (image-width score-text)
                           (image-width time-text)) 0 'solid 'white)
             time-text)))))
(define (game-over? w)
  (match-define (world state graph act score wt frames start-time) w)
  #f)
(define (my-on-tick w)
  (if (world-active w)
      (down-tetris w)
      (new-tetris w)))
(define (start)
  (big-bang (make-world (kara-state) 
                        ;(konran-state)
                        #f #f
                        0 #f null (current-seconds))
            (to-draw show-world)
            (on-key handle-key)
            (on-tick my-on-tick 0.2)
            (stop-when game-over? show-world)
            (name "tetris - Racket edition")))
(start)
