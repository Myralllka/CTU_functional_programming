(define (filter pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
    (else (filter pred (cdr lst)))
    )
  )

(define (qsort lst cmp)
  (cond
    ((null? lst) '())
    (else (let*
              ((pivot (car lst))
               (smaller (lambda (x) (cmp x pivot)))
               (greater (lambda (x) (not (cmp x pivot)))))
            (append
             (qsort (filter smaller (cdr lst)) cmp)
             (list pivot)
             (qsort (filter greater (cdr lst)) cmp))
            )
          )
    )
  )

(define (get-index node)
  (cadr node)
  )
(define (get-neighbors node)
  (caddr node)
  )
(define (get-label node)
  (car node)
  )
(define (get-node n g)
  (cond
    ((null? g) #f)
    ((equal? n (get-index (car g))) (car g))
    (else (get-node n (cdr g)))
    )
  )

(define (leaf? n g)
  (let ((node (get-node n g)))
    (null? (cdr (get-neighbors node))))
  )

(define (trimm s)
  (substring s 1 (- (string-length s) 1)))(define (join lst)
  (if (null? lst)
      ""
      (string-append (car lst) (join (cdr lst))))
  )

(define (add01 s)
  (string-append "0" s "1")
  )

(define (new-label n g)
  (let*
      ((node (get-node n g))
       (trimmed-label (trimm (get-label node)))
       (neighbors (get-neighbors node))
       (leafs (filter (lambda (x) (leaf? x g)) neighbors))
       (labels (cons trimmed-label (map (lambda (i) (get-label (get-node i g))) leafs))))
    (add01 (join (qsort labels string<?)))
    )
  )

(define (vertex-certificate vertex-id tree)
  (new-label vertex-id (reorder-nodes tree)))

(define (update-neighbors lst g)
  (let*
      ((leafs (filter (lambda (x) (leaf? x g)) lst)))
    (filter (lambda (i) (not (member i leafs))) lst)
    )
  )

(define (iter g)
  (let*
      ((indexes (map get-index g))
       (parents (filter (lambda (x) (not (leaf? x g))) indexes))
       (newlabels (map (lambda (i) (new-label i g)) parents))
       (neighborss (map (lambda (i) (get-neighbors (get-node i g))) parents))
       (newneighborss (map (lambda (ls) (update-neighbors ls g)) neighborss)))
    (map list newlabels parents newneighborss)
    )
  )

(define (cert g)
  (cond
    ((null? g) "")
    ((null? (cdr g)) (get-label (car g)))
    ((null? (cddr g)) (apply string-append (qsort (map get-label g) string<?)))
    (else (cert (iter g)))
    )
  )

(define (reorder-nodes tree)
  (cond ((null? tree) '())
        (#t (cons (list (cadar tree) (caar tree) (caddar tree)) (reorder-nodes (cdr tree))))
        ))

(define (certificate g)
  (cert (reorder-nodes g)))

(define (isomorphic? treeA treeB)
  (string=? (certificate treeA) (certificate treeB)))

