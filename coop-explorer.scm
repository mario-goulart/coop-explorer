(module coop-explorer

(coop-db
 coop-eggs
 egg-author
 egg-category
 egg-dependencies
 egg-files
 egg-has-tests?
 egg-latest-version
 egg-license
 egg-maintainer
 egg-metadata
 egg-synopsis
 egg-test-dependencies
 egg-versions
 egg-what-depends
 egg-what-test-depends
 populate-coop-db!)

(import scheme)
(import (chicken base)
        (chicken file)
        (chicken format)
        (chicken irregex)
        (chicken pathname)
        (chicken sort)
        (chicken string))

;; From setup-api (chicken-4.13.0)
(define (version>=? v1 v2)
  (define (version->list v)
    (map (lambda (x) (or (string->number x) x))
         (irregex-split "[-\\._]" (->string v))))
  (let loop ((p1 (version->list v1))
             (p2 (version->list v2)))
    (cond ((null? p1) (null? p2))
          ((null? p2))
          ((number? (car p1))
           (and (number? (car p2))
                (or (> (car p1) (car p2))
                    (and (= (car p1) (car p2))
                         (loop (cdr p1) (cdr p2))))))
          ((number? (car p2)))
          ((string>? (car p1) (car p2)))
          (else
           (and (string=? (car p1) (car p2))
                (loop (cdr p1) (cdr p2)))))))

(define coop-db
  ;; ((egg version metadata files))
  (make-parameter '()))

(define db-item-egg car)
(define db-item-version cadr)
(define db-item-metadata caddr)
(define db-item-files cadddr)

(define (populate-coop-db! coop-dir)
  (for-each
   (lambda (egg)
     (let ((egg-dir (make-pathname coop-dir egg)))
       (for-each
        (lambda (version)
          (let* ((egg-dir (make-pathname (list coop-dir egg) version))
                 (egg-dir-len (string-length egg-dir))
                 (egg-files (glob (make-pathname egg-dir "*.egg"))))
            (unless (null? egg-files)
              (when (not (null? (cdr egg-files)))
                (error 'coop-populate-db!
                       (sprintf "Found more than one egg file for ~a version ~a"
                                egg version)))
              (coop-db
               (cons (list (string->symbol egg)
                           version
                           (with-input-from-file (car egg-files) read)
                           (map (lambda (file)
                                  ;; strip egg-dir
                                  (substring file (+ 1 egg-dir-len)))
                                (find-files egg-dir)))
                     (coop-db))))))
        (directory egg-dir))))
   (directory coop-dir))
  (coop-db))

(define (coop-eggs)
  (map car (coop-db)))

(define egg-versions
  (let ((cache '()))
    (lambda (egg)
      (let ((versions (alist-ref egg cache)))
        (or versions
            (let ((versions
                   (let loop ((db (coop-db)))
                     (if (null? db)
                         '()
                         (let ((db-item (car db)))
                           (if (eq? egg (db-item-egg db-item))
                               (cons (db-item-version db-item) (loop (cdr db)))
                               (loop (cdr db))))))))
              (set! cache (cons (cons egg versions) cache))
              versions))))))

(define egg-latest-version
  (let ((cache '()))
    (lambda (egg)
      (let ((latest (alist-ref egg cache)))
        (or latest
            (let* ((versions (egg-versions egg))
                   (latest (car (sort versions version>=?))))
              (set! cache (cons (cons egg latest) cache))
              latest))))))

(define (egg-data-by-version egg version caller)
  (let ((version (or version (egg-latest-version egg))))
    (let loop ((db (coop-db)))
      (if (null? db)
          (error caller egg version)
          (let ((db-item (car db)))
            (if (and (eq? (db-item-egg db-item) egg)
                     (equal? (db-item-version db-item) version))
                db-item
                (loop (cdr db))))))))

(define (egg-metadata egg #!optional version)
  (db-item-metadata (egg-data-by-version egg version 'egg-metadata)))

(define (egg-files egg #!optional version)
  (db-item-files (egg-data-by-version egg version 'egg-files)))

(define (egg-has-tests? egg #!optional version)
  (and (member (make-pathname "tests" "run.scm")
               (egg-files egg version))
       #t))

(define (metadata-dependencies metadata)
  (append (alist-ref 'dependencies metadata eq? '())
          (alist-ref 'build-dependencies metadata eq? '())))

(define (egg-dependencies egg #!optional version)
  (metadata-dependencies (egg-metadata egg version)))

(define (egg-metadata-field egg version field default)
  (let ((metadata (egg-metadata egg version)))
    (alist-ref field metadata eq? default)))

(define (egg-metadata-field/single egg version field)
  (and-let* ((result (egg-metadata-field egg version field #f)))
    (car result)))

(define (egg-test-dependencies egg #!optional version)
  (egg-metadata-field egg version 'test-dependencies '()))

(define (egg-author egg #!optional version)
  (egg-metadata-field/single egg version 'author))

(define (egg-maintainer egg #!optional version)
  (egg-metadata-field/single egg version 'maintainer))

(define (egg-category egg #!optional version)
  (egg-metadata-field/single egg version 'category))

(define (egg-license egg #!optional version)
  (egg-metadata-field/single egg version 'license))

(define (egg-synopsis egg #!optional version)
  (egg-metadata-field/single egg version 'synopsis))

(define (dependency-egg dep)
  (if (pair? dep)
      (car dep)
      dep))

(define (%egg-what-depends egg deps-getter)
  (let loop ((db (coop-db))
             (dependees '()))
    (if (null? db)
        dependees
        (let* ((db-item (car db))
               (deps (map dependency-egg
                          (deps-getter (db-item-metadata db-item)))))
          (loop (cdr db)
                (if (and (memq egg deps)
                         (not (memq (db-item-egg db-item) dependees)))
                    (cons (db-item-egg db-item) dependees)
                    dependees))))))

(define (egg-what-depends egg)
  (%egg-what-depends egg metadata-dependencies))

(define (egg-what-test-depends egg)
  (%egg-what-depends egg
                     (lambda (metadata)
                       (alist-ref 'test-dependencies metadata eq? '()))))

) ;; end module
