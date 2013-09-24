
(library (cdb)
  (export open-input-cdb cdb-get close-input-cdb
          open-output-cdb cdb-set close-output-cdb)
  (import (rnrs))

  (define (bytevector-u8-fold kons knil e)
    (let ((len (bytevector-length e)))
      (do ((i 0 (+ i 1))
           (knil knil (kons (bytevector-u8-ref e i) knil)))
          ((= i len) knil))))

  (define (hashadd c h)
    (bitwise-xor c (* 33 h)))

  (define hashstart 5381)

  (define (hash buf)
    (bitwise-and #xFFFFFFFF (bytevector-u8-fold hashadd hashstart buf)))

  (define-record-type (<output-cdb> make-output-cdb output-cdb?)
    (fields
     (immutable port output-cdb-port)
     (immutable table output-cdb-table)
     (mutable pointer output-cdb-pointer output-cdb-pointer-set!)))

  (define output-file-option (file-options no-fail))

  (define output-file-mode (buffer-mode block))

  (define (open-output-cdb filename)
    (let ((port
           (open-file-output-port filename
                                  output-file-option
                                  output-file-mode)))
      (set-port-position! port 2048)
      (make-output-cdb port (make-vector 256 '()) 2048)))

  (define (put-u32 port value)
    (let ((v (make-bytevector 4)))
      (bytevector-u32-set! v 0 value (endianness little))
      (put-bytevector port v)))

  (define (table-push! table h pointer)
    (vector-set! table (mod h 256)
                 (cons (cons h pointer)
                       (vector-ref table (mod h 256)))))

  (define (cdb-set cdb key value)
    (let ((key-len (bytevector-length key))
          (value-len (bytevector-length value))
          (port (output-cdb-port cdb))
          (table (output-cdb-table cdb))
          (pointer (output-cdb-pointer cdb)))
      (put-u32 port key-len)
      (put-u32 port value-len)
      (put-bytevector port key)
      (put-bytevector port value)
      (table-push! table (hash key) pointer)
      (output-cdb-pointer-set! cdb (+ pointer 8 key-len value-len))))

  (define (get-cell-element cell i j)
    (bytevector-u32-ref cell (+ (* 8 i) (* 4 j)) (endianness little)))

  (define (set-cell-element! cell i k v)
    (bytevector-u32-set! cell (* 8 i) k (endianness little))
    (bytevector-u32-set! cell (+ 4 (* 8 i)) v (endianness little)))

  (define (close-output-cdb cdb)
    (let ((port (output-cdb-port cdb))
          (table (output-cdb-table cdb))
          (pointer (output-cdb-pointer cdb)))
      (vector-for-each
       (lambda(b1)
         (if (not (null? b1))
              (let* ((ncells (* 2 (length b1)))
                     (cell (make-bytevector (* ncells 8) 0)))
                (for-each
                 (lambda(x)
                   (let* ((h (car x))
                          (p (cdr x))
                          (i (mod (bitwise-arithmetic-shift-right h 8) ncells)))
                     (do ((i i (mod (+ i 1) ncells)))
                         ((zero? (get-cell-element cell i 1))
                          (set-cell-element! cell i h p)))))
                 b1)
                (put-bytevector port cell))))
       table)
      (set-port-position! port 0)
      (vector-for-each
       (lambda(b1)
         (put-u32 port pointer)
         (put-u32 port (* 2 (length b1)))
         (set! pointer (+ pointer (* 16 (length b1)))))
       table)
      (close-port port)))

  (define-record-type (<input-cdb> make-input-cdb input-cdb?)
    (fields
     (immutable port input-cdb-port)
     (immutable table input-cdb-table)))

  (define input-file-option (file-options no-fail))

  (define input-file-mode (buffer-mode block))

  (define (open-input-cdb filename)
    (let ((port
           (open-file-input-port filename input-file-option input-file-mode)))
      (make-input-cdb port (get-bytevector-n port 2048))))

  (define (close-input-cdb cdb)
    (let ((port (input-cdb-port cdb)))
      (close-port port)))

  (define (lookup-main-table table hash-value)
    (let ((pos (* 2 (mod hash-value 256))))
      (let ((p (bytevector-u32-ref table (* 4 pos) (endianness little)))
            (e (bytevector-u32-ref table (* 4 (+ 1 pos)) (endianness little))))
        (values p e))))

  (define (get-u32 port)
    (let ((v (get-bytevector-n port 4)))
      (bytevector-u32-ref v 0 (endianness little))))

  (define (get-u32/position port position)
    (set-port-position! port position)
    (get-u32 port))

  (define (cdb-get cdb key)
    (let ((h (hash key))
          (keylen (bytevector-length key))
          (t (input-cdb-table cdb))
          (port (input-cdb-port cdb)))
      (call-with-values (lambda()(lookup-main-table t h))
        (lambda(p e)
          (if (zero? e)
              #f
              (let ((start (bitwise-arithmetic-shift-right h 8)))
                (let loop ((i 0))
                  (if (= i e) #f
                      (let ((h1 (get-u32/position port (+ (* (mod (+ start i) e) 8) p))))
                        (cond ((zero? h1) #f)
                              ((= h1 h)
                               (let* ((r (get-u32 port))
                                      (kl (get-u32/position port r))
                                      (dl (get-u32 port)))
                                 (if (and (= kl keylen)
                                          (bytevector=?
                                           (get-bytevector-n port kl)
                                           key))
                                     (get-bytevector-n port dl)
                                     (loop (+ i 1)))))
                              (else (loop (+ i 1)))))))))))))

  )
