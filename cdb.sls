
(library (cdb)
  (export open-input-cdb cdb-get close-input-cdb)
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
