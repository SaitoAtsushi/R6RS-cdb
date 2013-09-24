
(import (rnrs)
        (cdb))

(let ((cdb (open-output-cdb "testdata.cdb")))
  (cdb-set cdb (string->utf8 "foo") #vu8(49))
  (cdb-set cdb (string->utf8 "bar") #vu8(50))
  (cdb-set cdb (string->utf8 "baz") #vu8(51))
  (close-output-cdb cdb)
  )

(let ((cdb (open-input-cdb "testdata.cdb")))
  (display
   (if (bytevector=? (cdb-get cdb (string->utf8 "foo")) #vu8(49)) 'OK 'NG))
  (newline)

  (display
   (if (bytevector=? (cdb-get cdb (string->utf8 "bar")) #vu8(50)) 'OK 'NG))
  (newline)

  (display
   (if (bytevector=? (cdb-get cdb (string->utf8 "baz")) #vu8(51)) 'OK 'NG))
  (newline)

  (display
   (if (eqv? (cdb-get cdb (string->utf8 "non-exist")) #f) 'OK 'NG))
  (newline)

  (close-input-cdb cdb)
  )
