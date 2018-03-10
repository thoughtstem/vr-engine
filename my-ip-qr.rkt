#lang racket

(provide my-ip-qr-img)

(require hostname
         simple-qr
         pict
         net/base64
         file/convertible)

;; pict->data-uri : Pict -> String
  (define (pict->data-uri pict)
    (format "data:image/png;base64,~a"
            (base64-encode (convert pict 'png-bytes))))

(define my-ip
    (first (get-ipv4-addrs)))

(define (my-ip-qr-img (post-fix ""))

  (define my-ip-qr
    (qr-write	 	(format "http://~a:8000~a" my-ip post-fix)	 	 	 
                        "/tmp/share.png"))
  `(div ((style "position:absolute; top:0; left:0; z-index: 1"))
        (img ((src ,(pict->data-uri (bitmap "/tmp/share.png")))))))



