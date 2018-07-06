#lang racket

(provide my-ip-qr-img)

(require hostname
         simple-qr
         pict
         net/base64
         file/convertible)

(define slash
  (if (eq? (system-type 'os) 'windows)
      "\\"
      "/"))

;; pict->data-uri : Pict -> String
(define (pict->data-uri pict)
  (format "data:image/png;base64,~a"
          (base64-encode (convert pict 'png-bytes))))

(define (my-ip)
    (if (eq? (system-type 'os) 'windows)
        (begin
          (cond [(eq? (system-type 'os) 'windows) (maybe-create-tmp)])
          (best-ipv4-ip-address))
        (first (get-ipv4-addrs))))

(define (my-ip-qr-img (post-fix ""))
  (define my-ip-qr
    (qr-write	 	(format "http://~a:8000~a" (my-ip) post-fix)	 	 	 
                        (if (eq? (system-type 'os) 'windows)
                            (string-append (path->string (find-system-path 'home-dir)) slash "tmp" slash "share.png")
                            "/tmp/share.png")))
  `(div ((style "position:absolute; top:0; left:0; z-index: 1"))
        (img ((src ,(pict->data-uri (bitmap (if (eq? (system-type 'os) 'windows)
                                             (string-append (path->string (find-system-path 'home-dir)) slash "tmp" slash "share.png")
                                             "/tmp/share.png"))))))))

(define (interface-ip-addresses)
  (map
   (lambda (pieces)
     (car (car (filter-map (lambda (s) (regexp-match #px"\\d+.\\d+.\\d+.\\d+" s)) pieces))))
   (filter (lambda (r) (and (pair? r) (string-ci=? (car r) "IPv4")))
	   (map string-split
		(string-split (with-output-to-string (lambda () (system "ipconfig"))) "\n")))))

(define (private-ip-address? x)
  (match (map string->number (string-split x "."))
    [(list 10 _ _ _) #t]
    [(list 172 n _ _) (and (>= n 16) (< n 32))]
    [(list 192 168 _ _) #t]
    [_ #f]))

(define (best-ipv4-ip-address)
  (define addresses (interface-ip-addresses))
  (define private-addresses (filter private-ip-address? addresses))

  (cond
   [(pair? private-addresses) (car private-addresses)]
   [else "127.0.0.1"]))

(define (maybe-create-tmp)
          (define tmp (string-append (path->string (find-system-path 'home-dir)) slash "tmp" slash))
          (or (directory-exists? tmp)
              (make-directory tmp)))

