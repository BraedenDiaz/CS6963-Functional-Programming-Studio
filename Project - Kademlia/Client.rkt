#lang racket

(require racket/udp)
(require hostname)
(require json)
; Represents a Kademlia node with a given guid (256-bit), ip address (string), and port number (Number)
(struct KadNode (guid ip port))

; Define sockets, one to send and one to receive on
(define main-socket (udp-open-socket))
(define receive-socket (udp-open-socket))

; Bind the sockets to the machines IP and a port
; The port for the receive-socket is the only one that
; needs to be known in order for other nodes to know where
; to send messages to.
(udp-bind! main-socket #f 0)
(udp-bind! receive-socket #f 10002)

(define (process-message message-jsexpr)
  (let* ([type (string->symbol (hash-ref message-jsexpr 'type))]
         [sender-node-info-list (hash-ref message-jsexpr 'kadnode)]
         [buffer (hash-ref message-jsexpr 'buffer)]
         [node-id-str (number->string (first sender-node-info-list))])
    (case type
      [(FIND_VALUE_REPLY) (display "Got Content:\n")
                          (display (string-append buffer "\n"))]
      [(STORE_REPLY) (display "Successfully stored content:\n")
                     (display (string-append (string-append "Node: " node-id-str) "\n"))
                     (display (string-append (string-append "Key: " buffer) "\n"))]
      [(RT_REPLY) (display (map (lambda (bucket)
                                  (if (empty? bucket)
                                      bucket
                                      (first bucket)))
                                buffer))]
      [(DHT_REPLY) (display (string-append (string-append "DHT for Node " node-id-str) ":\n"))
                   (for-each (lambda (kv)
                               (display kv)
                               (display "\n"))
                             buffer)
                   (display "\n")])))

; -------------------------------------- Serialization ----------------------------------------- ;

(define (serialize type kad-node buffer)
  (jsexpr->bytes (hash 'type type
                       'kadnode (list (KadNode-guid kad-node) (KadNode-ip kad-node) (KadNode-port kad-node))
                       'buffer buffer)))

(define (deserialize message)
  (bytes->jsexpr message))

; -------------------------------------- Networking -------------------------------------------- ;

; Send a message to the selected KadNode
(define (send-message-to kad-node json-bytes)
  (udp-send-to main-socket
               (KadNode-ip kad-node)
               (KadNode-port kad-node)
               json-bytes))

; Receive Buffer (RPC messages should me no larger than 1024 bytes)
(define receive-str (make-bytes 1024))

; Listen on the receive-socket for messages

(define (listen-for-messages)
  (sync (handle-evt (udp-receive!-evt receive-socket receive-str)
                    (lambda (_)
                      (process-message (deserialize receive-str))))))


; -------------------------------------- Client Commands  ----------------------------------------- ;

(define (listen-thread) (thread listen-for-messages))

(define (ClientKadNode)
  (let-values ([(ip port remoteIP remotePort) (udp-addresses receive-socket #t)])
    (let ([ip-to-use (if (empty? (get-ipv4-addrs))
                         "127.0.0.1" ; Windows
                         (first (get-ipv4-addrs)))])
      (KadNode -1 ip-to-use port))))

(define (put-content ip port content)
  (udp-send-to main-socket ip port (serialize "CLIENT_STORE" (ClientKadNode) content)))

(define (get-content ip port content-key)
  (udp-send-to main-socket ip port (serialize "FIND_VALUE" (ClientKadNode) content-key)))

(define (get-routing-table ip port)
  (udp-send-to main-socket ip port (serialize "CLIENT_GET_RT" (ClientKadNode) "")))

(define (get-dht ip port)
  (udp-send-to main-socket ip port (serialize "CLIENT_GET_DHT" (ClientKadNode) "")))

(define (shutdown-node ip port)
  (udp-send-to main-socket ip port (serialize "SHUTDOWN" (ClientKadNode) ""))
  (display "[*] Shutdown command sent."))

; -------------------------------------- Startup -------------------------------------------- ;

(define (put-content-option)
  (let ([disp1 (display "Enter Node IP: ")]
        [ip (read-line)]
        [disp2 (display "Enter Node Port: ")]
        [port (string->number (read-line))]
        [disp3 (display "Enter Text String Content: ")]
        [content (read-line)])
    (display "\n")
    (put-content ip port content)))

(define (get-content-option)
  (let ([disp1 (display "Enter Node IP: ")]
        [ip (read-line)]
        [disp2 (display "Enter Node Port: ")]
        [port (string->number (read-line))]
        [disp3 (display "Enter the Content Key Hexstring: ")]
        [content-key (read-line)])
    (display "\n")
    (get-content ip port content-key)))

(define (get-rt-option)
  (let ([disp1 (display "Enter Node IP: ")]
        [ip (read-line)]
        [disp2 (display "Enter Node Port: ")]
        [port (string->number (read-line))])
    (display "\n")
    (get-routing-table ip port)))

(define (get-dht-option)
  (let ([disp1 (display "Enter Node IP: ")]
        [ip (read-line)]
        [disp2 (display "Enter Node Port: ")]
        [port (string->number (read-line))])
    (display "\n")
    (get-dht ip port)))

(define (shutdown-node-option)
  (let ([disp1 (display "Enter Node IP: ")]
        [ip (read-line)]
        [disp2 (display "Enter Node Port: ")]
        [port (string->number (read-line))])
    (display "\n")
    (shutdown-node ip port)))

(define (exit-client listen-thread)
  (kill-thread listen-thread)
  (udp-close main-socket)
  (udp-close receive-socket)
  (exit 0))

(define (print-main-menu)
  (display "Options:\n")
  (display "1. Put Content On Node\n")
  (display "2. Get Content From Node\n")
  (display "3. Get Node Routing Table\n")
  (display "4. Get Node DHT\n")
  (display "5. Shutdown Node\n")
  (display "6. Exit\n")
  (display "Select an Option: "))

(define (main)
  (print-main-menu)
  (let ([selected-option (string->number (read-line))]
        [listen-thread (listen-thread)])
    (case selected-option
      [(1) (put-content-option)]
      [(2) (get-content-option)]
      [(3) (get-rt-option)]
      [(4) (get-dht-option)]
      [(5) (shutdown-node-option)]
      [(6) (display "Goodbye!\n\n")
           (exit-client listen-thread)])
    (thread-wait listen-thread)
    (exit-client listen-thread)))
(main)