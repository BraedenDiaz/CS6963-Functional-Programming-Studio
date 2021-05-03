#lang racket

(require racket/udp)
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
(udp-bind! main-socket "127.0.0.1" 0)
(udp-bind! receive-socket "127.0.0.1" 10001)

(define (process-message message-jsexpr)
  (let* ([type (string->symbol (hash-ref message-jsexpr 'type))]
         [sender-node-info-list (hash-ref message-jsexpr 'kadnode)]
         [buffer (hash-ref message-jsexpr 'buffer)])
    (case type
      [(FIND_VALUE_REPLY) (display "Got Content:\n")
                 (display (string-append buffer "\n"))]
      [(STORE_REPLY) (display "Successfully stored content:\n")
                     (display (string-append (string-append "Node: " (number->string (first sender-node-info-list))) "\n"))
                     (display (string-append (string-append "Key: " buffer) "\n"))])))

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
                      (process-message (deserialize receive-str))
                      (listen-for-messages)))))

; -------------------------------------- Client Commands  ----------------------------------------- ;

(define listen-thread (thread listen-for-messages))

(define (put-content ip port content)
  (udp-send-to main-socket ip port (serialize "CLIENT_STORE" (KadNode -1 "127.0.0.1" 10001) content)))

(define (get-content ip port content-key)
  (udp-send-to main-socket ip port (serialize "FIND_VALUE" (KadNode -1 "127.0.0.1" 10001) content-key)))