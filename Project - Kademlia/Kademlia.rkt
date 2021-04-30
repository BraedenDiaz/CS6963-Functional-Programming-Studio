#lang racket

(require racket/udp)
(require sha)
(require json)

; Represents a Kademlia node with a given guid (256-bit), ip address (string), and port number (Number)
(struct KadNode (guid ip port))

(define GUID-SIZE 4)
(define ALPHA 3)

; Define sockets, one to send and one to receive on
(define main-socket (udp-open-socket))
(define receive-socket (udp-open-socket))

; Simple power function
(define (pow base power)
  (foldl * 1
         (build-list power (lambda (_)
                             base))))

; -------------------------------------- Routing Table ----------------------------------------- ;

; A k-bucket list of list of KadNodes
(define ROUTING-TABLE (build-list GUID-SIZE (lambda (num)
                                              (list))))
; Calculate the index of which k-bucket corresponds to a specific distance
(define (k-index distance)
  (if (= distance 0)
      0
      (inexact->exact (floor (log distance)))))

; Determine if the k-bucket already contains the provided KadNode
(define (k-bucket-contains k-index node-guid routing-table)
  (let ([bucket (memf (lambda (n)
                        (equal? node-guid (KadNode-guid n)))
                      (list-ref routing-table k-index))])
    (if (equal? bucket #f)
        #f
        (first bucket))))

; Insert a KadNode into the ROUTING-TABLE at the specified k-index
(define (routing-table-insert routing-table KadNode k-index)
  (cond
    ; Make sure the routing table doesn't already contain the node
    [(equal? (k-bucket-contains k-index (KadNode-guid KadNode) routing-table) #f)
     (list-set routing-table k-index (append (list-ref
                                              routing-table
                                              k-index) (list KadNode)))]))

; -------------------------------------- Protocol ----------------------------------------- ;

; Connect to the Kademlia network through a pre-known bootstrap node
(define (bootstrap ThisNode BootstrapNode)
  (send-message-to BootstrapNode (serialize "FIND_NODE" ThisNode "")))

; Get a list of peers from the Kademlia network that contain a
; value (content) associated with the provided key.
(define (get-content key callback)
  (display "TODO"))

; Put the provided key into the Distributed Hash Table which means that
; this node contains the value (content) for the provided key and lets
; the other KadNodes in the network know of this fact.
(define (put-content key)
  (display "TODO"))

; Stop communicating with other KadNodes and disconnect this KadNode from
; the Kademlia network.
(define (disconnect)
  (udp-close main-socket)
  (udp-close receive-socket))

; -------------------------------------- Node Funcs ----------------------------------------- ;

; Generates a GUID based on the provided ip address and port number
(define (generate-guid ip port)
  (let ([ip-bytes (string->bytes/utf-8 ip)]
        [port-bytes (string->bytes/utf-8 (number->string port))])
    (sha256 (bytes-append ip-bytes port-bytes))))

; Return the logical xor distance between two KadNode's GUIDs
(define (distance guid1 guid2)
  (bitwise-xor guid1 guid2))

; Create a new KadNode struct
(define (create-node guid ip port)
  (if guid
      (KadNode guid ip port)
      (KadNode (generate-guid ip port) ip port)))

; Calculate a list 2^k distances
(define (k-distances)
  (map (lambda (num)
         (pow 2 num))
       (build-list GUID-SIZE values)))

; Returns a list of KadNodes from ThisNode that are closest to the provided node
(define (k-closest-nodes ThisNode OtherNode routing-table)
  (let ([closest-guids (map (lambda (d)
                              (bitwise-xor (KadNode-guid OtherNode) d))
                            (k-distances))])
    (map (lambda (close-guid)
           (k-bucket-contains (k-index (distance (KadNode-guid ThisNode) close-guid)) close-guid routing-table))
         closest-guids)))

; Find the k-closest nodes to the sender node and put them into a list suitable for sending
(define (find-node ThisNode SenderNode routing-table)
  (map (lambda (node)
         (if (equal? node #f)
             #f
             (list (KadNode-guid node) (KadNode-ip node) (KadNode-port node))))
       (k-closest-nodes ThisNode SenderNode routing-table)))

(define (default-lookup k-index routing-table)
  (list-ref routing-table k-index))

; Main lookup function
(define (node-lookup ThisNode SenderNode routing-table)
  (let ([k-index (k-index (distance (KadNode-guid ThisNode) (KadNode-guid SenderNode)))])
    (if (< (length (list-ref routing-table k-index)) ALPHA)
        (filter (lambda (l)
                  (not (= (first l) (KadNode-guid SenderNode))))
                (map (lambda (node)
                       (list (KadNode-guid node) (KadNode-ip node) (KadNode-port node)))
                     (default-lookup k-index routing-table)))
        (find-node ThisNode SenderNode routing-table))))

; Best reference node to use to contact a destination node
(define (contact ThisNode OtherNode routing-table)
  (first (sort
          (map (lambda (reference-node)
                 (cons reference-node (distance (KadNode-guid reference-node) (KadNode-guid OtherNode))))
               routing-table)
          (lambda (x y)
            (< (second x) (second y))))))

; Process an RPC message that was sent to ThisNode
(define (process-message ThisNode message-jsexpr routing-table)
  (let* ([type (string->symbol (hash-ref message-jsexpr 'type))]
         [sender-node-info-list (hash-ref message-jsexpr 'kadnode)]
         [buffer (hash-ref message-jsexpr 'buffer)]
         [sender-KadNode (create-node (first sender-node-info-list)
                                      (second sender-node-info-list)
                                      (third sender-node-info-list))]
         [routing-table (routing-table-insert routing-table sender-KadNode
                                              (k-index (distance (KadNode-guid ThisNode)
                                                                 (KadNode-guid sender-KadNode))))])
    (display "\nRouting Table:\n")
    (display routing-table)
    (display "\n")
    (case type
      [(PING) (display "PING")
              (send-message-to sender-KadNode (serialize "PING_REPLY" ThisNode ""))]
      [(STORE) (display "TODO")]
      [(FIND_NODE) (send-message-to sender-KadNode (serialize "FIND_NODE_REPLY"
                                                              ThisNode
                                                              (node-lookup ThisNode sender-KadNode routing-table)))]
      [(FIND_VALUE) (display "TODO")]
      [(PING_REPLY) (display "Got Reply to Ping\n")]
      [(FIND_NODE_REPLY) (display "Got FIND_NODE_REPLY\n")
                         (display "Buffer:\n")
                         (display buffer)
                         (for-each (lambda (node)
                                     (send-message-to node (serialize "PING" ThisNode "")))
                                   (map (lambda (list-item)
                                          (KadNode (first list-item) (second list-item) (third list-item)))
                                        buffer))])
    routing-table))

; -------------------------------------- Serialization ----------------------------------------- ;

(define (serialize type kad-node buffer)
  (jsexpr->bytes (hash 'type type
                       'kadnode (list (KadNode-guid kad-node) (KadNode-ip kad-node) (KadNode-port kad-node))
                       'buffer buffer)))

(define (deserialize message)
  (bytes->jsexpr message))

; -------------------------------------- Networking -------------------------------------------- ;

; Bind the sockets to the machines IP and a port
; The port for the receive-socket is the only one that
; needs to be known in order for other nodes to know where
; to send messages to.
(udp-bind! main-socket "127.0.0.1" 0)
(udp-bind! receive-socket "127.0.0.1" 13000)

; Send a message to the selected KadNode
(define (send-message-to kad-node json-bytes)
  (udp-send-to main-socket
               (KadNode-ip kad-node)
               (KadNode-port kad-node)
               json-bytes))

; Receive Buffer (RPC messages should me no larger than 1024 bytes)
(define receive-str (make-bytes 1024))

; Listen on the receive-socket for messages
(define (listen-for-messages this-node routing-table)
  (sync (handle-evt (udp-receive!-evt receive-socket receive-str)
                    (lambda (_)
                      (listen-for-messages this-node (process-message this-node (deserialize receive-str) routing-table))))))

; -------------------------------------- Startup -------------------------------------------- ;

(define (start is-bootstrap-node custom-guid)
  (let-values ([(ip port otherIP otherPort) (udp-addresses receive-socket #t)])
    (let ([BOOTSTRAP-NODE (create-node (generate-guid "127.0.0.1" 13000) "127.0.0.1" 13000)]
          [This-Node (create-node custom-guid ip port)])
      (if is-bootstrap-node
          This-Node
          (bootstrap This-Node BOOTSTRAP-NODE))
      (thread (listen-for-messages This-Node ROUTING-TABLE)))))

; (start #t #f)

