#lang racket

(require racket/udp)
(require sha)
(require json)

; Represents a Kademlia node with a given guid (256-bit), ip address (string), and port number (Number)
(struct KadNode (guid ip port))

(define GUID-SIZE 4)
(define K 20)
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
      (inexact->exact (floor (log distance 2)))))

; Calculate a list 2^k distances
(define (k-distances)
  (map (lambda (num)
         (pow 2 num))
       (build-list GUID-SIZE values)))

; Return a list of reference nodes that ThisNode should be looking for
(define (reference-nodes KadNode)
  (map (lambda (d)
         (bitwise-xor (KadNode-guid KadNode) d))
       (k-distances)))

(define (routing-table-complete? ThisNode routing-table)
  (if (routing-table-empty? routing-table)
      #f
      (let ([flat-routing-table-guids (map (lambda (bucket)
                                             (if (empty? bucket)
                                                 -1
                                                 (KadNode-guid (first bucket))))
                                           routing-table)])
        (equal? flat-routing-table-guids (reference-nodes ThisNode)))))

; Returns a list of KadNodes from ThisNode that are closest to the provided node
(define (k-closest-nodes OtherNode routing-table)
  (let ([closest-guids (reference-nodes OtherNode)])
    (if (routing-table-empty? routing-table)
        (list)
        (map (lambda (b)
               (first b))
             (filter (lambda (bucket)
                       (if (empty? bucket)
                           #f
                           (not (equal? (member (KadNode-guid (first bucket)) closest-guids) #f))))
                     routing-table)))))

; Add the node to the appropriate k-bucket
(define (routing-table-add routing-table k-index NewKadNode)
  (list-set routing-table k-index (append (list-ref
                                           routing-table
                                           k-index) (list NewKadNode))))

(define (routing-table-replace routing-table k-index NewKadNode)
  (routing-table-add (list-set routing-table k-index (remove (first (list-ref routing-table k-index))
                                                             (list-ref routing-table k-index)))
                     k-index
                     NewKadNode))

; Determine if the k-bucket already contains the provided KadNode
(define (k-bucket-contains k-index node-guid routing-table)
  (let ([k-index-node (first (list-ref routing-table k-index))])
    (if (equal? (KadNode-guid k-index-node node-guid))
        k-index-node
        #f)))

(define (routing-table-empty? routing-table)
  (equal? (member #f
                  (map (lambda (bucket)
                         (empty? bucket))
                       routing-table)) #f))

(define (print-routing-table routing-table)
  (if (routing-table-empty? routing-table)
      (display routing-table)
      (display (map (lambda (bucket)
                      (if (empty? bucket)
                          (list)
                          (KadNode-guid (first bucket))))
                    routing-table))))
  
; Check if a k-bucket has less than or equal to K nodes
#|
(define (k-bucket-less-than-K k-index routing-table)
  (<= (length (list-ref routing-table k-index) K)))

; Move the provided existing node to the tail of its k-bucket
(define (move-existing-node-to-tail k-index existing-node routing-table)
  (list-set routing-table k-index  (append (list-ref
                                            (list-set routing-table k-index(remove existing-node
                                                                                    (list-ref routing-table k-index)))
                                            k-index) existing-node)))
|#

; Insert a KadNode into the ROUTING-TABLE at the specified k-index
(define (routing-table-insert routing-table ThisNode NewKadNode k-index)
  (let ([k-bucket (list-ref routing-table k-index)])
    (if (empty? k-bucket)
        (routing-table-add routing-table k-index NewKadNode)
        (cond
          ; Make sure the routing table doesn't already contain the node.
          [(not (equal? (KadNode-guid (first k-bucket)) (KadNode-guid NewKadNode)))
           (let ([existing-node-to-ref-distance (distance (KadNode-guid (first k-bucket)) (list-ref (reference-nodes ThisNode) k-index))]
                 [new-node-to-ref-distance (distance (KadNode-guid NewKadNode) (list-ref (reference-nodes ThisNode) k-index))])
             (cond
               [(< new-node-to-ref-distance existing-node-to-ref-distance)
                (routing-table-replace routing-table k-index NewKadNode)]
               [else routing-table]))]))))

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

; Find the k-closest nodes to the sender node and put them into a list suitable for sending
(define (find-node ThisNode SenderNode routing-table)
  (map (lambda (k-node)
         (list (KadNode-guid k-node) (KadNode-ip k-node) (KadNode-port k-node)))
       (k-closest-nodes SenderNode routing-table)))

; Main lookup function
(define (node-lookup ThisNode SenderNode routing-table)
  (let ([k-index (k-index (distance (KadNode-guid ThisNode) (KadNode-guid SenderNode)))]
        [k-closest-nodes (find-node ThisNode SenderNode routing-table)])
    (if (empty? k-closest-nodes)
        (if (routing-table-empty? routing-table)
            (list)
            (filter (lambda (elem)
                      (not (equal? elem -1)))
                    (map (lambda (bucket)
                           (cond
                             [(not (empty? bucket))
                              (list (KadNode-guid (first bucket)) (KadNode-ip (first bucket)) (KadNode-port (first bucket)))]
                             [else -1]))
                         routing-table)))
        k-closest-nodes)))

#|

    (if (< (length (list-ref routing-table k-index)) ALPHA)
        ; Filter out the SenderNode so we don't call it causing a loop
        (filter (lambda (l)
                  (not (= (first l) (KadNode-guid SenderNode))))
                (map (lambda (node)
                       (list (KadNode-guid node) (KadNode-ip node) (KadNode-port node)))
                     (list-ref routing-table k-index)))
        (find-node ThisNode SenderNode routing-table))

|#

; Best reference node to use to contact a destination node
(define (contact ThisNode OtherNode routing-table)
  (first (sort
          (map (lambda (reference-node)
                 (cons reference-node (distance (KadNode-guid reference-node) (KadNode-guid OtherNode))))
               routing-table)
          (lambda (x y)
            (< (second x) (second y))))))

; Process an RPC message that was sent to ThisNode
(define (process-message ThisNode message-jsexpr items)
  (let* ([type (string->symbol (hash-ref message-jsexpr 'type))]
         [sender-node-info-list (hash-ref message-jsexpr 'kadnode)]
         [buffer (hash-ref message-jsexpr 'buffer)]
         [sender-KadNode (create-node (first sender-node-info-list)
                                      (second sender-node-info-list)
                                      (third sender-node-info-list))]
         [old-routing-table (car items)]
         [remember-list (cdr items)]
         [new-routing-table (routing-table-insert old-routing-table ThisNode sender-KadNode
                                                  (k-index (distance (KadNode-guid ThisNode)
                                                                     (KadNode-guid sender-KadNode))))])
    (display "\nOld Routing Table:\n")
    (print-routing-table old-routing-table)
    (display "\n")
    (display "\nNew Routing Table:\n")
    (print-routing-table new-routing-table)
    (display "\n")
    (case type
      [(PING) (display "PING")
              (send-message-to sender-KadNode (serialize "PING_REPLY" ThisNode ""))
              (cons new-routing-table remember-list)]
      [(STORE)     (cons new-routing-table remember-list)]
      [(FIND_NODE) (send-message-to sender-KadNode (serialize "FIND_NODE_REPLY"
                                                              ThisNode
                                                              (node-lookup ThisNode sender-KadNode old-routing-table)))
                   (cons new-routing-table remember-list)]
      [(FIND_VALUE)     (cons new-routing-table remember-list)]
      [(PING_REPLY) (display "Got Reply to Ping\n")
                    (cons new-routing-table remember-list)]
      [(FIND_NODE_REPLY) (display "Got FIND_NODE_REPLY\n")
                         (display "Buffer:\n")
                         (display buffer)
                         (let* ([buffer-minus-remembered (filter (lambda (list-item)
                                                                   (equal? (member (first list-item) remember-list) #f))
                                                                 buffer)]
                                [buffer-k-nodes (map (lambda (list-item)
                                                       (KadNode (first list-item) (second list-item) (third list-item)))
                                                     buffer-minus-remembered)])
                           (cond
                             [(not (routing-table-complete? ThisNode new-routing-table))
                              (for-each (lambda (node)
                                          (send-message-to node (serialize "FIND_NODE" ThisNode "")))
                                        buffer-k-nodes)])
                           (cons new-routing-table (append remember-list (map (lambda (list-item)
                                                                                (first list-item))
                                                                              buffer-minus-remembered))))])))

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
(udp-bind! receive-socket "127.0.0.1" 12000)

; Send a message to the selected KadNode
(define (send-message-to kad-node json-bytes)
  (udp-send-to main-socket
               (KadNode-ip kad-node)
               (KadNode-port kad-node)
               json-bytes))

; Receive Buffer (RPC messages should me no larger than 1024 bytes)
(define receive-str (make-bytes 1024))

; Listen on the receive-socket for messages
(define (listen-for-messages this-node items)
  (sync (handle-evt (udp-receive!-evt receive-socket receive-str)
                    (lambda (_)
                      (display "\n\nITEMS:\n")
                      (display items)
                      (display "\n")
                      (listen-for-messages this-node (process-message this-node (deserialize receive-str) items))))))

; -------------------------------------- Startup -------------------------------------------- ;

(define (start is-bootstrap-node custom-guid)
  (let-values ([(ip port otherIP otherPort) (udp-addresses receive-socket #t)])
    (let ([BOOTSTRAP-NODE (create-node (generate-guid "127.0.0.1" 12000) "127.0.0.1" 12000)]
          [This-Node (create-node custom-guid ip port)])
      (if is-bootstrap-node
          (thread (listen-for-messages This-Node (cons ROUTING-TABLE '())))
          (let ([remember-list (list 0)])
            (bootstrap This-Node BOOTSTRAP-NODE)
            (thread (listen-for-messages This-Node (cons ROUTING-TABLE remember-list))))))))

; (start #t #f)

