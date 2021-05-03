#lang racket

(require racket/udp)
(require sha)
(require json)
(require racket/cmdline) ; For command-line arguments	

; Represents a Kademlia node with a given guid (256-bit), ip address (string), and port number (Number)
(struct KadNode (guid ip port))

(define GUID-SIZE 4)
(define K 20)
(define ALPHA 3)

; Define sockets, one to send and one to receive on
(define main-socket (udp-open-socket))
(define receive-socket (udp-open-socket))

; Bind the sockets to the machines IP and a port
; The port for the receive-socket is the only one that
; needs to be known in order for other nodes to know where
; to send messages to.
(udp-bind! main-socket #f 0)
(udp-bind! receive-socket #f 15008)

; Simple power function
(define (pow base power)
  (foldl * 1
         (build-list power (lambda (_)
                             base))))

; -------------------------------------- DHT Storage ----------------------------------------- ;

; An empty immutable hash table to start out the DHT storage data structure
(define DHT (make-immutable-hash))

(define (dht-store dht key value)
  (hash-set dht key value))

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

; Check if our routing table is complete in that it contains
; all of our optimal reference nodes.
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

; Remove the node at the k-index (k-bucket) from the routing table
(define (routing-table-remove routing-table k-index)
  (list-set routing-table k-index (remove (first (list-ref routing-table k-index))
                                          (list-ref routing-table k-index))))

; Replace the entry in the appropriate k-bucket. This is typically done when we find
; either our reference node or a node closer to the reference node than the one already
; in the bucket.
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

; Check if the routing table is empty by checking each bucket
(define (routing-table-empty? routing-table)
  (equal? (member #f
                  (map (lambda (bucket)
                         (empty? bucket))
                       routing-table)) #f))

; Display the routing table in a readable form. E.g. a list of GUIDs instead of
; a list of KadNode struct types.
(define (print-routing-table routing-table)
  (if (routing-table-empty? routing-table)
      (display routing-table)
      (display (map (lambda (bucket)
                      (if (empty? bucket)
                          (list)
                          (KadNode-guid (first bucket))))
                    routing-table))))
  
; Check if a k-bucket has less than or equal to K nodes
; For full Kademlia protocol implementation, decided not to go that route
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
               [else routing-table]))]
          [else routing-table]))))

; -------------------------------------- Protocol ----------------------------------------- ;

; Connect to the Kademlia network through a pre-known bootstrap node
(define (bootstrap ThisNode BootstrapNode)
  (send-message-to BootstrapNode (serialize "FIND_NODE" ThisNode "")))

; Stop communicating with other KadNodes and disconnect this KadNode from
; the Kademlia network.
(define (disconnect ThisNode routing-table)
  (for-each (lambda (bucket)
              (cond
                [(not (empty? bucket))
                 (send-message-to (first bucket) (serialize "NODE_DISCONNECT" ThisNode ""))]))
            routing-table)
  (udp-close main-socket)
  (udp-close receive-socket)
  (exit 0))

; -------------------------------------- Node Funcs ----------------------------------------- ;

; Generates a GUID based on the provided ip address and port number
(define (generate-guid ip port)
  (let ([ip-bytes (string->bytes/utf-8 ip)]
        [port-bytes (string->bytes/utf-8 (number->string port))])
    (bytes->hex-string (sha256 (bytes-append ip-bytes port-bytes)))))

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

; For full Kademlia protocol implementation, decided not to go that route
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

; Process an RPC message that was sent to ThisNode.
;
; Note that this will process messages apart of the Kadmelia
; protocol as well as messages from P2P clients trying to
; store and/or access content or trying to manage ThisNode.
(define (process-message ThisNode message-jsexpr items)
  (let* ([type (string->symbol (hash-ref message-jsexpr 'type))]
         [sender-node-info-list (hash-ref message-jsexpr 'kadnode)]
         [buffer (hash-ref message-jsexpr 'buffer)]
         [sender-KadNode (create-node (first sender-node-info-list)
                                      (second sender-node-info-list)
                                      (third sender-node-info-list))]
         [old-routing-table (first items)] ; Keep track of the old routing table before the newly added entry
         [dht (second items)] ; Keep track of our DHT
         [remember-list (third items)] ; For remembering nodes we already contacted when bulding the network
         [reply-node (fourth items)] ; For constructing a reply chain when retrieving content from the network
         [got-content-reply (fifth items)] ; For ignoring other replies if we already got the content we requested
         [new-routing-table (cond
                              [(>= (KadNode-guid sender-KadNode) 0)
                               (routing-table-insert old-routing-table ThisNode sender-KadNode
                                                     (k-index (distance (KadNode-guid ThisNode)
                                                                        (KadNode-guid sender-KadNode))))]
                              [else old-routing-table])])
    (display "\nOld Routing Table:\n")
    (print-routing-table old-routing-table)
    (display "\n")
    (display "\nNew Routing Table:\n")
    (print-routing-table new-routing-table)
    (display "\n")
    (case type
      [(PING) (display "PING")
              (send-message-to sender-KadNode (serialize "PING_REPLY" ThisNode ""))
              (list new-routing-table dht remember-list reply-node #f)]
      [(CLIENT_GET_RT) (let ([routing-table-flat-list (if (routing-table-empty? new-routing-table)
                                                          new-routing-table
                                                          (map (lambda (bucket)
                                                                 (if (empty? bucket)
                                                                     bucket
                                                                     (list (KadNode-guid (first bucket)) (KadNode-ip (first bucket)) (KadNode-port (first bucket)))))
                                                               new-routing-table))])
                         (send-message-to sender-KadNode (serialize "RT_REPLY" ThisNode routing-table-flat-list))
                         (list new-routing-table dht remember-list reply-node #f))]
      [(CLIENT_GET_DHT) (send-message-to sender-KadNode (serialize "DHT_REPLY" ThisNode dht))
                        (list new-routing-table dht remember-list reply-node #f)]
      [(SHUTDOWN) (disconnect ThisNode new-routing-table)
                  (list new-routing-table dht remember-list reply-node #f)]
      [(NODE_DISCONNECT) (let ([updated-routing-table (routing-table-remove new-routing-table (k-index (distance
                                                                                                        (KadNode-guid ThisNode)
                                                                                                        (KadNode-guid sender-KadNode))))])
                           (list updated-routing-table dht remember-list reply-node #f))]
      ; Put the provided key into the Distributed Hash Table which means that
      ; this node contains the value (content) for the provided key and lets
      ; the other KadNodes in the network know of this fact.
      [(CLIENT_STORE) (let* ([content-key (bytes->hex-string (sha256 (string->bytes/utf-8 buffer)))]
                             [updated-dht (dht-store dht content-key buffer)])
                        (for-each (lambda (bucket)
                                    (cond
                                      [(not (empty? bucket))
                                       (send-message-to (first bucket) (serialize "STORE" ThisNode content-key))]))
                                  new-routing-table)
                        (send-message-to sender-KadNode (serialize "STORE_REPLY" ThisNode content-key))
                        (list new-routing-table updated-dht remember-list reply-node #f))]
      [(STORE)     (let* ([content-key buffer]
                          [updated-dht (dht-store dht content-key sender-KadNode)])
                     (list new-routing-table updated-dht remember-list reply-node #f))]
      [(FIND_NODE) (send-message-to sender-KadNode (serialize "FIND_NODE_REPLY"
                                                              ThisNode
                                                              (node-lookup ThisNode sender-KadNode old-routing-table)))
                   (list new-routing-table dht remember-list reply-node #f)]
      [(FIND_VALUE)     (let* ([content-key buffer])
                          (cond
                            [(hash-has-key? dht content-key)
                             (let ([content-value (hash-ref dht content-key)])
                               (cond
                                 [(string? content-value) (send-message-to sender-KadNode (serialize "FIND_VALUE_REPLY"
                                                                                                     ThisNode
                                                                                                     content-value))
                                                          (list new-routing-table dht remember-list reply-node #f)]
                                 [else (send-message-to content-value (serialize "FIND_VALUE"
                                                                                 ThisNode
                                                                                 content-key))
                                       (list new-routing-table dht remember-list sender-KadNode #f)]))]
                            [else (let* ([routing-table-minus-sender (remove sender-KadNode new-routing-table)])
                                    (for-each (lambda (bucket)
                                                (send-message-to (first bucket) (serialize "FIND_VALUE"
                                                                                           ThisNode
                                                                                           content-key)))
                                              routing-table-minus-sender)
                                    (list new-routing-table dht remember-list sender-KadNode #f))]))]
      [(PING_REPLY) (display "Got Reply to Ping\n")
                    (list new-routing-table dht remember-list reply-node #f)]
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
                           (list new-routing-table dht (append remember-list (map (lambda (list-item)
                                                                                    (first list-item))
                                                                                  buffer-minus-remembered)) reply-node #f))]
      [(FIND_VALUE_REPLY) (cond
                            [(equal? got-content-reply #f) (send-message-to reply-node (serialize "FIND_VALUE_REPLY"
                                                                                                  ThisNode
                                                                                                  buffer))
                                                           (list new-routing-table dht remember-list reply-node #t)])])))

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
(define (listen-for-messages this-node items)
  (sync (handle-evt (udp-receive!-evt receive-socket receive-str)
                    (lambda (_)
                      (display "\n\nITEMS:\n")
                      (display items)
                      (display "\n")
                      (listen-for-messages this-node (process-message this-node (deserialize receive-str) items))))))

; -------------------------------------- Startup -------------------------------------------- ;

; Function that starts the node and begins the Kademlia protocol
(define (start is-bootstrap-node BOOTSTRAP-NODE ThisNode)
  (if is-bootstrap-node
      (listen-for-messages ThisNode (list ROUTING-TABLE DHT '() #f #f))
      (let ([remember-list (list 0)])
        (bootstrap ThisNode BOOTSTRAP-NODE)
        (listen-for-messages ThisNode (list ROUTING-TABLE DHT remember-list #f #f)))))

; Helps parse configuration entries
(define (parse-config-entry entry-line)
  (second (string-split (string-trim entry-line))))

; Get the configuration file as a command-line argument
(define (get-config-file-arg)
  (command-line
   #:args (config-filename)
   config-filename))

; Main fuction where the program begins
; It reads in the appropriate configuration parameters from the "config.txt"
; textfile and starts the Kademlia node.
(define (main config-file)
  (define config-file-input (open-input-file config-file))
  (let ([is-bootstrap-node-str (parse-config-entry (read-line config-file-input))]
        [use-custom-guid-str (parse-config-entry (read-line config-file-input))]
        [custom-guid-str (parse-config-entry (read-line config-file-input))]
        [bootstrap-ip-str (parse-config-entry (read-line config-file-input))]
        [bootstrap-port-str (parse-config-entry (read-line config-file-input))])
    (let-values ([(ip port remoteIP remotePort) (udp-addresses receive-socket #t)])
      (let* ([is-bootstrap-node (equal? is-bootstrap-node-str "yes")]
             [use-custom-guid (equal? use-custom-guid-str "yes")]
             [custom-guid (string->number custom-guid-str)]
             [bootstrap-ip bootstrap-ip-str]
             [bootstrap-port (string->number bootstrap-port-str)]
             [ThisNode (if use-custom-guid
                           (create-node custom-guid ip port)
                           (create-node (generate-guid ip port) ip port))]
             [BOOTSTRAP-NODE (if is-bootstrap-node
                                 ThisNode
                                 (create-node -1 bootstrap-ip bootstrap-port))])
        ;(start is-bootstrap-node BOOTSTRAP-NODE ThisNode)
        (display bootstrap-port)))
    (udp-close receive-socket)
    (udp-close main-socket)))

; Run main with the first command-line argument which should be the
; name of the configuration file.
(main (get-config-file-arg))
