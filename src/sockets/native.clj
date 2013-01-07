;; Copyleft (ɔ) meh. - http://meh.schizofreni.co
;;
;; This file is part of clj-sockets - https://github.com/meh/clj-sockets
;;
;; clj-sockets is free software: you can redistribute it and/or modify it under
;; the terms of the Lesser GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; clj-sockets is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License
;; for more details.
;;
;; You should have received a copy of the Lesser GNU General Public License
;; along with clj-sockets If not, see <http://www.gnu.org/licenses/>.

(ns sockets.native
  (:refer-clojure :exclude [send])
  (:import [java.net InetAddress Inet4Address Inet6Address]
           [java.nio ByteBuffer ByteOrder]
           [com.sun.jna Native Function Pointer Memory]))

(defonce domain
  {:unix  1
   :inet  2
   :inet6 (case (System/getProperty "os.name")
            "Linux"               10
            "FreeBSD"             28
            ("Mac OS" "Mac OS X") 30)})

(defonce mode
  {:stream   1
   :datagram 2
   :raw      3})

(defonce protocol
  {:ip   0
   :icmp 1
   :tcp  6
   :udp  17})

(defonce shutdown-mode
  {:read  0
   :write 1
   :both  2})

(defmacro ^:private defnative* [func ret-type args-type]
  `(defn ~func [& args#]
     (.invoke (Function/getFunction "c" ~(name func)) ~ret-type (to-array args#))))

(defmacro ^:private defnative
  ([func ret-type args-type]
   `(defnative ~func ~ret-type ~args-type ~(fn [res] (neg? res))))
  ([func ret-type args-type errno-check]
   `(do
     (defnative* ~(symbol (str (name func) "*")) ~ret-type ~args-type)
     (defn ~func [& args#]
        (let [res# (.invoke (Function/getFunction "c" ~(name func)) ~ret-type (to-array args#))]
          (if (~errno-check res#)
            (let [errno# (Native/getLastError)]
              (throw (ex-info (strerror (Native/getLastError)) {:code errno#})))
            res#))))))

(defnative* strerror
  String
  [Integer])

(defnative socket
  Integer
  [Integer Integer Integer])

(defnative connect
  Integer
  [Integer Pointer Integer])

(defnative bind
  Integer
  [Integer Pointer Integer])

(defnative listen
  Integer
  [Integer Integer])

(defnative accept
  Integer
  [Integer Pointer Pointer])

(defnative close
  Integer
  [Integer])

(defnative shutdown
  Integer
  [Integer Integer])

(defnative recv
  Long
  [Integer Pointer Long Integer])

(defnative recvfrom
  Long
  [Integer Pointer Long Integer Pointer Pointer])

(defnative send
  Long
  [Integer Pointer Long Integer])

(defnative sendto
  Long
  [Integer Pointer Long Integer Pointer Pointer])

(defnative setsockopt
  Integer
  [Integer Integer Integer Pointer Integer])

(defnative getsockopt
  Integer
  [Integer Integer Integer Pointer Pointer])

(defnative getsockname
  Integer
  [Integer Pointer Pointer])

(defnative getpeername
  Integer
  [Integer Pointer Pointer])

(defnative fcntl
  Integer
  [Integer Integer Integer])

(defn ^:private network-order [number]
  (.array (condp instance? number
            Short   (.putShort (.order (ByteBuffer/allocate 2) ByteOrder/BIG_ENDIAN) number)
            Integer (.putInt (.order (ByteBuffer/allocate 4) ByteOrder/BIG_ENDIAN) number))))

(defmulti create-sockaddr (fn [type] type))

(defmethod create-sockaddr :unix [_]
  (Memory. (+ #_(unsigned short)     2
              #_(char sun_path[108]) 108)))

(defmethod create-sockaddr :inet [_]
  (Memory. (+ #_(short sin_family)        2
              #_(unsigned short sin_port) 2
              #_(unsigned long sin_addr)  4
              #_(char sin_zero[8])        8)))

(defmethod create-sockaddr :inet6 [_]
  (Memory. (+ #_(uint16_t sin6_family)        2
              #_(uint16_t sin6_port)          2
              #_(uint32_t sin6_flowinfo)      4
              #_(unsigned char sin6_addr[16]) 16
              #_(uint32_t sin6_scope_id)      4)))

(defmulti to-sockaddr (fn [type & _] type))

(defmethod to-sockaddr :unix [_ path]
  (if (> (count path) 107)
    (throw (IllegalArgumentException. "path is too long"))
    (doto (create-sockaddr :unix)
      (.clear)
      (.write 0 (short-array [(short (:unix domain))]) 0 1)
      (.write 2 (char-array path) 0 1))))

(defmethod to-sockaddr :inet [_ ip port]
  (doto (create-sockaddr :inet)
    (.clear)
    (.write 0 (short-array [(short (:inet domain))]) 0 1)
    (.write 2 (network-order (short port)) 0 2)
    (.write 4 (.getAddress (if (instance? Inet4Address ip) ip (Inet4Address/getByName ip))) 0 4)))

(defmethod to-sockaddr :inet6
  ([_ ip port]
   (to-sockaddr :inet6 ip port 0 0))
  ([_ ip port flow-info scope-id]
    (doto (create-sockaddr :inet6)
      (.clear)
      (.write 0  (short-array [(short (:inet6 domain))]) 0 1)
      (.write 2  (network-order (short port)) 0 2)
      (.write 4  (network-order (int flow-info)) 0 4)
      (.write 8  (.getAddress (if (instance? Inet6Address ip) ip (Inet6Address/getByName ip))) 0 16)
      (.write 24 (network-order (int scope-id)) 0 4))))

(defn from-sockaddr [ptr]
  (let [result (transient [])]
    (persistent! (case ((zipmap (vals domain) (keys domain)) (.getShort ptr 0))
                   :unix (conj! result (.getString ptr 2))
                   :inet (-> result
                             (conj! (-> ptr
                                        (.getByteArray 4 4)
                                        (InetAddress/getByAddress)
                                        (.getHostAddress)))
                             (conj! (-> ptr
                                        (.getByteBuffer 2 2)
                                        (.order ByteOrder/BIG_ENDIAN)
                                        (.getShort)
                                        (bit-and 0xffff))))
                   :inet6 (-> result
                              (conj! (-> ptr
                                         (.getByteArray 8 16)
                                         (InetAddress/getByAddress)
                                         (.getHostAddress)))
                              (conj! (-> ptr
                                         (.getByteBuffer 2 2)
                                         (.order ByteOrder/BIG_ENDIAN)
                                         (.getShort)
                                         (bit-and 0xffff)))
                              (conj! (-> ptr
                                         (.getByteBuffer 4 2)
                                         (.order ByteOrder/BIG_ENDIAN)
                                         (.getShort)
                                         (bit-and 0xffff)))
                              (conj! (-> ptr
                                         (.getByteBuffer 24 4)
                                         (.order ByteOrder/BIG_ENDIAN)
                                         (.getInt)
                                         (bit-and 0xffffffff))))))))

(defn size-for [type]
  (case type
    :bool Integer/SIZE
    :int  Integer/SIZE))

(defn pointer-for
  ([type]
   (Memory. (size-for type)))
  ([type data]
   (let [ptr (pointer-for type)]
     (case type
       :bool (.setInt ptr 0 (if data 1 0))
       :int  (.setInt ptr 0 data))
     ptr)))
