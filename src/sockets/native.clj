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
  (:import
    (java.net Inet4Address Inet6Address)
    (java.nio ByteBuffer ByteOrder)
    (com.sun.jna Function NativeLibrary Pointer Memory)))

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

(defmacro ^:private defnative [func ret-type args-type]
  `(defn ~func [& args#]
     (.invoke (Function/getFunction "c" ~(name func)) ~ret-type (to-array args#))))

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

(defn ^:private network-order [number]
  (.array (condp instance? number
            Short   (.putShort (.order (ByteBuffer/allocate 2) ByteOrder/BIG_ENDIAN) number)
            Integer (.putInt (.order (ByteBuffer/allocate 4) ByteOrder/BIG_ENDIAN) number))))

(defn ^:private to-unix-sockaddr [path]
  (if (> (count path) 107)
    (throw (IllegalArgumentException. "path is too long"))
    (doto (Memory. (+ 2 #_sun_family 108 #_sun_path))
      (.write 0 (short-array (short (:unix domain))) 0 1)
      (.write 2 (char-array path) 0 1))))

(defn ^:private to-inet-sockaddr [ip port]
  (doto (Memory. (+ 2 #_sin_family 2 #_sin_port 4 #_sin_addr 8 #_sin_zero))
    (.write 0 (short-array (short (:inet domain))) 0 1)
    (.write 2 (network-order (short port)) 0 2)
    (.write 4 (.getAddress (if (= (class ip) Inet4Address) ip (Inet4Address/getByName ip))) 0 4)))

(defn ^:private to-inet6-sockaddr
  ([ip port] (to-inet6-sockaddr ip port 0 0))
  ([ip port flow-info scope-id]
    (doto (Memory. (+ 2 #_sin6_family 2 #_sin6_port 4 #_sin6_flowinfo 16 #_sin6_addr 4 #_sin6_scope_id))
      (.write 0  (short-array (short (:inet6 domain))) 0 1)
      (.write 2  (network-order (short port)) 0 2)
      (.write 4  (network-order (int flow-info)) 0 4)
      (.write 8  (.getAddress (if (= (class ip) Inet6Address) ip (Inet6Address/getByName ip))) 0 16)
      (.write 24 (network-order (int scope-id)) 0 4))))

(defn to-sockaddr [type & args]
  (apply (ns-resolve 'sockets.native (symbol (str "to-" (name type) "-sockaddr"))) args))

(defn ^:private from-unix-sockaddr [addr]
  addr)

(defn ^:private from-inet-sockaddr [addr]
  addr)

(defn ^:private from-inet6-sockaddr [addr]
  addr)

(defn from-sockaddr [type addr]
  ((symbol (str "from-" (name type) "-sockaddr")) addr))
