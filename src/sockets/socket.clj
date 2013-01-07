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

(ns sockets.socket
  (:refer-clojure :exclude [send set get])
  (:import [java.nio ByteBuffer CharBuffer ShortBuffer IntBuffer LongBuffer]
           [com.sun.jna Memory]))

(defprotocol Socket
  (connect [this addr])
  (bind [this addr])
  (listen [this] [this backlog])
  (accept [this])
  (close [this])
  (shutdown [this] [this mode])
  (recv [this size])
  (send [this data])
  (recvfrom [this size])
  (sendto [this to data])
  (set [this option] [this option data])
  (unset [this option])
  (get [this option])
  (local-address [this])
  (remote [this]))

(defprotocol Sendable
  (sendable [this]))

(extend-protocol Sendable
  Byte
  (sendable [this]
    (let [size Byte/SIZE]
      [(.put (ByteBuffer/allocateDirect size) this) size]))

  Short
  (sendable [this]
    (let [size Short/SIZE]
      [(.putShort (ByteBuffer/allocateDirect size) this) size]))

  Integer
  (sendable [this]
    (let [size Integer/SIZE]
      [(.putInt (ByteBuffer/allocateDirect size) this) size]))

  Long
  (sendable [this]
    (let [size Long/SIZE]
      (.putLong (ByteBuffer/allocateDirect Long/SIZE) this)))

  String
  (sendable [this]
    (let [buf (.getBytes this "UTF-8")]
      [buf (alength buf)]))

  ByteBuffer
  (sendable [this]
    [this (.position this)])

  ShortBuffer
  (sendable [this]
    [this (* (.position this) Short/SIZE)])

  IntBuffer
  (sendable [this]
    [this (* (.position this) Integer/SIZE)])

  LongBuffer
  (sendable [this]
    [this (* (.position this) Long/SIZE)]))
