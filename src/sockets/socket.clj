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
  (:require [sockets.native :as native]
            [sockets.fd :as fd])
  (:import [java.nio ByteBuffer CharBuffer ShortBuffer IntBuffer LongBuffer]
           [com.sun.jna Memory]))

(defprotocol Socket
  (fd [this])
  (set [this option] [this option data])
  (unset [this option])
  (get [this option]))

(defprotocol Stateful
  (recv [this size])
  (send [this data])
  (local-address [this])
  (remote-address [this]))

(defprotocol Stateless
  (recv-from [this size])
  (send-to [this to data]))

(defn close [socket]
  (assert (satisfies? Socket socket))
  (native/close (fd socket)))

(defn shutdown
  ([socket]
    (shutdown socket #{:read :write}))
  ([socket mode]
   (assert (satisfies? Socket socket))
   (native/shutdown (fd socket)
                    (case mode
                      #{:read}        (:read native/shutdown-mode)
                      #{:write}       (:write native/shutdown-mode)
                      #{:read :write} (:both native/shutdown-mode)))))

(defn synchronous! [socket]
  (assert (satisfies? Socket socket))
  (fd/synchronous! (fd socket)))

(defn synchronous? [socket]
  (assert (satisfies? Socket socket))
  (fd/synchronous? (fd socket)))

(defn asynchronous! [socket]
  (assert (satisfies? Socket socket))
  (fd/asynchronous! (fd socket)))

(defn asynchronous? [socket]
  (assert (satisfies? Socket socket))
  (fd/asynchronous? (fd socket)))

(defn alive? [socket]
  (assert (satisfies? Socket socket))
  (fd/alive? (fd socket)))

(defmacro with [bindings & body]
  (assert (vector? bindings))
  (assert (even? (count bindings)))
  `(let [~@bindings]
     (try
       ~@body
       (finally
         ~@(map #(list `close %) (take-nth 2 bindings))))))

(defprotocol Sendable
  (sendable [this]))

(extend-protocol Sendable
  Byte
  (sendable [this]
    (let [ptr (Memory. Byte/SIZE)]
      [(.setByte ptr 0 this) (.size ptr)]))

  Short
  (sendable [this]
    (let [ptr (Memory. Short/SIZE)]
      [(.setShort ptr 0 this) (.size ptr)]))

  Integer
  (sendable [this]
    (let [ptr (Memory. Integer/SIZE)]
      [(.setInt ptr 0 this) (.size ptr)]))

  Long
  (sendable [this]
    (let [ptr (Memory. Long/SIZE)]
      [(.setLong ptr 0 this) (.size ptr)]))

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
