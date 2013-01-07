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

(ns sockets.fd
  (:refer-clojure :exclude [send set get])
  (:require [sockets.native :as native]))

(defonce ^:private options
  (case (System/getProperty "os.name")
    "Linux" {:broadcast    [6  :bool]
             :reuse-addr   [2  :bool]
             :keep-alive   [9  :bool]
             :linger       [13 :bool]
             :do-not-route [5  :bool]}

    "FreeBSD" {:broadcast    [0x20 :bool]
               :reuse-addr   [0x04 :bool]
               :keep-alive   [0x08 :bool]
               :linger       [0x80 :bool]
               :do-not-route [0x10 :bool]}

    ("Mac OS" "Mac OS X") {:broadcast    [0x20 :bool]
                           :reuse-addr   [0x04 :bool]
                           :keep-alive   [0x08 :bool]
                           :linger       [0x80 :bool]
                           :do-not-route [0x10 :bool]}))

(defonce ^:private socket-level
  (case (System/getProperty "os.name")
    "Linux"               1
    "FreeBSD"             0xffff
    ("Mac OS" "Mac OS X") 0xffff))

(defn option? [name]
  (contains? options name))

(defn set [fd name & data]
  (let [[id type] (options name)]
    (apply native/setsockopt fd socket-level id
           (case type
             :bool [(native/pointer-for :bool (or (first data) true)) (native/size-for :bool)]))))

(defn get [fd name]
  (let [[id type] (options name)]
    (case type
      :bool (native/getsockopt fd socket-level (native/pointer-for :int) (native/size-for :int)))))

(defonce ^:private get-flags 3)
(defonce ^:private set-flags 4)
(defonce ^:private non-block
  (case (System/getProperty "os.name")
    "Linux"               0x0800
    "FreeBSD"             0x0004
    ("Mac OS" "Mac OS X") 0x0004))

(defn synchronous! [fd]
  (native/fcntl fd set-flags (bit-and (native/fcntl fd get-flags) (bit-not non-block))))

(defn synchronous? [fd]
  (zero? (bit-and (native/fcntl fd get-flags) non-block)))

(defn asynchronous! [fd]
  (native/fcntl fd set-flags (bit-or (native/fcntl fd get-flags) non-block)))

(defn asynchronous? [fd]
  (not (zero? (bit-and (native/fcntl fd get-flags) non-block))))

(defonce ^:private socket-type
  (case (System/getProperty "os.name")
    "Linux"               0x0003
    "FreeBSD"             0x1008
    ("Mac OS" "Mac OS X") 0x1008))

(defn alive? [fd]
  (not (neg? (native/getsockopt* fd socket-level socket-type (native/pointer-for :int) (native/size-for :int)))))
