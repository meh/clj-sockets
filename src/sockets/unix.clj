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

(ns sockets.unix
  (:refer-clojure :exclude [send set get])
  (:require [sockets
             [native :as native]
             [fd :as fd]
             [address :as address]
             [socket :as socket :refer :all :rename {Socket Socket*}]])
  (:import [sockets.address UNIXAddress]
           [com.sun.jna Memory]))

(defonce ^:private options
  {})

(defn option? [name]
  (contains? options name))

(deftype Socket [fd side mode]
  Socket*
  (fd [this]
    fd)

  (set [this option]
    (if (fd/option? option)
      (fd/set fd option)
      (set this option true)))
  (set [this option data]
    (assert (option? option)))

  (unset [this option]
    (if (fd/option? option)
      (fd/unset fd option)
      (set this option false)))

  (get [this option]
    (if (fd/option? option)
      (fd/get fd option)
      (assert (option? option))))

  Stateful
  (recv [this size]
    (let [ptr (Memory. size)]
      (.getByteBuffer ptr 0 (native/recv fd ptr size 0))))

  (send [this data]
    (assert (satisfies? Sendable data))
    (let [[data length] (sendable data)]
      (native/send fd data length 0))))

(defn mode [socket]
  (.mode socket))

(defn client? [socket]
  (assert (instance? Socket socket))
  (= (.side socket) :client))

(defn server? [socket]
  (assert (instance? Socket socket))
  (= (.side socket) :server))

(defn ^:private socket [side mode]
  (Socket. (native/socket
             (native/domain :unix)
             (native/mode mode)
             (native/protocol :ip))
           side
           mode))

(defn ^:private connect [socket addr]
  (let [sockaddr (.native addr)]
    (native/connect (fd socket) sockaddr (.size sockaddr))))

(defn client
  ([path-or-addr]
   (client path-or-addr :stream))
  ([path-or-addr mode]
   (if (instance? UNIXAddress path-or-addr)
     (doto (socket :client mode) (connect path-or-addr))
     (client (address/make path-or-addr) mode))))

(defn ^:private bind [this addr]
  (let [sockaddr (.native addr)]
    (native/bind fd sockaddr (.size sockaddr))))

(defn ^:private listen
  ([this]
   (listen this 4096))
  ([this backlog]
   (native/listen fd backlog)))

(defn accept [socket]
  (assert (instance? Socket socket))
  (Socket. (native/accept (fd socket) nil nil) :client (.mode socket)))

(defn server
  ([path-or-addr]
   (server path-or-addr :stream))
  ([path-or-addr mode]
   (if (instance? UNIXAddress path-or-addr)
     (doto (socket :server mode) (bind path-or-addr) (listen))
     (server (address/make path-or-addr) mode))))
