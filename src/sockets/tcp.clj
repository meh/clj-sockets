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

(ns sockets.tcp
  (:refer-clojure :exclude [send set get])
  (:require [sockets
             [native :as native]
             [fd :as fd]
             [address :as address]
             [socket :as socket :refer :all :rename {Socket Socket*}]])
  (:import [sockets.address InternetAddress Internet6Address]
           [com.sun.jna Memory]))

(defonce ^:private options
  (case (System/getProperty "os.name")
    "Linux" {:cork           [3  :bool]
             :defer-accept   [9  :int]
             :keep-count     [6  :int]
             :keep-idle      [4  :int]
             :keep-interval  [5  :int]
             :no-delay       [1  :bool]
             :fast-open      [23 :bool]
             :quick-ack      [12 :bool]
             :syn-count      [7  :int]
             :window-clamp   [10 :int]
             :md5-signatures [14 :bool]}

    "FreeBSD" {:no-delay       [0x001 :bool]
               :keep-interval  [0x200 :int]
               :keep-count     [0x400 :int]
               :md5-signatures [0x010 :bool]}

    ("Mac OS" "Mac OS X") {:no-delay       [0x001 :bool]
                           :keep-interval  [0x200 :int]
                           :keep-count     [0x400 :int]
    {}))

(defonce ^:private versions
  {4 :inet
   6 :inet6})

(defn option? [name]
  (contains? options name))

(deftype Socket [fd side version]
  Socket*
  (fd [this]
    fd)

  (set [this option]
    (if (fd/option? option)
      (fd/set fd option)
      (set this option true)))
  (set [this option data]
    (assert (option? option))
    (let [[id type] (options name)]
      (native/setsockopt fd (native/protocol :tcp) id (native/pointer-for type data) (native/size-for type))))

  (unset [this option]
    (if (fd/option? option)
      (fd/unset fd option)
      (set this option false)))

  (get [this option]
    (assert (option? option))
    (let [[id type] (options name), ptr (native/pointer-for type)]
      (case type
        :bool (do (native/getsockopt fd (native/protocol :tcp)
                                     id ptr (native/size-for type))
                  (not (zero? (.getInt ptr))))
        :int (do (native/getsockopt fd (native/protocol :tcp)
                                    id ptr (native/size-for type))
                 (.getInt ptr)))))

  Stateful
  (recv [this size]
    (let [ptr (Memory. size)]
      (.getByteBuffer ptr 0 (native/recv fd ptr size 0))))

  (send [this data]
    (assert (satisfies? Sendable data))
    (let [[data length] (sendable data)]
      (native/send fd data length 0)))

  Addressable
  (local-address [this]
    (let [ptr (native/create-sockaddr (versions version))]
      (native/getsockname fd ptr (native/pointer-for :int (.size ptr)))
      (apply address/make (native/from-sockaddr ptr))))

  (remote-address [this]
    (let [ptr (native/create-sockaddr (versions version))]
      (native/getpeername fd ptr (native/pointer-for :int (.size ptr)))
      (apply address/make (native/from-sockaddr ptr)))))

(defn client? [socket]
  (assert (instance? Socket socket))
  (= (.side socket) :client))

(defn server? [socket]
  (assert (instance? Socket socket))
  (= (.side socket) :server))

(defn ^:private socket [side version]
  (Socket. (native/socket
             (native/domain (versions version))
             (native/mode :stream)
             (native/protocol :ip))
           side
           version))

(defn ^:private connect [socket addr]
  (let [sockaddr (.native addr)]
    (native/connect (fd socket) sockaddr (.size sockaddr))))

(defn client
  ([addr]
   (case (class addr)
     InternetAddress (doto (socket :client 4) (connect addr))
     Internet6Address (doto (socket :client 6) (connect addr))))
  ([host port]
   (client (address/make host port))))

(defn client4
  ([addr]
   (assert (instance? InternetAddress addr))
   (client addr))
  ([host port]
   (client4 (address/make host port))))

(defn client6
  ([addr]
   (assert (instance? Internet6Address addr))
   (client addr))
  ([host port]
   (client6 (address/make host port))))

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
  (Socket. (native/accept (fd socket) nil nil) :client (.version socket)))

(defn server
  ([addr]
   (case (class addr)
     InternetAddress (doto (socket :server 4) (bind addr) (listen))
     Internet6Address (doto (socket :server 6) (bind addr) (listen))))
  ([host port]
   (server (address/make host port))))

(defn server4
  ([addr]
   (assert (instance? InternetAddress addr))
   (server addr))
  ([host port]
   (server4 (address/make host port))))

(defn server6
  ([addr]
   (assert (instance? Internet6Address addr))
   (server addr))
  ([host port]
   (server6 (address/make host port))))
