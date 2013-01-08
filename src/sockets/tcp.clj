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
  (:require [sockets.native :as native]
            [sockets.fd :as fd]
            [sockets.address :as address]
            [sockets.socket :as socket :refer :all :rename {Socket Socket*}])
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
                           :md5-signatures [0x010 :bool]}))

(defonce ^:private versions
  {4 :inet
   6 :inet6})

(defn option? [name]
  (contains? options name))

(deftype Socket [fd version]
  Socket*
  (fd [this]
    fd)
  (connect [this addr]
    (assert (address/internet? addr))
    (let [sockaddr (.native addr)]
      (native/connect fd sockaddr (.size sockaddr))))
  (bind [this addr]
    (assert (address/internet? addr))
    (let [sockaddr (.native addr)]
      (native/bind fd sockaddr (.size sockaddr))))
  (listen [this]
    (listen this 4096))
  (listen [this backlog]
    (native/listen fd backlog))
  (accept [this]
    (Socket. (native/accept fd nil nil) version))
  (recv [this size]
    (let [ptr (Memory. size)]
      (.getByteBuffer ptr 0 (native/recv fd ptr size 0))))
  (send [this data]
    (assert (satisfies? Sendable data))
    (let [[data length] (sendable data)]
      (native/send fd data length 0)))
  (set [this option]
    (if (fd/option? option)
      (fd/set fd option)
      (set this option true)))
  (unset [this option]
    (if (fd/option? option)
      (fd/unset fd option)
      (set this option false)))
  (set [this option data]
    (assert (option? option))
    (let [[id type] (options name)]
      (native/setsockopt fd (native/protocol :tcp) id (native/pointer-for type data) (native/size-for type))))
  (get [this option]
    (assert (option? option))
       (let [[id type] (options name), ptr (native/pointer-for type)]
         (case type
           :bool (do (native/getsockopt fd (native/protocol :tcp) id ptr (native/size-for type))
                     (not (zero? (.getInt ptr))))
           :int (do (native/getsockopt fd (native/protocol :tcp) id ptr (native/size-for type))
                    (.getInt ptr)))))
  (local-address [this]
    (let [ptr (native/create-sockaddr (versions version))]
      (native/getsockname fd ptr (native/pointer-for :int (.size ptr)))
      (apply address/make (native/from-sockaddr ptr))))
  (remote-address [this]
    (let [ptr (native/create-sockaddr (versions version))]
      (native/getpeername fd ptr (native/pointer-for :int (.size ptr)))
      (apply address/make (native/from-sockaddr ptr)))))

(defn socket [version]
  (Socket. (native/socket
             (native/domain (versions version) )
             (native/mode :stream)
             (native/protocol :ip))
           version))

(defn client
  ([host port]
   (client (address/make host port)))
  ([addr]
   (condp instance? addr
     InternetAddress (doto (socket 4) (connect addr))
     Internet6Address (doto (socket 6) (connect addr)))))

(defn client4
  ([host port]
   (client4 (address/make host port)))
  ([addr]
   (assert (instance? InternetAddress addr))
   (socket addr)))

(defn client6
  ([host port]
   (client6 (address/make host port)))
  ([addr]
   (assert (instance? Internet6Address addr))
   (socket addr)))

(defn server
  ([host port]
   (server (address/make host port)))
  ([addr]
   (condp instance? addr
     InternetAddress (doto (socket 4) (bind addr) (.listen))
     Internet6Address (doto (socket 6) (bind addr) (.listen)))))

(defn server4
  ([host port]
   (server4 (address/make host port)))
  ([addr]
   (assert (instance? InternetAddress addr))
   (socket addr)))

(defn server6
  ([host port]
   (server6 (address/make host port)))
  ([addr]
   (assert (instance? Internet6Address addr))
   (socket addr)))
