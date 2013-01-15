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

(ns sockets.udp
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
    "Linux" {:cork [3 :bool]}
    {}))

(defonce ^:private versions
  {4 :inet
   6 :inet6})

(defn option? [name]
  (contains? options name))

(def ^:private -socket
  {:fd (fn fd [socket]
         (.fd socket))

   :set (fn set
          ([socket option]
           (if (fd/option? option)
             (fd/set (fd socket) option)
             (set socket option true)))
          ([socket option data]
           (assert (option? option))
           (let [[id type] (options name)]
             (native/setsockopt (fd socket) (native/protocol :udp)
                                id (native/pointer-for type data) (native/size-for type)))))
   :unset (fn unset [socket option]
            (if (fd/option? option)
              (fd/unset (fd socket) option)
              (set socket option false)))

   :get (fn get [socket option]
          (assert (option? option))
          (let [[id type] (options name)
                ptr (native/pointer-for type)]
            (case type
              :bool (do (native/getsockopt (fd socket) (native/protocol :udp)
                                           id ptr (native/size-for type))
                        (not (zero? (.getInt ptr)))))))})


(deftype Socket [fd version]
  Stateless
  (recv-from [this size]
    (let [ptr (Memory. size)
          addr (native/create-sockaddr (versions version))]
      [(.getByteBuffer ptr 0 (native/recv fd ptr size 0
                                          addr (native/pointer-for :int (.size addr))))
       (apply address/make (native/from-sockaddr addr))]))

  (send-to [this data to]
    (assert (satisfies? Sendable data))
    (let [[data length] (sendable data)
          addr (.native to)]
      (native/sendto fd data length 0 addr (native/pointer-for :int (.size addr))))))

(deftype Client [fd version]
  Stateful
  (recv [this size]
    (let [ptr (Memory. size)]
      (.getByteBuffer ptr 0 (native/recv fd ptr size 0))))

  (send [this data]
    (assert (satisfies? Sendable data))
    (let [[data length] (sendable data)]
      (native/send fd data length 0))))

(deftype Server [fd version]
  Stateless
  (recv-from [this size]
    (let [ptr (Memory. size)
          addr (native/create-sockaddr (versions version))]
      [(.getByteBuffer ptr 0 (native/recv fd ptr size 0
                                          addr (native/pointer-for :int (.size addr))))
       (apply address/make (native/from-sockaddr addr))]))

  (send-to [this data to]
    (assert (satisfies? Sendable data))
    (let [[data length] (sendable data)
          addr (.native to)]
      (native/sendto fd data length 0 addr (native/pointer-for :int (.size addr))))))

(extend Socket Socket* -socket)
(extend Client Socket* -socket)
(extend Server Socket* -socket)

(defn ^:private -socket [version]
  (native/socket
    (native/domain (versions version))
    (native/mode :datagram)
    (native/protocol :udp)))

(defn socket [version]
  (Socket. (-socket version) version))

(defn ^:private connect [socket addr]
  (let [sockaddr (.native addr)]
    (native/connect (fd socket) sockaddr (.size sockaddr))))

(defn client
  ([addr]
   (condp instance? addr
     InternetAddress (doto (Client. (-socket 4) 4) (connect addr))
     Internet6Address (doto (Client. (-socket 6) 6) (connect addr))))
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

(defn server
  ([addr]
   (condp instance? addr
     InternetAddress (doto (Server. (-socket 4) 4) (bind addr))
     Internet6Address (doto (Server. (-socket 6) 6) (bind addr))))
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
