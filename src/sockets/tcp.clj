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
  (:require [sockets.native :as n]
            [sockets.address :as address]
            [sockets.socket :refer :all :rename {Socket Socket*}])
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

(deftype Socket [fd]
  Socket*
  (connect [this addr]
    (assert (address/internet? addr))
    (let [sockaddr (.native addr)]
      (println fd)
      (println (.size sockaddr))
      (n/connect fd sockaddr (.size sockaddr))))
  (bind [this addr]
    (assert (address/internet? addr))
    (let [sockaddr (.native addr)]
      (n/bind fd sockaddr (.size sockaddr))))
  (listen [this]
    (listen this 4096))
  (listen [this backlog]
    (n/listen fd backlog))
  (accept [this]
    (Socket. (n/accept fd nil nil)))
  (close [this]
    (n/close fd))
  (shutdown [this]
    (shutdown this #{:read :write}))
  (shutdown [this mode]
    (n/shutdown fd
                (case mode
                  #{:read}  (:read n/shutdown-mode)
                  #{:write} (:write n/shutdown-mode)
                  #{:read :write} (:both n/shutdown-mode))))
  (recv [this size]
    (let [ptr (Memory. size)]
      (.getByteBuffer ptr 0 (n/recv fd ptr size 0))))
  (send [this data]
    (assert (satisfies? Sendable data))
    (let [[data length] (sendable data)]
      (n/send fd data length 0)))
  (set [this option data]
    (if (handles-option? option)
      (set-option this data)
      (true))))

(defn socket
  ([] (socket 4))
  ([host port]
   (socket (address/make host port)))
  ([addr-or-version]
   (condp instance? addr-or-version
     InternetAddress (doto (socket 4) (.connect addr-or-version))
     Internet6Address (doto (socket 6) (.connect addr-or-version))
     (Socket. (n/socket
                   ((case addr-or-version
                      4 :inet
                      6 :inet6) n/domain)
                   (:stream n/mode)
                   (:ip n/protocol))))))

(defn socket4
  ([] (socket 4))
  ([host port]
   (socket4 (address/make host port)))
  ([addr]
   (assert (instance? InternetAddress addr))
   (socket addr)))

(defn socket6
  ([] (socket 6))
  ([host port]
   (socket6 (address/make host port)))
  ([addr]
   (assert (instance? Internet6Address addr))
   (socket addr)))
