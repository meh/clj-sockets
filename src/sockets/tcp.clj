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
            [sockets.address :refer [internet?]]
            [sockets.socket :refer :all :rename {Socket Socket*}])
  (:import [sockets.address InternetAddress Internet6Address]
           [com.sun.jna Memory]))

(deftype Socket [socket]
  Socket*
  (connect [this addr]
    (assert (internet? addr))
    (let [sockaddr (.native addr)]
      (n/connect socket sockaddr (.size sockaddr))))
  (bind [this addr]
    (assert (internet? addr))
    (let [sockaddr (.native addr)]
      (n/bind socket sockaddr (.size sockaddr))))
  (listen [this]
    (listen this 4096))
  (listen [this backlog]
    (n/listen socket backlog))
  (accept [this]
    (Socket. (n/accept socket nil nil)))
  (close [this]
    (n/close socket))
  (shutdown [this]
    (shutdown this :both))
  (shutdown [this mode]
    (n/shutdown socket (mode n/shutdown-mode)))
  (recv [this size]
    (let [ptr (Memory. size)]
      (.getByteBuffer ptr 0 (n/recv socket ptr size 0))))
  (send [this data]
    (assert (satisfies? Sendable data))
    (let [buf (.array (sendable data))
          ptr (doto (Memory. (.length buf)) (.write 0 buf 0 (.length buf)))]
      (n/send socket ptr (.size ptr) 0))))

(defn socket
  ([] (socket 4))
  ([addr-or-version]
   (condp instance? addr-or-version
     InternetAddress (.connect (socket 4) addr-or-version)
     Internet6Address (.connect (socket 6) addr-or-version)
     (Socket. (n/socket
                   ((case addr-or-version
                      4 :inet
                      6 :inet6) n/domain)
                   (:stream n/mode)
                   (:ip n/protocol))))))
(defn socket4
  ([] (socket 4))
  ([addr]
   (assert (instance? InternetAddress addr))
   (socket addr)))

(defn socket6
  ([] (socket 6))
  ([addr]
   (assert (instance? Internet6Address addr))
   (socket addr)))
