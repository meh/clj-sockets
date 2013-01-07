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

(ns sockets.address
  (:require [sockets.native :as n])
  (:import  [java.net InetAddress Inet4Address Inet6Address]))

(defprotocol Address
  (domain [this])
  (native [this]))

(deftype InternetAddress [address port]
  Address
  (domain [this]
    :inet)
  (native [this]
    (n/to-sockaddr :inet address port))

  Object
  (toString [this]
    (str address ":" port)))

(deftype Internet6Address [address port flow-info scope-id]
  Address
  (native [this]
    (n/to-sockaddr :inet6 address port flow-info scope-id))
  (domain [this]
    :inet6)

  Object
  (toString [this]
    (str "[" address "]:" port)))

(deftype UNIXAddress [path]
  Address
  (native [this]
    (n/to-sockaddr :unix path))
  (domain [this]
    :unix)

  Object
  (toString [this]
    (str path)))

(defn make
  ([host] (UNIXAddress. host))
  ([host & args]
    (let [addr (InetAddress/getByName host)]
      (if (instance? Inet4Address addr)
        (InternetAddress. (.getHostAddress addr) (args 0))
        (Internet6Address. (.getHostAddress addr) (args 0) (or (get args 1) 0) (or (get args 2) (.getScopeId addr)))))))

(defn internet? [addr]
  (or (instance? InternetAddress addr)
      (instance? Internet6Address addr)))
