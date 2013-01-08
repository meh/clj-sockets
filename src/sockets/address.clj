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
  (:require [sockets.native :as native])
  (:import  [java.net InetAddress Inet4Address Inet6Address]))

(defprotocol Address
  (domain [this])
  (native [this]))

(deftype InternetAddress [address port]
  Address
  (domain [this]
    :inet)
  (native [this]
    (native/to-sockaddr :inet address port))

  Object
  (toString [this]
    (str address ":" port)))

(deftype Internet6Address [address port flow-info scope-id]
  Address
  (domain [this]
    :inet6)
  (native [this]
    (native/to-sockaddr :inet6 address port flow-info scope-id))

  Object
  (toString [this]
    (str "[" address "]:" port)))

(deftype UNIXAddress [path]
  Address
  (domain [this]
    :unix)
  (native [this]
    (native/to-sockaddr :unix path))

  Object
  (toString [this]
    (str path)))

(defn make
  ([host]
   (UNIXAddress. host))
  ([host port]
    (let [addr (InetAddress/getByName host)]
      (if (instance? Inet4Address addr)
        (InternetAddress. (.getHostAddress addr) port)
        (Internet6Address. (.getHostAddress addr) port 0
                           (.getScopeId addr)))))
  ([host port flow-info]
    (let [addr (InetAddress/getByName host)]
      (assert (instance? Inet4Address addr))
      (Internet6Address. (.getHostAddress addr) port flow-info
                         (.getScopeId addr))))
  ([host port flow-info scope-id]
    (let [addr (InetAddress/getByName host)]
      (assert (instance? Inet4Address addr))
      (Internet6Address. (.getHostAddress addr) port flow-info
                         scope-id))))

(defn internet? [addr]
  (or (instance? InternetAddress addr)
      (instance? Internet6Address addr)))

(defn unix? [addr]
  (instance? UNIXAddress addr))
