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

(ns sockets.native
  (:import (com.sun.jna Function NativeLibrary Pointer)))

(defonce domain
  {:unix  1
   :inet  2
   :inet6 (case (System/getProperty "os.name")
            "Linux"               10
            "FreeBSD"             28
            ("Mac OS" "Mac OS X") 30)})

(defonce type
  {:stream   1
   :datagram 2
   :raw      3})

(defonce protocol
  {:ip   0
   :icmp 1
   :tcp  6
   :udp  17})

; sockaddr (unsigned short, char[14])

(defmacro defnative [func ret-type args-type]
  `(defn ~func [& args#]
     (.invoke (Function/getFunction "c" (name '~func)) ~ret-type (to-array args#))))

(defnative socket
  Integer
  [Integer Integer Integer])

(defnative connect
  Integer
  [Integer Pointer Integer])

(defnative bind
  Integer
  [Integer Pointer Integer])

(defnative listen
  Integer
  [Integer Integer])

(defnative accept
  Integer
  [Integer Pointer Pointer])

(defnative close
  Integer
  [Integer])

(defnative recv
  Long
  [Integer Pointer Long Integer])

(defnative recvfrom
  Long
  [Integer Pointer Long Integer Pointer Pointer])

(defnative send
  Long
  [Integer Pointer Long Integer])

(defnative sendto
  Long
  [Integer Pointer Long Integer Pointer Pointer])
