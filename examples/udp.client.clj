(require '[sockets.socket :as socket]
         '[sockets.udp :as udp])

(socket/with [client (udp/client "localhost" 2707)]
  (socket/send client "u wot m8"))
