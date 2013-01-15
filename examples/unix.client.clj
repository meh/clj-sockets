(require '[sockets.socket :as socket]
         '[sockets.unix :as unix])

(socket/with [client (unix/client (first *command-line-args*))]
  (socket/send client "lol\n")
  (println (socket/local-address client))
  (println (socket/remote-address client)))
