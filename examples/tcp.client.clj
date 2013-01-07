(require '[sockets.socket :as socket]
         '[sockets.tcp :as tcp])

(socket/with [client (tcp/client "google.com" 80)]
  #_(println client)
  (println (socket/local-address client))
  (println (socket/remote-address client)))
