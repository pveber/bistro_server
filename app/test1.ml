module X = struct
end

module Server = Bistro_server.Make(X)

let () = Lwt_main.run (Server.start ())
