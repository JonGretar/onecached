{application, onecached,
  [{description,
    "OneCached is a memcached (http://danga.com/memcached/) server"},
   {vsn,          "1.0.0"},
   {modules,      [onecached, onecached_app, onecached_server, onecached_storage,
                   onecached_listener, onecached_sup]},
   {registered,   [onecached_sup, onecached_listener]},
   {applications, [kernel, stdlib, sasl]},
   {mod, {onecached_app,[]}}]}.
