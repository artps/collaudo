-module(cld_app).
-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/websocket", cld_websocket_handler, []},
      {"/[...]", cowboy_static, [
        {directory, {priv_dir, cld, []}},
        {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
        {etag, {attributes, [filepath, filesize, inode, mtime]}}
      ]}
    ]}
  ]),

  {ok, _}   = cowboy:start_http(http, 100, [{port, 8080}],[
            {env, [{dispatch, Dispatch}]}
  ]),
  {ok, Pid} = cld_sup:start_link(),
  {ok, Pid}.

stop(_State) ->
  ok.
