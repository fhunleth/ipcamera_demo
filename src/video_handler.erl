-module(video_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

send_pictures(Req) ->
    Picture = troodon_cam:get_next_picture(),
    MimeHeader = io_lib:format("\r\n--foofoo\r\nContent-Type: image/jpeg\r\nContent-Length: ~B\r\n\r\n", [byte_size(Picture)]),
    Msg = [list_to_binary(MimeHeader), Picture],
    ok = cowboy_req:chunk(Msg, Req),
    send_pictures(Req).

handle(Req, State) ->
    {ok, Req2} = cowboy_req:chunked_reply(200,
					  [{<<"MIME-Version">>, <<"1.0">>},
					   {<<"content-type">>, <<"multipart/x-mixed-replace; boundary=foofoo">>}],
					  Req),
    send_pictures(Req2),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
