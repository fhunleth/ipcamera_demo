-module(video_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

multipart_header(Picture) ->
    io_lib:format("Content-Type: image/jpeg\r\n"
		  "Content-Length: ~B\r\n\r\n",
		  [byte_size(Picture)]).

boundary() -> <<"foofoo">>.
delimiter() ->
    Boundary = boundary(),
    <<"\r\n--", Boundary/binary, "\r\n">>.

send_first_picture(Req) ->
    Pic = troodon_cam:get_next_picture(),
    Msg = [delimiter(), multipart_header(Pic), Pic, delimiter()],
    ok = cowboy_req:chunk(Msg, Req).

send_pictures(Req) ->
    Pic = troodon_cam:get_next_picture(),
    Msg = [multipart_header(Pic), Pic, delimiter()],
    ok = cowboy_req:chunk(Msg, Req),
    send_pictures(Req).

handle(Req, _State) ->
    Boundary = boundary(),
    Headers =
	[{<<"connection">>, <<"close">>},
	 {<<"Cache-Control">>, <<"no-cache">>},
	 {<<"MIME-Version">>, <<"1.0">>},
	 {<<"content-type">>, <<"multipart/x-mixed-replace; ",
				"boundary=", Boundary/binary>>}],
    {ok, Req2} = cowboy_req:chunked_reply(200,
					  Headers,
					  Req),
    send_first_picture(Req2),
    send_pictures(Req2).

terminate(_Reason, _Req, _State) ->
	ok.
