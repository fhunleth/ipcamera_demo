-module(video_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

mime_header_for_picture(Picture) ->
    Header = io_lib:format("Content-Type: image/jpeg\r\nContent-Length: ~B\r\n\r\n",
			   [byte_size(Picture)]),
    list_to_binary(Header).

boundary() ->
    <<"\r\n--foofoo\r\n">>.

send_pictures(Req) ->
    Picture = troodon_cam:get_next_picture(),
    Msg = [boundary(), mime_header_for_picture(Picture), Picture, boundary()],
    ok = cowboy_req:chunk(Msg, Req),
    send_remaining_pictures(Req).

send_remaining_pictures(Req) ->
    Picture = troodon_cam:get_next_picture(),
    Msg = [mime_header_for_picture(Picture), Picture, boundary()],
    ok = cowboy_req:chunk(Msg, Req),
    send_remaining_pictures(Req).

handle(Req, _State) ->
    {ok, Req2} = cowboy_req:chunked_reply(200,
					  [{<<"MIME-Version">>, <<"1.0">>},
					   {<<"content-type">>, <<"multipart/x-mixed-replace; boundary=foofoo">>}],
					  Req),
    send_pictures(Req2).

terminate(_Reason, _Req, _State) ->
	ok.
