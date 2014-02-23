-module(relsync_hooks).

-export([presync/0, postsync/0]).

presync() ->
    io:format("Presync: stopping application~n"),

    % Stop the application if it's running so that
    % we can update the ports..
    application:stop(troodon_cam),
    application:stop(ipcamera_demo).


postsync() ->
    io:format("Postsync: restarting...~n"),

    % Make procket setuid, so that it doesn't try to run sudo
    % Currently, it doesn't know when it is being run as root.
    %file:change_mode("/srv/erlang/lib/procket-0.4.3/priv/procket", 8#4750),

    % Start everything going again.
    application:start(troodon_cam),
    application:start(ipcamera_demo).
