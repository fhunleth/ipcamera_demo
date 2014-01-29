-module(relsync_hooks).

-export([presync/0, postsync/0]).

presync() ->
    io:format("Presync: stopping application~n"),

    % Stop the application if it's running so that
    % we can update the ports..
    application:stop(erlangdc_demo),

    % Mount read-write so that we can update files
    mount:remount("/", [rw]).

postsync() ->
    io:format("Postsync: restarting...~n"),

    % Make procket setuid, so that it doesn't try to run sudo
    % Currently, it doesn't know when it is being run as root.
    %file:change_mode("/srv/erlang/lib/procket-0.4.3/priv/procket", 8#4750),

    % Remount as read-only so that the system
    % is like it normally is.
    mount:remount("/", [ro]),

    % Start everything going again.
    application:start(erlangdc_demo).
