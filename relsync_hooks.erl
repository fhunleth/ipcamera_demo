-module(relsync_hooks).

-export([presync/0, postsync/0]).

presync() ->
    io:format("Got a presync~n"),
    % Need to kill the ports to update them.
    os:cmd("killall gpio_port"),

    % Mount read-write so that we can update files
    mount:remount("/", [rw]).

postsync() ->
    io:format("Got a postsync~n"),
    % Remount as read-only so that the system
    % is like it normally is.
    mount:remount("/", [ro]).
