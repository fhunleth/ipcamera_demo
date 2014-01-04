-module(test_hooks).

-export([presync/0, postsync/0]).

presync() ->
    io:format("Got a presync~n"),
    % Mount read-write so that we can update files
    mount:remount("/", [rw]).

postsync() ->
    io:format("Got a postsync~n"),
    % Remount as read-only so that the system
    % is like it normally is.
    mount:remount("/", [ro]).

