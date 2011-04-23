-module(erl).
-export([start/0]).

%%% start distributed Erlang
%% erl -sname SHORT_NAME
%% erl -name LONG_NAME
%%

%%% warnings
%% erl +W w, erl +W i - print warnings as warnings / infos

%%% start MODULE (call MODULE:start)
%% erl -s MODULE [FUNCTION [ARG1, ARG2, ...]] % erl -run
start() ->
	io:format("started~n").

%%% set environment variables
%% erl -Application Par Val
%%  $ erl -mnesia dir "'/tmp/'" -s mnesia
%%  > application:get_all_env(mnesia).

%%% boot a file
%% erl -boot FILE

%%% setting cookie
%% erl -setcookie COOKIE

%%% adding paths
%% erl -pa PATH % prepend PATH to PATH variable
%% erl -pz PATH % append PATH to PATH vairalbe

%%% remote work
%% erl -remsh Node

%%% debug
%% erl +l % display info while loading code
