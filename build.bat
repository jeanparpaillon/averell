@echo off
escript.exe rebar get-deps
escript.exe rebar compile
escript.exe rebar escriptize
