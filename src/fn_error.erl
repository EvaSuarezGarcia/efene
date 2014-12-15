%% Copyright 2015 Mariano Guerra
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(fn_error).
-export([to_string/2]).

to_string(Module, Errors) when is_list(Errors) ->
    lists:map(fun (Error) -> to_string(Module, Error) end, Errors);
to_string(Module, {Type, Line, Details}) ->
    TypeStr = type_to_string(Type),
    DetailsStr = details_to_string(Details),
    io_lib:format("~p:~p:~p: ~s at line ~p: ~s~n", [Module, Line, Type, TypeStr, Line, DetailsStr]).

type_to_string(case_mismatch) -> <<"Case Mismatch">>;
type_to_string(invalid_export) -> <<"Invalid Export">>;
type_to_string(invalid_expression) -> <<"Invalid Expression">>;
type_to_string(invalid_top_level_expression) -> <<"Invalid Top Level Expression">>;
type_to_string(invalid_catch) -> <<"Invalid Catch">>;
type_to_string(Other) -> atom_to_list(Other).

format_maybe_ast({ast, Ast}) -> fn_pp:print(Ast);
format_maybe_ast(Other) -> io_lib:format("~p", [Other]).

details_to_string({expected, Expected, got, Got}) ->
    io_lib:format("Expected ~p got ~s", [Expected, format_maybe_ast(Got)]);
details_to_string(Other) -> format_maybe_ast(Other).
