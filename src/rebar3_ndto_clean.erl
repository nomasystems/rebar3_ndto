%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
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
-module(rebar3_ndto_clean).

%%% PROVIDER EXPORTS
-export([
    init/1,
    do/1,
    format_error/1
]).

%%% MACROS
-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).
-define(SHORT_DESC, "Cleans ndto modules.").

%%%-----------------------------------------------------------------------------
%%% PROVIDER EXPORTS
%%%-----------------------------------------------------------------------------
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ndto},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {opts, []},
        {example, "rebar3 ndto clean"},
        {short_desc, ?SHORT_DESC},
        {desc, ?SHORT_DESC}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    lists:foreach(
        fun(App) ->
            rebar3_ndto:clean(App, State)
        end,
        Apps
    ),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
