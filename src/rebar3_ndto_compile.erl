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
-module(rebar3_ndto_compile).

%%% PROVIDER EXPORTS
-export([
    init/1,
    do/1,
    format_error/1
]).

%%% MACROS
-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).
-define(SHORT_DESC, "Automatic generation of ndto modules.").
-define(DESC,
    "Automatic generation of ndto modules.\n"
    "usage:\n"
    "{ndto, [\n"
    "    {output_dir, \"foo/foo_output_dir/\"}, % defaults to \"_gen/dtos\"\n"
    "    {parser, foo_parser}, % defaults to ndto_parser_json_schema_draft_04\n"
    "    {specs, [\n"
    "        \"foo/specs/foo.json\", % uses default options\n"
    "        {\"foo/bar/specs/foo_bar.json\", [\n"
    "            {output_dir, \"foo/bar/foo_bar_ouput_dir/\"}, % overrides default output_dir\n"
    "            {parser, foo_bar_parser} % overrides default parser\n"
    "        ]}\n"
    "    ]}\n"
    "]}."
).

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
        {example, "rebar3 ndto compile"},
        {short_desc, ?SHORT_DESC},
        {desc, ?DESC}
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
            rebar3_ndto:compile(App, State)
        end,
        Apps
    ),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
