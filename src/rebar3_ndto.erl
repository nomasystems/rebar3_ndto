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
-module(rebar3_ndto).

%%% INIT/STOP EXPORTS
-export([
    init/1
]).

%%% EXTERNAL EXPORTS
-export([
    compile/2,
    clean/2
]).

%%% MACROS
-define(DEFAULT_OUTPUT_DIR, "_gen/dtos").
-define(DEFAULT_PARSER, ndto_parser_json_schema).

%%%-----------------------------------------------------------------------------
%%% INIT/STOP EXPORTS
%%%-----------------------------------------------------------------------------
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_ndto_compile:init(State),
    {ok, State2} = rebar3_ndto_clean:init(State1),
    {ok, State2}.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec compile(rebar_app_info:t(), rebar_state:t()) -> ok.
compile(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Conf = rebar_state:get(State, ndto, []),
    GlobalOutputDir = proplists:get_value(output_dir, Conf, ?DEFAULT_OUTPUT_DIR),
    GlobalParser = proplists:get_value(parser, Conf, ?DEFAULT_PARSER),
    Specs = proplists:get_value(specs, Conf, []),
    lists:foreach(
        fun
            ({Spec, SpecOpts}) ->
                OutputDir = filename:join(
                    AppDir, proplists:get_value(output_dir, SpecOpts, GlobalOutputDir)
                ),
                Parser = proplists:get_value(parser, SpecOpts, GlobalParser),
                compile(AppDir, OutputDir, Parser, Spec);
            (Spec) ->
                OutputDir = filename:join(AppDir, GlobalOutputDir),
                Parser = GlobalParser,
                compile(AppDir, OutputDir, Parser, Spec)
        end,
        Specs
    ).

-spec clean(rebar_app_info:t(), rebar_state:t()) -> ok.
clean(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Conf = rebar_state:get(State, ndto, []),
    GlobalOutputDir = proplists:get_value(output_dir, Conf, ?DEFAULT_OUTPUT_DIR),
    Specs = proplists:get_value(specs, Conf, []),
    lists:foreach(
        fun
            ({_Spec, SpecOpts}) ->
                case proplists:get_value(output_dir, SpecOpts, undefined) of
                    undefined ->
                        ok;
                    OutputDir ->
                        clean(filename:join(AppDir, OutputDir))
                end;
            (_Spec) ->
                ok
        end,
        Specs
    ),
    clean(filename:join(AppDir, GlobalOutputDir)).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
compile(AppDir, OutputDir, Parser, RawSpec) ->
    SpecPath = unicode:characters_to_binary(filename:join(AppDir, RawSpec)),
    case ndto_parser:parse(Parser, SpecPath) of
        {error, Reason} ->
            rebar_utils:abort("[rebar3_ndto] Failed parsing ~s spec: ~p\n", [SpecPath, Reason]);
        {ok, Schemas} ->
            case filelib:ensure_path(OutputDir) of
                {error, Reason} ->
                    rebar_utils:abort("[rebar3_ndto] Failed creating ~s folder: ~p\n", [
                        OutputDir, Reason
                    ]);
                ok ->
                    lists:foreach(
                        fun({SchemaName, Schema}) ->
                            DTO = ndto:generate(SchemaName, Schema),
                            OutputFile = filename:join(
                                OutputDir, erlang:atom_to_list(SchemaName) ++ ".erl"
                            ),
                            case ndto:write(DTO, OutputFile) of
                                {error, Reason} ->
                                    rebar_utils:abort(
                                        "[rebar3_ndto] Failed writing ~s DTO: ~p\n", [
                                            OutputFile, Reason
                                        ]
                                    );
                                ok ->
                                    ok
                            end
                        end,
                        Schemas
                    )
            end
    end.

clean(OutputDir) ->
    case file:list_dir(OutputDir) of
        {error, _Reason} ->
            ok;
        {ok, Filenames} ->
            lists:foreach(
                fun(File) ->
                    file:delete(File)
                end,
                Filenames
            )
    end.
