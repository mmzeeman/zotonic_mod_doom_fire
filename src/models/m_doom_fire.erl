%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc Doom Fire Model.

%% Copyright 2021 Maas-Maarten Zeeman
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


-module(m_doom_fire).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(zotonic_model).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    m_get/3,
    m_post/3
]).

m_get([<<"rows">> | Rest], _Msg, _Context) ->
    Rows = rows(60, 35),
    {ok, {Rows, Rest}};

m_get(V, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.

m_post(Topic, _Msg, _Context) ->
    ?DEBUG(Topic),
    ok.

rows(Width, Height) ->
    [row(Width) || _N <- lists:seq(1, Height)].

row(Width) ->
    [0 || _N <- lists:seq(1, Width)].

