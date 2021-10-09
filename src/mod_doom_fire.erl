%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc Provides server rendered live updating doom fire.

%% Copyright 2019-2021 Maas-Maarten Zeeman 
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

-module(mod_doom_fire).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    init/1,

    observe_teleview_state_init/2,
    observe_teleview_render/2,

    observe_acl_is_allowed/2,

    tick/2
]).

init(_Context) ->
    ok.

%% Observe initialization of the doom_fire teleview.
observe_teleview_state_init({teleview_state_init, #{ width := Width,
                                                     height := Height,
                                                     type := doom_fire } = Args}, Context) ->
    %% Add the initial state of the fire to the args.
    Fire = m_doom_fire:new(Width, Height),
    Fire1 = m_doom_fire:add_fire_source(Fire),

    Args1 = Args#{doom_fire => Fire1},

    {ok, Args1, z_acl:anondo(z_context:prune_for_scomp(Context))};
observe_teleview_state_init({teleview_state_init, #{ }=A}, _Context) ->
    undefined.

%% Observe render trigger of the doom_fire teleview
observe_teleview_render({teleview_render, Id, #{ tick := _Tick }, #{ type := doom_fire, doom_fire := Fire} = Args}, Context) ->
    %% When there is a tick, do a fire propagation
    Fire1 = m_doom_fire:fire_propagation(Fire),
    Args#{doom_fire := Fire1};
observe_teleview_render({teleview_render, _Id, _Msg, #{}}, _Context) ->
    undefined.


