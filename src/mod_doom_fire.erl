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
-include_lib("zotonic_mod_teleview/include/teleview.hrl").

-export([
    observe_teleview_init/2,
    observe_teleview_render/2
]).

%% Observe initialization of the doom_fire teleview state process. This is the process controlling
%% all the possible renderers of the teleview. This process subscribes to the requested topics. It
%% is possible to do special initializations here. In this case, the doom_fire record is intialized,
%% and added to the state. 
observe_teleview_init(#teleview_init{ args =  #{ width := Width,
                                                 height := Height,
                                                 type := doom_fire } = Args}, Context) ->
    %% Get the initial state of the fire from the supplied arguments.
    Fire = m_doom_fire:new(Width, Height),
    Fire1 = m_doom_fire:add_fire_source(Fire),

    % Add the fire to the state.
    Args1 = Args#{doom_fire => Fire1},

    % Run the teleview with an anonymous context. 
    {ok, Args1, z_acl:anondo(z_context:new(Context))};
observe_teleview_init(#teleview_init{}, _Context) ->
    undefined.

%% Observe render trigger of the doom_fire teleview
observe_teleview_render(#teleview_render{ msg = #{ tick := _Tick }, 
                                          args = #{ type := doom_fire, doom_fire := Fire} = Args}, _Context) ->
    %% When there is a tick, do a fire propagation
    Fire1 = m_doom_fire:fire_propagation(Fire),
    Args#{doom_fire := Fire1};
observe_teleview_render(#teleview_render{}, _Context) ->
    undefined.


