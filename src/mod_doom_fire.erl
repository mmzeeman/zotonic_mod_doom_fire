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
observe_teleview_state_init({teleview_state_init, #{ type := doom_fire, tick := Tick }=Args}, Context) ->
    trigger_tick(Tick, Context),
    %% Get the initial state of the fire.

    undefined;
observe_teleview_state_init({teleview_state_init, #{ }=A}, _Context) ->
    undefined.

%% Observe render trigger of the doom_fire teleview
observe_teleview_render({teleview_render, Id, Msg, #{ type := doom_fire, tick := Tick }}, Context) ->
    trigger_tick(Tick, Context),
    undefined;
observe_teleview_render({teleview_render, _Id, _Msg, #{}}, _Context) ->
    undefined.


observe_acl_is_allowed(#acl_is_allowed{action=subscribe,
                                      object=#acl_mqtt{topic = [<<"model">>, <<"doom_fire">>, <<"event">> | SessionTopic ]}}, Context) ->
    is_event_subscribe_allowed(SessionTopic, Context);
observe_acl_is_allowed(_E, _Context) ->
    undefined.

%%
%% Helpers
%%

trigger_tick(After, Context) ->
    timer:apply_after(After, ?MODULE, tick, [After, Context]).

tick(After, Context) ->
    ?DEBUG(tick),
    %% Publish a doom_fire event tick. This will trigger a new render.
    z_mqtt:publish([model, doom_fire, event, tick], #{}, z_acl:sudo(Context)).

% Everybody is allowed to subscribe to ticks
is_event_subscribe_allowed([<<"tick">>|_], _Context) -> true;
is_event_subscribe_allowed(_, _Context) -> false.


