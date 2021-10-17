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

-export([
    new/2,
    fire_propagation/1,
    add_fire_source/1,

    split_in_rows/2
]).

-record(doom_fire, {
    width=60  :: non_neg_integer(),
    height=40 :: non_neg_integer(),
    pixels    :: array:array()
}).

m_get([<<"rows">>, #doom_fire{width=Width, pixels=Pixels} | Rest], _Msg, _Context) ->
    Rows = split_in_rows(array:to_list(Pixels), Width),
    {ok, {Rows, Rest}};

m_get(V, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.

m_post(Topic, _Msg, _Context) ->
    ?DEBUG(Topic),
    ok.

%% Create a new doom fire data structure
new(Width, Height) ->
    #doom_fire{width=Width,
               height=Height,
               pixels=array:new([{fixed, true}, 
                                 {size, Width*Height},
                                 {default, 0}])}.

%% Calculate one step in the fire propagation
fire_propagation(#doom_fire{width=Width, height=Height, pixels=Pixels}=Fire) ->
    NewPixels = array:map(fun(I, Intensity) ->
                                  case I + Width of
                                      Overflow when Overflow >= Width * Height ->
                                          Intensity;
                                      BelowIndex ->
                                          IntensityBelow = array:get(BelowIndex, Pixels),
                                          Decay = rand:uniform(4) - 1,
                                          max(IntensityBelow - Decay, 0)
                                  end
                          end,
                          Pixels),
    Fire#doom_fire{pixels=NewPixels}.

%% Add the fire source.
add_fire_source(#doom_fire{width=Width, height=Height, pixels=Pixels}=Fire) ->
    OverflowIndex = Width * Height,
    StartIndex = OverflowIndex - Width,
    Pixels1 = lists:foldl(
                fun(Index, FirePixels) ->
                        array:set(Index, 36, FirePixels)
                end,
                Pixels,
                lists:seq(StartIndex, OverflowIndex-1)),
    Fire#doom_fire{pixels=Pixels1}.

%%
%% Helpers
%%

split_in_rows(List, Width) ->
    split_in_rows(List, Width, []).

split_in_rows(List, Width, Acc) when length(List) =< Width ->
    lists:reverse([List | Acc]);

split_in_rows(List, Width, Acc) ->
    {Row, Rest} = lists:split(Width, List),
    split_in_rows(Rest, Width, [ Row | Acc]).


rows(Width, Height) ->
    [row(Width) || _N <- lists:seq(1, Height)].

row(Width) ->
    [0 || _N <- lists:seq(1, Width)].

