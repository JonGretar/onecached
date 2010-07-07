-module(onecached_storage).
-author('jongretar@jongretar.com').
-behaviour(gen_server).

-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1,
	store_item/1,
	has_item/1,
	get_item/1,
	delete_item/1,
	update_item_value/3,
	flush_items/0]).

%% The State
-record (state, {module, module_state}).

%% Behaviour Part
behaviour_info(callbacks) ->
	[
		{init,1},
		{store_item,2},
		{has_item,2},
		{get_item,2},
		{delete_item,2},
		{update_item_value,4},
		{flush_items,1}
	];
behaviour_info(_Other) ->
	undefined.

%% gen_server part


% API
start_link(Module) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Module], []).
store_item(Item) -> gen_server:call(?MODULE, {store_item, Item}).
has_item(Item) -> gen_server:call(?MODULE, {has_item, Item}).
get_item(Item) -> gen_server:call(?MODULE, {get_item, Item}).
delete_item(Item) -> gen_server:call(?MODULE, {delete_item, Item}).
update_item_value(Key, Value, Operation) -> gen_server:call(?MODULE, {update_item_value, Key, Value, Operation}).
flush_items() -> gen_server:call(?MODULE, {flush_items}).


% Server implementation, a.k.a.: callbacks
 
init([Module]) ->
	ModuleState = Module:init([]), %% Add the passing of configuration over.
	{ok, #state{module=Module, module_state=ModuleState}}.

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(state, _From, State) ->
	{reply, State, State};

%
handle_call({flush_items}, _From, State) ->
	Module = State#state.module,
	Reply = Module:flush_items(State#state.module_state),
	{reply, Reply, State};
handle_call({update_item_value, Key, Value, Operation}, _From, State) ->
	Module = State#state.module,
	Reply = Module:update_item_value(State#state.module_state, Key, Value, Operation),
	{reply, Reply, State};
handle_call({Command, Item}, _From, State) ->
	Module = State#state.module,
	Reply = Module:Command(State#state.module_state, Item),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Helpers

