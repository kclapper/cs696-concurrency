%%% Template generated using Emacs Erlang Mode

-module(tic_tac_toe).

-behaviour(gen_server).

%% API
-export([start_link/0, make_move/1, join_game/0, leave_game/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
    board = {blank, blank, blank, blank, blank, blank, blank, blank, blank},
    xs,
    os,
    next = xs
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call(Request, From, State) ->
    FromPID = element(1, From),
    case Request of
        join -> handle_join(FromPID, State);
        leave -> handle_leave(FromPID, State);
        {move, Space} -> handle_move(Space, FromPID, State)
    end.

handle_join(From, State) ->
    case {State#state.xs, State#state.os} of
        {undefined, _} ->
            {reply, xs, State#state{xs=From}};

        {_, undefined} ->
            {reply, os, State#state{os=From}};

        _Else -> {reply, game_full, State}
    end.

join_game() ->
    gen_server:call(tic_tac_toe, join).

handle_leave(From, State) ->
    case {State#state.xs, State#state.os} of
        {From, undefined} ->
            {reply, ok, #state{}};

        {undefined, From} ->
            {reply, ok, #state{}};

        {From, _} ->
            State#state.os ! partner_left,
            {reply, ok, #state{}};

        {_, From} ->
            State#state.xs ! partner_left,
            {reply, ok, #state{}};

        _Else -> {reply, ok, State}
    end.

leave_game() ->
    gen_server:call(tic_tac_toe, leave).

handle_move(Space, From, State) ->
    Player = State#state.next,
    PlayerPID = get_player_pid(Player, State),
    CorrectPlayer = From == PlayerPID,

    OtherPlayer = case Player of xs -> os; os -> xs end,
    OtherPlayerPID = get_player_pid(OtherPlayer, State),

    {MarkResult, Board} = mark_board(Player, Space, State#state.board),

    IsWin = is_win(Player, Board),
    IsFull = is_full(Board),

    case {CorrectPlayer, MarkResult, IsWin, IsFull} of
        {false, _, _, _} ->
            {reply, wrong_player, State};

        {true, taken, _, _} ->
            {reply, taken, State};

        {true, ok, true, _} ->
            Reply = {win, Player, Board},
            OtherPlayerPID ! Reply,
            {reply, Reply, #state{}};

        {true, ok, false, true} ->
            Reply = {tie, Board},
            OtherPlayerPID ! Reply,
            {reply, Reply, #state{}};

        {true, ok, false, false} ->
            NextPlayer = case Player of xs -> os; os -> xs end,
            NextPlayerPID = get_player_pid(NextPlayer, State),
            NewState = State#state{
                         next = NextPlayer,
                         board = Board
                        },
            NextPlayerPID ! {next, Board},
            {reply, ok, NewState}
    end.

get_player_pid(Mark, State) ->
    case Mark of
        xs -> State#state.xs;
        os -> State#state.os
    end.

mark_board(Player, Space, Board) ->
    Position = element(Space, Board),
    case Position of
        blank -> {ok, setelement(Space, Board, Player)};
        _Else -> {taken, Board}
    end.

is_full(Board) ->
    lists:foldl(
      fun (Space, Acc) ->
              Acc andalso Space /= blank
      end,
      true,
      tuple_to_list(Board)
     ).

is_win(Player, Board) ->
    case Board of
        {Player, Player, Player, _, _, _, _, _, _} -> true;
        {_, _, _, Player, Player, Player, _, _, _} -> true;
        {_, _, _, _, _, _, Player, Player, Player} -> true;

        {Player, _, _, Player, _, _, Player, _, _} -> true;
        {_, Player, _, _, Player, _, _, Player, _} -> true;
        {_, _, Player, _, _, Player, _, _, Player} -> true;

        {Player, _, _, _, Player, _, _, _, Player} -> true;
        {_, _, Player, _, Player, _, Player, _, _} -> true;

        _Else -> false
    end.

make_move(Space) ->
    gen_server:call(tic_tac_toe, {move, Space}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
