%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : backend.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2012 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(backend).
-behaviour(gen_server).
-include("backend.hrl").

-export([start/0, start_link/0, stop/0,
         account/1, pin_valid/2, change_pin/3,
         balance/2, transactions/2,
         withdraw/3, deposit/2, transfer/4,
         init/1, handle_call/3, terminate/2, 
         code_change/3, handle_cast/2, handle_info/2,
         block/1]).

-define(DB, db_list).
-define(ACCOUNTS,
        [{1, 100, "1234", "Henry Nystrom"},
         {2, 200, "4321", "Francesco Cesarini"},
         {3, 1000, "1111", "Donald Duck"},
         {4, 5000, "1234", "Henry Nystrom"}
        ]).

-record(state, {accounts, blocked = []}).

start() -> start_link().

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

stop() -> gen_server:call(?MODULE, stop).

terminate(_Reason, _State) -> ok.

code_change(_A, _B, _C) -> notImplemented.

handle_cast(_A, _B) -> notImplemented.

handle_info(_A, _B) -> notImplemented.

account(Account) -> 
  gen_server:call(?MODULE, {account, Account}).

pin_valid(AccountNo, Input) ->
  gen_server:call(?MODULE, {pin_valid, AccountNo, Input}).

block(AccountNo) ->
  gen_server:call(?MODULE, {block, AccountNo}).

change_pin(User, OldPin, NewPin) ->
  gen_server:call(?MODULE, {change_pin, User, OldPin, NewPin}).

withdraw(AccountNo, Pin, Amount) ->
  gen_server:call(?MODULE, {withdraw, AccountNo, Pin, Amount}).

deposit(AccountNo, Amount) ->
  gen_server:call(?MODULE, {deposit, AccountNo, Amount}).

transfer(Amount, From, To, Pin) ->
  gen_server:call(?MODULE, {transfer, From, To, Pin, Amount}).

balance(AccountNo, Pin) ->
  gen_server:call(?MODULE, {balance, AccountNo, Pin}).

transactions(AccountNo, Pin) ->
  gen_server:call(?MODULE, {transactions, AccountNo, Pin}).

init(no_args) ->
  process_flag(trap_exit, true),
  Accounts = lists:foldl(fun({No, Balance, Pin, Name}, DB) ->
	  ?DB:insert(do_new_account(No, Balance, Pin, Name), DB) end, 
    ?DB:empty(), ?ACCOUNTS),
  {ok, #state{accounts = Accounts}}.


handle_call({account, Accounts}, _, State) ->
  Reply =
  case Accounts of
    all ->
      lists:map(fun(#account{no = No, name = Name}) -> {No, Name} end,
          ?DB:db_to_list(State#state.accounts));
    Name when is_list(Name) -> find_account(Name, State);
    No when is_integer(No) -> [find_account(No, State)]
  end,
  {reply, Reply, State};

handle_call({pin_valid, AccountNo, Pin}, _, State) ->
  Account = find_account(AccountNo, State),
  {reply, do_pin_valid(Account, Pin), State};

handle_call({balance, AccountNo, Pin}, _, State) ->
  case lists:member(AccountNo, State#state.blocked) of
    true -> {reply, {error, blocked}, State};
    false ->  {reply, do_balance(AccountNo, Pin, State), State}
  end;

handle_call({block, AccountNo}, _, State) ->
  NewState = do_block(AccountNo, State),
  {reply, ok, NewState};

handle_call({transactions, AccountNo, Pin}, _, State) ->
  {reply, do_transactions(AccountNo, Pin, State), State};

handle_call({withdraw, AccountNo, Pin, Amount}, _, State) ->
  case lists:member(AccountNo, State#state.blocked) of
    true -> {reply, {error, blocked}, State};
    false ->    
      case do_withdraw(AccountNo, Pin, Amount, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
      end
  end;

handle_call({deposit, AccountNo, Amount}, _, State) ->
  case lists:member(AccountNo, State#state.blocked) of
    true -> {reply, {error, blocked}, State};
    false ->
      case do_deposit(AccountNo, Amount, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
      end
  end;

handle_call({transfer, FromAccountNo, ToAccountNo, Pin, Amount}, _, State) ->
  case lists:member(FromAccountNo, State#state.blocked) or lists:member(ToAccountNo, State#state.blocked) of
    true -> {reply, {error, blocked}, State};
    false ->
      case do_transfer(FromAccountNo, ToAccountNo, Pin, Amount, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
      end
  end; 

handle_call({change_pin, User, OldPin, NewPin}, _, State) ->
  case do_change_pin(User, OldPin, NewPin, State) of
    {ok, NewState} -> {reply, ok, NewState};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;

handle_call(stop, _, State) ->
   {reply, stop, State}.

do_new_account(No, Balance, Pin, Name) ->
  #account{no = No, balance = Balance, pin = Pin, name = Name}.

find_account(AccountNo, State) when is_integer(AccountNo) ->
  ?DB:lookup(AccountNo, State#state.accounts);
find_account(User, State) when is_list(User) ->
  ?DB:lookup_all(#account.name, User, State#state.accounts).

do_withdraw(_, _, Amount, _) when Amount < 0 -> {error, "Negative value"};
do_withdraw(AccountNo, Pin, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountNo, State),
  case do_pin_valid(Account, Pin) of
    false -> {error, "PIN code not valid!"};
    true when OldBalance < Amount -> {error, "Not enough money on account!"};
    true ->
      NewBalance = OldBalance - Amount,
      NewTransactions = [{withdraw, date(), Amount} | OldTransactions],
      AccountUpdated =
	Account#account{balance = NewBalance, transactions = NewTransactions},
      NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
      {ok, State#state{accounts = NewAccounts}}
  end.

do_deposit(AccountNo, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountNo, State),
  NewBalance = OldBalance + Amount,
  NewTransactions = [{deposit, date(), Amount} | OldTransactions],
  AccountUpdated =
    Account#account{balance = NewBalance, transactions = NewTransactions},
  NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
  {ok, State#state{accounts = NewAccounts}}.

do_balance(AccountNo, Pin, State) ->
  Account = find_account(AccountNo, State),
  case do_pin_valid(Account, Pin) of
    true -> Account#account.balance;
    false -> {error, "PIN code not valid!"}
  end.

do_block(AccountN, State) -> State#state{blocked = [AccountN | State#state.blocked]}.
	
do_transactions(AccountNo, Pin, State) ->
  Account = find_account(AccountNo, State),
  case do_pin_valid(Account, Pin) of
    true -> Account#account.transactions;
    false -> {error, "PIN code not valid!"}
  end.

do_transfer(FromAccountNo, ToAccountNo, Pin, Amount, State) ->
  case do_withdraw(FromAccountNo, Pin, Amount, State) of
    {ok, NewState} -> do_deposit(ToAccountNo, Amount, NewState);
    {error, Reason} -> {error, Reason}
  end.

do_pin_valid([], _) -> false;
do_pin_valid([Account | _], Pin) -> Account#account.pin == Pin;
do_pin_valid(Account, Pin) -> Account#account.pin == Pin.

do_change_pin(User, OldPin, NewPin, State) ->
  Accounts = find_account(User, State),
  case do_pin_valid(Accounts, OldPin) of
    false -> {error, "Wrong Pin"};
    true ->
      Accounts1 =
        lists:foldl(fun(Account, Acc) ->
                        ?DB:update(Account#account{pin = NewPin}, Acc)
                    end,
                    State#state.accounts,
                    Accounts),
      {ok, State#state{accounts = Accounts1}}
  end.