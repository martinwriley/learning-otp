-module(db_list).

-export([empty/0, insert/2, db_to_list/1, lookup/2, lookup_all/3]).
-export([update/2, close/0, db_size/1]).

-record(account, {no, balance=0, pin, name, transactions=[]}).

%%%%%%%%%%%%%%%%%%%%%%%%% Exported Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty() -> [].

insert(A = #account{}, []) -> [A];
insert(#account{no = N}, [#account{no = N} | _Rest]) -> {error, exists};
insert(A = #account{}, [Current | Rest]) -> [Current | insert(A, Rest)].

db_to_list(L) -> L.

lookup(_, []) -> {error, instance};
lookup(N, [A = #account{no=N} | _Rest]) -> A;
lookup(N, [_ | Rest]) -> lookup(N, Rest).

lookup_all(_N, _Key, []) -> [];
lookup_all(N, Key, [Rec|Db]) ->
  if element(N,Rec) =:= Key -> [Rec | lookup_all(N, Key, Db)];
  	element(N,Rec) =/= Key -> lookup_all(N, Key, Db)
  end.

update(A = #account{}, []) -> [A];
update(A = #account{}, [Current|Rest]) ->
  if A#account.no =:= Current#account.no -> [A|Rest];
    A#account.no =/= Current#account.no -> [Current|update(A, Rest)]
  end.

close() -> ok.

db_size(DBRef) -> length(DBRef).

%%%%%%%%%%%%%%%%%%%%%%%%% Private  Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
