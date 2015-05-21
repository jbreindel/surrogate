-module(surrogate_db).
-export([init/0, migrate/0, rebuild/0, recreate_table/1]).

-define(APPNAME, surrogate).
-define(MODELS, [list_to_atom(M) || M <- boss_files:model_list(?APPNAME)]).
-define(NODES, [node()]).

init() ->
    mnesia:stop(),
    mnesia:create_schema(?NODES),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:start(),
    ExistingTables = mnesia:system_info(tables),
    TablesToCreate = (?MODELS ++ ['_ids_']) -- ExistingTables,
    [create_table(T) || T <- TablesToCreate],
    {ok, []}.

create_table('_ids_') ->
    create_table(?NODES, '_ids_', [type, id, count]);
create_table(Model) ->
    io:format("Installing table ~p~n",[Model]),
    DummyRecord = boss_record_lib:dummy_record(Model),
    Attributes = DummyRecord:attribute_names(),
    create_table(?NODES, Model, Attributes).

create_table(Nodes, Table, Attributes) ->
    mnesia:create_table(Table, [{attributes, Attributes},
                               {disc_copies, Nodes}]).

migrate() ->
    mnesia:stop(),
    init().

rebuild() ->
    mnesia:stop(),
    mnesia:delete_schema(?NODES),
    init().

recreate_table(Model) ->
    mnesia:delete_table(Model),
    create_table(Model).