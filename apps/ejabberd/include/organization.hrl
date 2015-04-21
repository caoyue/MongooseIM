%% ==============================
%% organization
%%
%% ===============================


-record(node, {
    id :: integer() | binary()|undefined,
    name :: binary(),
    lft :: binary() | integer(),
    rgt :: binary() | integer(),
    depth :: binary() | integer(),
    description :: binary(),
    project :: binary() | integer()
}).

-record(employee, {
    jid :: binary(),
    organization_id :: integer(),
    organization_name :: binary()
}).

-record(project, {
    id :: integer()|binary()|undefined,
    name :: binary(),
    description :: binary(),
    status :: integer(),
    start_at :: binary(),
    end_at :: binary()|undefined
}).