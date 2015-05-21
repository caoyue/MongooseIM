%% ==============================
%% organization
%%
%% ===============================

-record(group, {
    id :: binary() | integer(),
    name :: binary(),
    owner :: binary()|ejabberd:jid(),
    type :: binary() | integer(),
    status :: binary() | integer(),
    project :: binary() | integer(),
    private::binary()
}).

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
    admin :: binary(),
    start_at :: binary(),
    end_at :: binary()|undefined
}).