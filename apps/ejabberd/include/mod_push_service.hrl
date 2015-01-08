-record(push_token, {jid :: binary(),
    token :: binary(),
    type :: integer()
}).

-record(push_message, {type :: integer(),
    content :: binary(),
    from :: binary(),
    to :: binary()
}).