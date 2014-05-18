-module(splunkclient_http_handler).

-callback init(Protocol :: string(),
               Host :: string(),
               Port :: non_neg_integer()) ->
    tuple('ok', State :: term()) | tuple('error', Reason :: string()).

-callback terminate(Args :: list(term())) ->
    'ok'|tuple('error', Reason :: string()).

-callback send_request(State :: term(),
                       Request :: term()) ->
    tuple('ok', ResponseBody :: string() | tuple('error', ErrorReason :: string())).
