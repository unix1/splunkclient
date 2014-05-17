-module(splunkclient_http_handler).

-callback init(Args :: list(term())) ->
    'ok'|tuple('error', Reason :: string()).

-callback send_request(Method :: 'get'|'post',
                       Uri :: string(),
                       Body :: string(),
                       Headers :: list(term()),
                       Type :: string()) ->
    tuple('ok', ResponseBody :: string() | tuple('error', ErrorReason :: string())).
