:- initialization(main, main).

main([Port]) :-
    ansi_format([fg(blue)], 'Started server at ', []),
    ansi_format([bold, fg(blue)], 'http://localhost:~w/~n', [Port]),
    ansi_format([fg(blue)], 'Go to ', []),
    ansi_format([bold, fg(blue)], 'http://localhost:~w/openapi', [Port]),
    ansi_format([fg(blue)], ' for a web-based REPL~n', []).