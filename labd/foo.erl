-module(foo).
-compile(export_all).
foo() -> io:format(user,"hello",[]).
