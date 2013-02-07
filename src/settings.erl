-module(settings).

-export([version/0, bname/0, nick/0, channel/0]).
-export([server/0, whois/0, admin/0, cmd_string/0]).

version() -> "1.2".
bname() -> "ercbot".

nick() -> "name[bot]".
channel() -> "#channel".
server() -> {"irc.freenode.net", 6667}.
whois() -> "Erlang bot, written by mbal".

admin() -> ["admin"].

cmd_string() -> "!bot".
