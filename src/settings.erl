-module(settings).

-export([version/0, bname/0, nick/0, channel/0, plugins/0]).
-export([server/0, whois/0, admin/0, cmd_string/0]).

version() -> "1.3".
bname() -> "ercbot".

nick() -> "ercbot[bot]".
channel() -> "#channel".
server() -> {"irc.freenode.net", 6667}.
whois() -> "An extensible Erlang bot, written by mbal".

admin() -> ["mbal"].

cmd_string() -> "!bot".

plugins() -> [tell_time, uptime, admin].
