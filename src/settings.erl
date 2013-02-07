-module(settings).

-export([version/0, bname/0, nick/0, channel/0]).
-export([server/0, whois/0, admin/0, cmd_string/0]).

version() -> "1.2".
bname() -> "tachikoma[bot]".

nick() -> "tachikoma[bot]".
channel() -> "#hackerforum".
server() -> {"irc.freenode.net", 6667}.
whois() -> "Erlang bot, written by Flame_Alchemist".

admin() -> ["Flame_Alchemist"].

cmd_string() -> "!bot".
