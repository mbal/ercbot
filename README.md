ercbot
======

an extensible Erlang IRC bot.

This is my first Erlang project ever.

1. Edit settings.erl to your needs. All that can be customized is in this file.
2. To write a plugin, all you need is:
   a. a file, plugins/plugin_name.erl, with a gen_event behaviour. The init function takes a argument, which is the Pid of the Bot. You'll use it to answer to users.
   b. two functions, name() and short_description(), which are used when !bot help is issued.
   c. now, write the handle_event(Event, State). This should contain all the logic for your plugin
      Event is a tuple in the form {cmd, Nick, Command, Args}; you should do pattern matching on Command.
   d. the response is written with the function irc_bot_api:send_priv_msg(BotPid, Message), where BotPid is the argument passed to the init/1 function.
   e. add the atom plugin_name to the list in settings:plugins(). 
   f. restart the bot and try your new plugin!
