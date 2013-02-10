ercbot
======

an Extensible ERlang iRC BOT (yes, the acronym is overlapping, also, my first Erlang project ever).

Customizations
--------------
Edit settings.erl to fit your needs. All that can be customized is in this file.

Plugins
--------------
To write a plugin, all you need is:


1. a file, `plugins/plugin_name.erl`, with a `gen_event` behaviour. The init function takes a argument, which is the Pid of the Bot. You'll use it to answer to users.
2. two functions, `name()` and `short_description()`, which are used when !bot help is issued.
3. now, write the `handle_event(Event, State)`. This should contain all the logic for your plugin. Event is a tuple in the form `{cmd, Nick, Command, Args}`; you should do pattern matching on Command.
4. the response is written with the function `irc_bot_api:send_priv_msg(BotPid, Message)`, where BotPid is the argument passed to the init/1 function.
5. add the atom plugin_name to the list in `settings:plugins()`. 
6. restart the bot and try your new plugin!
   
   
The application also provide an ets table to store state, `state_storage`. The key should be `?MODULE`. You can use it to store your plugin's state in the terminate function, in order to restore it when the plugin crashes.

Problems
--------------
Right now, this application has some problem:

1. if the choosen nick is not available, the whole application will simply hang.
2. many others which right now I don't know of. Testing needed.
