ercbot
--------------------

__E__xtensible __Er__lang I __RC BOT__ (overlapping acronym).
This is my first ever Erlang project.

Usage
--------------------
Compile the source with rebar 

    $ rebar compile
    $ erl -pa ebin
    1> application:start(bot).

Configuration
--------------------
The configuration is done in two ways. You can either:

1. start the application and configure it with `conf_server:set_X`
2. Edit the `settings.cfg` file, and then start the application.

Plugins
--------------------
Since this is an __extensible__ bot, you would expect to be able to write plugins. Indeed.
A plugin is a simple erlang module, with a `gen_event` behaviour.

1. create a `plugin_name.erl` module, with a `gen_event` behaviour. The `init\1` function takes no arguments.
2. add two functions: `name/0` and `short_description/0`, which return the text to be displayed in when a user issues the help.
3. write the `handle_event({cmd, Nick, Command, Args}, State)` function. This function gets called every time your bot encounters a !bot command, so you should do pattern matching on Command. For example, if you were to write a "time" plugin, your handle event could look something like this:
`handle_event({cmd, _, "time", _}, State) -> ...`
all the other functions, like `handle_call` or `terminate` can be empty.
4. to write a response, you can use the function `send_priv_msg(Message)` defined in `plugin_api.erl`. In this module, there are all the function which are safe to use for your plugin.
5. add the plugin to the list in `settings.cfg`, using whichever method you prefer.
6. restart the whole application (or simply write in IRC, where your bot can listen, `!bot admin reload`).

Issues
--------------------
Things to change, improve...

1. right now, if the chosen nickname is already taken, the bot simply hang.
2. it's not very stable; but since it has a supervisor, in the event of crash, it should simply restart the offended part. 

If you have suggestions or bug report, send me an e-mail or open an issue here on github.

