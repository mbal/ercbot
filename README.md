ercbot
--------------------

__E__xtensible __Er__lang I __RC BOT__ (overlapping acronym).
This is my first ever Erlang project.

Configuration
--------------------

To configure the application, you must edit the `settings.cfg` file, placed in your `ercbot\src` directory. You can move it wherever you prefer, then use the file path when you start the application, like showed below.

Usage
--------------------
Compile the source with rebar 

    $ rebar compile
    ==> ercbot (compile)
    ...
    $ erl -pa ebin
    Eshell V5.9.3.1 (abort with ^G)
    1> bot:start("path/to/settings.cfg").
    [debug>]bot_sup starting...
    ...

There are other two, equivalent, ways to start the application

    $ erl -pa ebin -bot settings_file '"path/to/settings.cfg"'
    Eshell V5.9.3.1 (abort with ^G)
    1> bot:start().
    ...

or, you can specify the value of settings_file through a .config,
    
    $ cat file.config
    [{bot, [{settings_file, "path/to/settings.cfg"}]}].
    
    $ erl -pa ebin -config file
    Eshell V.5.9.3.1 (abort with ^G)
    1> bot:start().
    ...


Plugins
--------------------
Since this is an __extensible__ bot, you would expect to be able to write plugins. Indeed.

A plugin is a simple erlang module, with a `gen_event` behaviour.

1. create a `plugin_name.erl` module, with a `gen_event` behaviour. The `init\1` function takes no arguments.
2. add two functions `name/0` and `short_description/0`, which return the text to be displayed in when a user issues the help.
3. write the `handle_event({cmd, Nick, Command, Args}, State)` function. This function gets called every time your bot encounters a !bot command, so you should do pattern matching on Command. For example, if you were to write a "time" plugin, your handle event could look something like this:
`handle_event({cmd, _, "time", _}, State) -> ...`
all the other functions, like `handle_call` or `terminate` can be empty.
4. to write a response, you can use the function `send_priv_msg(Message)` defined in `plugin_api.erl`. In this module, there are all the function which are safe to use for your plugin.
5. add the plugin to the list in `settings.cfg`. In the erl command line, write: `conf_server:reload_config()`.
6. restart the whole application (or simply write in IRC, where your bot can listen, `!bot admin reload`.

Architecture
--------------------

Application's architecture, in a powerful ASCII-gram.


                             +-------------+
                        +----| conf_server |
                        |    +-------------+
                        |
    +------------+      |    +------------+
    |  bot_sup   |------+----|  bot_fsm   |
    +------------+      |    +------------+
                        |
                        |    +---------------+
                        +----| irc_connector |          +---------+
                        |    +---------------+      +---| plugin1 |
                        |                           |   +---------+
                        |    +------------+         |   +---------+
                        +----| plugin_mgr |---------+---| plugin2 |
                             +------------+         |   +---------+
                                                    |   +---------+
                                                    +---| plugin3 |
                                                        +---------+
                                                        
The application is quite easy to understand. When the supervisor starts, it spawns two process: first the `conf_server`, and then the `bot_fsm`. The latter starts, on the behalf of the supervisor, the `irc_connector`, which is where the real communication happens, and the `plugin_mgr`, which is mainly a dispatcher.
When the `plugin_mgr` starts, it starts all the plugins listed in `settings.cfg`. Then the `plugin_mgr` sends them the !bot (or whatever you defined in `settings.cfg`) command.

The plugins have a simple `gen_event` behaviour, and each one send its response back, using the `plugin_api` module. Of course, most of the times only a plugin each command will write its response, though it's not certain.
                                                        
Issues
--------------------

Right now it's not very stable, however, thanks to the supervisor, in the event of a crash, only the offended part should restart.

If you have suggestions or bug report, send me an e-mail or open an issue here on github.

