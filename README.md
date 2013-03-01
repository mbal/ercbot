ercbot
--------------------

__E__xtensible __Er__lang I __RC BOT__ (overlapping acronym).
This is my first ever Erlang project.

Features
--------------------
This small project is my attempt to learn Erlang and create a bot for
the IRC channels I am on.

The bot is extensible, as its name suggests, by writing simple Erlang
module. It's possible to use it on multiple channels. 
It's slowly becoming stable, however, if there's an issue, or a
feature request, you can send it to the github's issue tracker.

Configuration
--------------------

To configure the application, you must edit the `settings.cfg` file,
placed in your `ercbot` directory. You can move it wherever you
prefer, then use the file path when you start the application, like
showed below.

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
Since this is an __extensible__ bot, you would expect to be able to
write plugins. Indeed.
See here: https://github.com/mbal/ercbot/wiki/Plugins

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
                        |    +------------+             +---------+
                        |
                        |    +---------------+          +---------+
                        +----| plugin_superv |----------| plugins |
                             +---------------+          +---------+ 
                                                        
The application is quite easy to understand. When the supervisor
starts, it spawns two process: first the `conf_server`, and then the
`bot_fsm`. The latter starts, on the behalf of the supervisor, the
`irc_connector`, which is where the real communication happens, and
the `plugin_mgr`, which is mainly a dispatcher.
When the `plugin_mgr` starts, it starts all the plugins listed in
`settings.cfg`. The `plugin_mgr` sends to the plugins all the message
it receives.
`plugin_superv` is a simple supervisor for plugins that need to be 
`gen_server`s.

The plugins have a simple `gen_event` behaviour, and each one send its
response back, using the `irc_api` module. Of course, most of the
times only a plugin each command will write its response, though it's
not certain.
                                                        
Issues
--------------------

Right now it's not very stable, however, thanks to the supervisor, in
the event of a crash, only the offended part should restart.

If you have suggestions or bug report, send me an e-mail or open an
issue here on github.

