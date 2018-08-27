# elm-phoenix
Testbed for elm phoenix channel implementation.

Not much to see here. Look at the files.
* [Simple websocket api](websocket-ports.js)
* [Package file](src/Phoenix.elm)
* [Test main file, change `phoenixUrl` and `phoenixChannels`](src/Main.elm)

Run with `make dev` to see what happens.

* Push not implemented yet, you can only subscribe to channels and get messages from others right now. Still thinking about how to do the push stuff in a good way.
