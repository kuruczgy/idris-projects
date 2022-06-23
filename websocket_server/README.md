# A websocket server
This project interfaces `libwebsockets` and `libpq` with the `refc` backend of
Idris.

## Building
- `IDRIS2_CC='cc -g -lwebsockets -lpq -Isrc' idris2 --build`

## Running
- Start a PostgreSQL server, for example with something like this: `podman run
  -e POSTGRES_PASSWORD=asdfg -p 5432:5432 docker.io/postgres`
- Run the server with `./build/exec/main`
- Launch `test.html` in your browser. It will connect to the websocket server,
  and send a `ReqEcho` request. The idris server should print a debug message
  indicating an established connection, while in the browser console you should
  see the `ReplyEcho` message printed.
