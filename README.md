## Erlvue: A Modern Take on Erlang Webtools

### Setting

```erlang
{erlvue, [
  {ip, "127.0.0.1"},
  {port, 9081},
  {ssl, false},
  {acceptors, 5},
  {dispatch_file, "priv/dispatch.script"},
  {wamp_uri_file, "priv/wamp_uri.script"}
]}
```

### Running Demo

```bash
./start-dev.sh
```
