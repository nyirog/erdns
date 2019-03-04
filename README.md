erdns
=====

erdns is a simple json-rpc service which resolves the dns name of an ip with
inet_res:resolve(_IP_, in, ptr) function.

The service is listening on the 8080 port by default.

Build
-----

    $ rebar3 compile

Usage
-----

    $ rebar3 shell

    $ jq < command.json
    {
        "jsonrpc": "2.0",
        "method": "rdns",
        "params": ["79.172.213.243"],
        "id": 1
    }
    $ curl http://localhost:8080/ -d @command.json -H 'content-type: application/json' | jq
    {
        "jsonrpc": "2.0",
        "result": {
            "ttl": 433,
            "address": "manis-1.mgxcdn.magex.hu"
        },
        "id": 1
    }

    $ jq < command_with_nameservers.json
    {
        "jsonrpc": "2.0",
        "method": "rdns",
        "params": ["79.172.213.243", ["192.168.0.1"]],
        "id": 2
    }
    $ curl http://localhost:8080/ -d @command_with_nameservers.json -H 'content-type: application/json' | jq
    {
        "jsonrpc": "2.0",
        "result": {
            "ttl": 3507,
            "address": "manis-1.mgxcdn.magex.hu"
        },
        "id": 2
    }
