# jsonrpc - JSON-RPC library


### JSON-RPC HTTP client

#### Options

Name|Type|Default|Description
----|----|-------|-----------
ip|string()||IP used to connect to
port|inet:port_number()||Port to connect to
uri|binary()|<<"/json_rpc">>|URI used for requests
response_timeout|non_neg_integer()|5000|How long wait response from server in milliseconds
connect_timeout|non_neg_integer()|5000|Connection timeout in milliseconds
send_timeout|non_neg_integer()|500|TCP send timeout in milliseconds
retry|pos_integer()|5|Number of times client will try to reconnect on failure
retry_timeout|non_neg_integer()|5000|Time between retries in milliseconds
keepalive|non_neg_integer() or infinity|infinity|Time between pings in milliseconds

#### How to use

```
{ok, Pid} = jsonrpc_http_client:start_link(#{ip => "localhost", port => 8545}).

jsonrpc:make_req(Pid, eth_getBalance, [<<"0xd3CdA913deB6f67967B99D67aCDFa1712C293601">>, latest]).
{ok,#{id => 7,jsonrpc => <<"2.0">>, result => <<"0x18d809bef21b94d4">>}}

jsonrpc:make_req(Pid, eth_blockNumber, []).
{ok,#{id => 2,jsonrpc => <<"2.0">>,result => <<"0xd6c8b4">>}}

5> jsonrpc:make_req(Pid, eth_chainId, []).
{ok,#{id => 3,jsonrpc => <<"2.0">>,result => <<"0x1">>}}
```

### JSON-RPC TCP client

#### Options

Name|Type|Default|Description
----|----|-------|-----------
ip|inet:ip_address()||IP used to connect to
port|inet:port_number()||Port to connect to
response_timeout|non_neg_integer()|5000|How long wait response from server in milliseconds
send_timeout|non_neg_integer()|500|TCP send timeout in milliseconds
retry_timeout|non_neg_integer()|5000|Time between retries in milliseconds
oneshot|boolean()|false|Connect - Send - Receive - Close

#### How to use

```
{ok, Pid} = jsonrpc_tcp_client:start_link(#{ip => "localhost", port => 3333}).

jsonrpc:make_req(Pid, miner_getstat1, #{}).
```
