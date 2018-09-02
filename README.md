jsonrpc - JSON RPC client
=========================

Options
-------

Name|Type|Default|Description
----|----|-------|-----------
ip|inet:ip_address()||IP used to connect to
port|inet:port_number()||Port to connect to
uri|binary()|<<"/json_rpc">>|URI used for requests
response_timeout|non_neg_integer()|5000|How long wait response from server in milliseconds
connect_timeout|non_neg_integer()|5000|Connection timeout in milliseconds
send_timeout|non_neg_integer()|500|TCP send timeout in milliseconds
retry|pos_integer()|5|Number of times client will try to reconnect on failure
retry_timeout|non_neg_integer()|5000|Time between retries in milliseconds
keepalive|non_neg_integer() or infinity|infinity|Time between pings in milliseconds

How to use
----------

```
{ok, Pid} = jsonrpc_client:start_link(#{ip => "localhost", port => 7341}).

jsonrpc:make_req(Pid, f_blocks_list_json, #{height => 7}).
```