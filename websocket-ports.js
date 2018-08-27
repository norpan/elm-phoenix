// LOWLEVEL websocket stuff in javascript

function websocketBindPorts(ports) {
    // PORTS

    ports.webSocketOpen.subscribe(open); // String -> Cmd msg
    ports.webSocketSend.subscribe(send); // (Value, Value) -> Cmd msg
    ports.webSocketClose.subscribe(close); // (Value, Int, String) -> Cmd msg


    //COMMANDS

    function open(url) {
        const ws = new WebSocket(url);
        ws.onclose = sendToElm('webSocketOnClose');
        ws.onerror = sendToElm('webSocketOnError');
        ws.onmessage = sendToElm('webSocketOnMessage');
        ws.onopen = sendToElm('webSocketOnOpen');

        function sendToElm(port) {
            return function (event) {
                if (ports[port] && ports[port].send) {
                    ports[port].send(event)
                }
            }
        }
    }


    function send(param) {
        const ws = param[0];
        const data = param[1];

        ws.send(JSON.stringify(data));
    }

    function close(param) {
        const ws = param[0];
        const code = param[1];
        const reason = param[2];

        ws.close(ws, code, reason);
    }
}