$.fn.consolize = function() {
	var terminal = this.terminal(handleCommand),
		socket   = null,
		host     = 'ws://127.0.0.1:8080';

	// --- socket stuff

	function socketInit() {
		socket = new WebSocket(host);

		socket.onopen = function() {
			console.debug('socket open', arguments);
		}

		socket.onclose = function() {
			console.debug('socket closed', arguments);
		}

		socket.onmessage = function(msg) {
			var result = JSON.parse(msg.data)
			if('error' == result.topic) {
				printError(result.data);
			} else if('ghci_output' == result.topic) {
				terminal.echo(clearOutputFromGHCI(result.data));
			} else if('inotify' == result.topic) {
				printInfo(JSON.stringify(result.data));
			} else {
				printDebug(msg.data);
			}
		}
	}
	function socketSend(data) {
		socket.send(data);
	}

	// --- command stuff

	function handleCommand(cmd, term) {
		var cmd_split = cmd.split(" ");
		if(cmd_split[0].toLowerCase() == 'wssend') {
			debugger
			socketSend(JSON.stringify({topic: cmd_split[1], data: cmd_split[2]}));
			return;
		}
		ghciExecuteCommand(cmd);
	}

	function ghciExecuteCommand(cmd) {
		socketSend(JSON.stringify({topic:'ghci_input', data: cmd+'\n'}));
	}

	// --- utils

	function clearOutputFromGHCI(str) {
		var filterOut = ['\u001b[?1h\u001b=','\u001b[?1l\u001b>']
		for(var i in filterOut)
			str = str.replace(filterOut[i],'')
		return str
	}

	function printError(str) {
		terminal.echo('[[;red;]'+str+']')
	}

	function printInfo(str) {
		terminal.echo('[[;purple;]'+str+']')
	}

	function printDebug(str) {
		terminal.echo('[[;cyan;]'+str+']')
	}

	// --- initialize everything

	socketInit();
}

$(document).ready(function() {
	$('#terminal').consolize()
})
