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
			if('ghci_output' == result.topic) {
				terminal.echo(clearOutputFromGHCI(result.data))
			} else if('inotify' == result.topic) {
				printInfo(JSON.stringify(result.data))
			}
		}
	}
	function socketSend(data) {
		socket.send(data);
	}

	// --- command stuff

	function handleCommand(cmd, term) {
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

	function printInfo(str) {
		terminal.echo('[[;purple;]'+str+']')
	}

	// --- initialize everything

	socketInit();
}

$(document).ready(function() {
	$('#terminal').consolize()
})
