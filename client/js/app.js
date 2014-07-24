$.fn.consolize = function(commandParser) {
	var self     = this,
		terminal = self.terminal(handleCommand),
		socket   = null,
		host     = 'ws://127.0.0.1:8080';
	// host     = 'ws://10.0.6.169:8080';

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
				self.printError(result.data);
			} else if('ghci_output' == result.topic) {
				self.print(clearOutputFromGHCI(result.data));
			} else if('inotify' == result.topic) {
				self.printInfo(JSON.stringify(result.data));
			} else {
				self.printDebug(msg.data);
			}
		}
	}

	// --- command stuff

	function handleCommand(cmd, term) {
		var cmd_split = cmd.split(" ");
		if(cmd_split[0].toLowerCase() == 'wssend') {
			debugger
			self.socketSend(JSON.stringify({topic: cmd_split[1], data: cmd_split[2]}));
			return;
		}
		self.ghciExecuteCommand(cmd);
	}

	// --- utils

	function clearOutputFromGHCI(str) {
		var filterOut = ['\u001b[?1h\u001b=','\u001b[?1l\u001b>']
		for(var i in filterOut)
			str = str.replace(filterOut[i],'')
		return str
	}

	$.extend(self, {
		socketSend: function(data) {
			socket.send(data);
		},
		ghciExecuteCommand: function(cmd) {
			cmd = commandParser(cmd);
			if(cmd)
				self.socketSend(JSON.stringify({topic:'ghci_input', data: cmd+'\n'}));
		},
		print: function(str) {
			terminal.echo(str)
		},
		printError: function(str) {
			// terminal.echo('[[;red;]'+str+']')
			terminal.error(str)
		},
		printInfo: function(str) {
			terminal.echo('[[;purple;]'+str+']')
		},
		printDebug: function(str) {
			terminal.echo('[[;cyan;]'+str+']')
		}
	})

	// --- initialize everything

	socketInit();

	return this;
}

ko.bindingHandlers.ghciTerminal = {
	init: function(element, valueAccessor) {
		var valueAccessor = valueAccessor();
		valueAccessor.obj.terminal = $(element).consolize(valueAccessor.handler)
	}
};

function WebGHCI() {
	var app = this;

	// terminals
	app.terminals = ko.observableArray([]);
	app.terminalEach = function(f) {
		var terms = app.terminals();
		for(var i in terms)
			f(terms[i].terminal);
	}
	app.appendTerminal = function() {
		app.terminals.push({})
	}
	app.terminalHandler = function(cmd) {
		console.debug(cmd);
		var vars = app.snippetMatchVar(cmd),
			errors = app.findMissingVars(vars);
		console.debug(vars, errors)
		if(errors.length > 0) {
			app.terminalEach(function(term) {term.printError('Missing identifiers: ' + errors.join(', '))})
			return false;
		}

		for(var i in vars) {
			var variable = vars[i],
				value = ko.utils.arrayFirst(app.controls(), function(control) {
					return control.name() == variable;
				}).value();
			cmd = cmd.replace(new RegExp('\#\{'+variable+'\}', 'g'), value);
		}
		return cmd;
	}

	// controls
	app.controlTypes = ['slider'];
	app.selectedControl = ko.observable(app.controlTypes[0]);
	app.controls  = ko.observableArray([]);
	app.controlCounter = 0;
	app.appendControl = function() {
		switch(app.selectedControl()) {
			case 'slider': app.controls.push(new ControlSlider()); break;
		}
	}

	// snippets
	app.snippets = ko.observableArray([]);
	app.snippetCounter = 0;
	app.appendSnippet = function() {
		app.snippets.push(new Snippet());
	}
	app.snippetUnsubscribe = function(snippet) {
		for(var i in snippet.subscriptions)
			snippet.subscriptions[i].dispose();
	}
	app.snippetSubscribe = function(snippet, subscriptions) {
		for(var i in subscriptions)
			snippet.subscriptions.push(subscriptions[i].subscribe(snippet.run))
	}
	app.snippetRebind = function(snippet, e) {
		var vars = app.snippetMatchVar(snippet.code()),
			subscriptions = [],
			errors = app.findMissingVars(vars);

		snippet.errors(errors);
		app.snippetUnsubscribe(snippet);

		if(errors.length > 0)
			return;

		var subscriptionObjects = ko.utils.arrayFilter(app.controls(), function(control) {
			return ko.utils.arrayFirst(vars, function(variable) {
				return control.name() == variable;
			});
		});
		for(var i in subscriptionObjects)
			subscriptions.push(subscriptionObjects[i].value);

		app.snippetSubscribe(snippet, subscriptions);
	}

	app.snippetRegexpVar = /\#\{([a-zA-Z0-9_]+?)\}/g
	app.snippepRegexp = function(regexp, str) {
		var matches=[],
			tmp;
		while(tmp = regexp.exec(str)) {
			matches.push(tmp)
		}
		return matches;
	}
	app.snippetMatchVar = function(str) {
		var result = [],
			tmp = app.snippepRegexp(app.snippetRegexpVar, str);
		for(var i in tmp)
			result.push(tmp[i][1]);
		return ko.utils.arrayGetDistinctValues(result).sort();
	}

	app.findMissingVars = function(vars) {
		return ko.utils.arrayFilter(vars, function(variable) {
			return ko.utils.arrayFirst(app.controls(), function(control) {
				return control.name() == variable;
			}) ? false : true;
		});
	}

	// objects
	function ControlSlider() {
		var self = this;
		self.type  = 'slider';
		self.name  = ko.observable('slider'+(++app.controlCounter));
		self.value = ko.observable(0);
		self.min   = ko.observable(0);
		self.max   = ko.observable(100);
		self.step  = ko.observable(0.01);
	}

	function Snippet() {
		var self = this;
		self.name = ko.observable('snippet'+(++app.snippetCounter));
		self.code = ko.observable('');
		self.subscriptions = [];
		self.errors        = ko.observableArray([]);
		self.hasErrors     = ko.computed(function() {
			return self.errors().length > 0;
		});
		self.runFailure    = ko.observable(false);

		self.run = function() {
			self.runFailure(self.hasErrors())
			// console.debug('running: ',self.name(), self.hasErrors(), self.runFailure())
			app.terminalEach(function(term) {term.ghciExecuteCommand(self.code())})
		}
	}

	// init
	app.appendTerminal();
}

$(document).ready(function() {
	// $('#terminal').consolize()
	ko.applyBindings(new WebGHCI());
})
