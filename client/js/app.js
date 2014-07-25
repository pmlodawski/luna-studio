String.prototype.trimChars = function(chars) {
	var regexp = new RegExp('^['+chars+']+|['+chars+']+$', 'gm');
	return this.replace(regexp, '');
}

$.fn.consolize = function(commandHandler) {
	var self     = this,
		terminal = self.terminal(commandHandler);

	$.extend(self, {
		print: function(str) {
			terminal.echo(str);
			self.parent().scrollTop(self.outerHeight());
		},
		printError: function(str) {
			self.print('[[;red;]'+str+']')
		},
		printInfo: function(str) {
			self.print('[[;purple;]'+str+']')
		},
		printDebug: function(str) {
			self.print('[[;cyan;]'+str+']')
		}
	})

	return this;
}

ko.bindingHandlers.ghciTerminal = {
	init: function(element, valueAccessor) {
		var valueAccessor = valueAccessor();
		valueAccessor.obj.terminal = $(element).consolize(valueAccessor.handler)
	}
};

function WebGHCI(host) {
	var app = this;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// terminals
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	app.terminals = ko.observableArray([]);
	app.terminalEach = function(f) {
		var terms = app.terminals();
		for(var i in terms)
			f(terms[i].terminal);
	}
	app.appendTerminal = function() {
		app.terminals.push({})
	}

	function commandParser(cmd) {
		var vars = app.snippetMatchVar(cmd),
			errors = app.findMissingVars(vars);
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

	app.shellExecuteCommand = function(cmd) {
		cmd = commandParser(cmd);
		console.debug(cmd)
		if(cmd)
			app.socketSend({topic:'shell_input', data: cmd+'\n'});
	}

	app.commandHandler = function(cmd, term) {
		var cmd_split = cmd.split(" ");
		if(cmd_split[0] == 'wssend') {
			app.socketSend({topic: cmd_split[1], data: cmd_split[2]});
			return;
		} else if(cmd_split[0] == 'watch') {
			app.socketSend({topic: 'inotify_subscribe', data: 'projects' + (cmd_split[1] ? cmd_split[1] : '')})
			return;
		}
		app.shellExecuteCommand(cmd);
	}

	function clearOutputFromGHCI(str) {
		console.debug('- - - - - clearing - - - - -')
		// console.debug(str)
		var filterOut = ['\u001b[?1h\u001b=','\u001b[?1l\u001b>']
		for(var i in filterOut)
			str = str.replace(filterOut[i],'')
		return str
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// controls
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	app.controlTypes = ['slider'];
	app.selectedControl = ko.observable(app.controlTypes[0]);
	app.controls  = ko.observableArray([]);
	app.controlCounter = 0;
	app.appendControl = function() {
		switch(app.selectedControl()) {
			case 'slider': app.controls.push(new ControlSlider()); break;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// snippets
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// files
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	app.files = ko.observableArray([]);
	app.fileCounter = 0;
	app.appendFile = function(path, type) {
		var segments = segmentizePath(path),
			lastFile = app,
			lastPath = '',
			remaining = 0,
			tmp = null;

		while((remaining = segments.length) > 0) {
			tmp = segments.shift();
			var exists = ko.utils.arrayFirst(lastFile.files(), function(file) {
				return file.name() == tmp;
			});
			if(exists) {
				tmp = exists;
			} else {
				tmp = remaining > 1 ? new FileNode(lastPath + '/' + tmp, 'dir') : new FileNode(path, type);
				lastFile.files.push(tmp);
			}
			lastFile = tmp;
			lastPath = lastFile.path();
		}
	}
	// app.removeFile = function(path) {
	// 	var segments = segmentizePath(path),
	// 		lastFile = app,
	// 		remaining = 0,
	// 		tmp = null;

	// 	while((remaining = segments.length) > 1) {
	// 		tmp = segments.shift();

	// 		var exists = ko.utils.arrayFirst(lastFile.files(), function(file) {
	// 			return file.name() == tmp;
	// 		});

	// 		if(!exists) break;

	// 		lastFile = exists;
	// 	}

	// 	lastFile.files.remove(function(file) {
	// 		return file.name() == segments[0];
	// 	}) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	// }

	app.fileClicked = function(file,e) {
		e.stopPropagation();
		console.debug(file.type())
		if(file.type() == 'image')
			app.appendViewer(file)
	}

	function segmentizePath(path) {
		return path.trimChars('/').split('/');
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// viewers
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	app.viewers = ko.observableArray([]);
	app.appendViewer = function(file) {
		// app.viewers.push(file);
		app.viewers.removeAll();
		if('image' == file.type())
			app.viewers([file]);
	}

	app.freshFileURL = function(url) {
		return url + '?' + new Date().getTime();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// suckIt
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	app.www    = host  ? 'http://'+host+':8000' : 'http://127.0.0.1:8000';
	app.host   = host ? 'ws://'+host+':8080' : 'ws://127.0.0.1:8080';
	app.socket = null;

	function socketInit() {
		app.socket = new WebSocket(app.host);

		app.socket.onopen = function() {
			console.debug('socket open', arguments);
			app.socketSend({topic: 'spawn_shell', data: 'cd ../projects && bash'});
		}

		app.socket.onclose = function() {
			console.debug('socket closed', arguments);
		}

		app.socket.onmessage = function(msg) {
			var result = JSON.parse(msg.data)
			if('error' == result.topic) {
				app.terminalEach(function(term) {term.printError(result.data)});
			} else if('shell_output' == result.topic) {
				app.terminalEach(function(term) {term.print(clearOutputFromGHCI(result.data))});
			} else if('inotify' == result.topic) {
				var data = result.data,
					type = 'undefined',
					hasFlag = function(flag) {return $.inArray(flag, data.flags) >= 0};
				console.debug(data.name, data.mime, data.size, data.flags)

				if(hasFlag('isdir'))
					type = 'dir';
				else if($.inArray(data.mime, ['image/bmp','image/jpeg','image/png','image/gif']) >= 0)
					type = 'image';

				if(hasFlag('create'))
					app.appendFile(data.name, type);
			} else {
				app.terminalEach(function(term) {term.printDebug(msg.data)});
			}
		}
	}

	app.socketSend = function(data) {
		app.socket.send(JSON.stringify(data));
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// 'classes'
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
			app.terminalEach(function(term) {term.shellExecuteCommand(self.code())})
		}
	}

	function FileNode(path, type) {
		var self = this;

		self.path = ko.observable(path ? path : '');
		self.name = ko.computed(function() {
			var result = self.path().match(/([^\/]+)[\/]?$/);
			return result ? result[1] : result;
		});
		self.ext = ko.computed(function() {
			var result = self.path().match(/(([\/])|([\.]([^\.\/]+)))$/);
			return result ? result[0] : result;
		});
		self.type = ko.observable(type);

		self.files = ko.observableArray([]);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// init
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	socketInit();
	app.appendTerminal();
}

var datApp = null;
$(document).ready(function() {
	ko.applyBindings(datApp = new WebGHCI());
})
