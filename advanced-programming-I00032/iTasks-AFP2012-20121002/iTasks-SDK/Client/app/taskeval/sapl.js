Sapl = new function () {

	this.variable_prefix = "__";

	this.escapeName = function (name) {
		if (name.endsWith("$eval")) {
			return Sapl.escapeName(name.substring(0, name.length - 5)) + "$eval";
		} else {
			name = encodeURIComponent(name);
			name = name.replace(/!/g, "%21");
			name = name.replace(/\*/g, "%2A");
			name = name.replace(/-/g, "%2D");
			name = name.replace(/~/g, "%7E");
			name = name.replace(/`/g, "%60");
			name = name.replace(/\./g, "_");
			name = name.replace(/%/g, '$');
			return Sapl.variable_prefix + name;
		}
	}

	// An SaplHtml event handler returns a tuple of the document and boolean
	// so, we have to return with the boolean
	this.execEvent = function (event, expr) {
		var tmp;
		eval("tmp = " + Sapl.escapeName(expr) + ";");
		return (Sapl.toJS(tmp(event, document))[1]);
	}

	this.print_ident = function (name) {
		if (typeof name == "string") {
			var a = name.trim();
			a = a.substring(Sapl.variable_prefix.length);
			a = a.replace(/\$/g, '%');
			a = encodeURIComponent(a);
			return a;
		} else {
			return name;
		}
	}

	this.print_consname = function (name) {
		var a = name.trim();
		var dot = a.indexOf(".");
		if(dot>0){
			a = a.substring(dot+1);
		}
		return a;
	}	
	
	this.isCompound = function (expr) {
		return expr.length > 0 && expr.indexOf(" ") > -1 && expr.indexOf("{") != 0;
	}

	this.toString = function (expr) {

		if (isArray(expr)) {

			// It's a constructor! We can just drop the selector number
			if (isNumber(expr[0])) {

				// Very important! Do NOT use splice here! 	
				var args = expr.slice(2, expr.length);
				var consname = expr[1];
				var consfunc = eval(this.escapeName(consname));				
				record = isArray(consfunc.fields);

				if (record) {
					var res = "{";					
					var fieldnames = consfunc.fields;
					
					for (var i = 0; i < args.length; i++) {
						var aarg = this.toString(this.feval(args[i]));
						if (i > 0) res += ", ";
						res += this.print_consname(fieldnames[i]) + ": ";
						res += this.isCompound(aarg) ? "(" + aarg + ")" : aarg;
					}

					return res + "}";
				} else {
					var res = this.toString(consfunc);

					for (var i = 0; i < args.length; i++) {
						var aarg = this.toString(this.feval(args[i]));
						res += " " + (this.isCompound(aarg) ? "(" + aarg + ")" : aarg);
					}

					return res;
				}

			}

			// It's an application
			if (isArray(expr) && isFunction(expr[0]) && isArray(expr[1])) {
				if (expr[0].length > expr[1].length) {
					var res = this.toString(expr[0]);
					var args = expr[1];

					for (var i = 0; i < args.length; i++) {
						var aarg = this.toString(this.feval(args[i]));
						res += " " + (aarg.indexOf(" ") > 0 ? "(" + aarg + ")" : aarg);
					}

					return res;
				} else {
					return this.toString(this.feval(expr));
				}
			}

		} else if (isFunction(expr)) {
			if (expr.name.endsWith("$eval")) {
				return this.print_ident(expr.name.substring(0, expr.name.length - 5)) + "$eval";
			} else {
				return this.print_ident(expr.name);
			}
		} else {
			if (isNumber(expr)) {
				return expr.toString();
			} else if (isBoolean(expr)) {
				return expr.toString();
			} else {
				return "\"" + expr.replace(/"/g, "\\\"") + "\"";
			}
		}
	}

	this.isJust = function (consname){
		return consname == "StdMaybe.Just" || consname == "Maybe.Just";
	}

	this.isNothing = function (consname){
		return consname == "StdMaybe.Nothing" || consname == "Maybe.Nothing";
	}

	this.isCons = function (consname){
		return consname == "_predefined._Cons" || consname == "cons";
	}

	this.isNil = function (consname){
		return consname == "_predefined._Nil" || consname == "nil";
	}
	
	this.toJSON = function (expr) {
		return JSON.stringify(this.toJS(expr));
	}

	// It expects the expression argument to be HNF
	this.toJS = function (expr) {

		if (isArray(expr)) {

			// It's a constructor! We can just drop the selector number
			if (isNumber(expr[0])) {

				// Very important! Do NOT use splice here! 	
				var args = expr.slice(2, expr.length);
				var consname = expr[1];
				var consfunc = eval(this.escapeName(consname));
				record = isArray(consfunc.fields);

				if (record) {
					var res = {};
					var fieldnames = consfunc.fields;

					for (var i = 0; i < args.length; i++) {
						var aarg = this.toJS(this.feval(args[i]));
						if(aarg != null)
							res[this.print_consname(fieldnames[i])] = aarg;
					}

					return res;
				} else {
					if(consname == "SystemTypes.Username" || consname == "SystemTypes.Password"){
						return Sapl.feval(expr[2]);
					}
					
					if (this.isNothing(consname)) return null;
					if (this.isJust(consname)) return this.toJS(this.feval(expr[2]));
					if (this.isNil(consname)) return [];
					var res = [];

					var arraycons = this.isCons(consname);

					if (!arraycons && !consname.startsWith("_predefined._Tuple")) {

						res.push(this.print_consname(consname));
					}

					if (arraycons) {
						var f = this.toJS(this.feval(args[0]));
						res.push(f);
						var s = this.toJS(this.feval(args[1]));
						for (var i = 0; i < s.length; i++) res.push(s[i]);
					} else {
						for (var i = 0; i < args.length; i++) {
							var aarg = this.toJS(this.feval(args[i]));
							res.push(aarg);
						}
					}

					return res;
				}

			}

			// It's an application
			if (isArray(expr) && isFunction(expr[0]) && isArray(expr[1])) {
				if (expr[0].length > expr[1].length) {
					// it's an partial application. leave it like that
					return expr;
				} else {
					return this.toJS(this.feval(expr));
				}
			}

	/*	} else if (isFunction(expr)) {
			return this.print_ident(expr.name);*/
		} else {
			return expr;
		}
	}
	
	this.dynamicToString = function(expr) {

		if (isArray(expr)) {

			var ret = "[";
			for(var i=0;i<expr.length;i++){
				if(i>0) ret += ",";
				ret += this.dynamicToString(expr[i]);
			}
			return ret+"]";
			
		} else if (isFunction(expr)) {
			return expr.name;
		
		} else if (isObject(expr)) {
			return "\"OBJECT\"";
			
		} else {
			if (isNumber(expr)) {
				return expr.toString();
			} else if (isBoolean(expr)) {
				return expr.toString();
			} else if (isString(expr)){
				return "\"" + expr.replace(/"/g, "\\\"") + "\"";
			} else {
				return expr;
			}
		}
	}	
	
	// hyper(strict) eval
	this.heval = function (expr) {
		expr = Sapl.feval(expr);
		
		if (isArray(expr)) {

			// It's a constructor
			if (isNumber(expr[0])) {
				for(var i = 2; i<expr.length; i++){
					expr[i] = Sapl.heval(expr[i]);
				}
			}
		}
			
		return expr;
	}
	
	this.feval = function (expr) { // check on length split for == and <=
		var y, f, xs;
		while (1) {
			if (typeof (expr) == "object") {
				if (expr.length == 1) return expr[0]; // boxed primitive
				else if (typeof (expr[0]) == "function") { // closure
					f = expr[0];
					xs = expr[1];
					if (f.length == xs.length) { // most often occuring case
						y = f.apply(null, xs);
						expr[0] = y;
						expr.length = 1;
						return y;
					} else if (f.length < xs.length) { // less likely case
						y = f.apply(null, xs.splice(0, f.length));
						expr[0] = y;
					} else // not enough args
					return expr;
				} else if (typeof (expr[0]) == "object") { // curried application -> uncurry
					y = expr[0];
					expr[0] = y[0];
					expr[1] = y[1].concat(expr[1]);
				} else return expr; // constructor
			} else if (typeof (expr) == "function") expr = [expr, []]; // function
			else // simple value
			return expr;
		}
	};

}();