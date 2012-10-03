DB = new function () {
	
	this.saveValue = function(taskletId, name, val){
		if(val){
			localStorage.setItem(taskletId+"-"+name, val);
		}else{
			localStorage.removeItem(taskletId+"-"+name);
		}
	}
	
	this.loadValue = function(taskletId, name){
		return localStorage.getItem(taskletId+"-"+name);
	}	

	this.removeValue = function(taskletId, name){
		return localStorage.removeItem(taskletId+"-"+name);
	}	
	
	this.saveTasklet = function(tasklet){
	
		var taskletId = tasklet.taskId;
		// evaluate it completely to avoid serialize JS objects as much as possible
		var st = DB.stringify(Sapl.heval(tasklet.st)); 
		var events = DB.stringify(tasklet.events);
		var resultFunc = DB.stringify(tasklet.resultFunc);
		var tui = JSON.stringify(tasklet.tui);
		var html = tasklet.html;
		var controllerFunc = DB.stringify(tasklet.controllerFunc);
		var instanceNo = tasklet.instanceNo;
		
		this.saveValue(taskletId, "st", st);
		this.saveValue(taskletId, "events", events);
		this.saveValue(taskletId, "resultFunc", resultFunc);
		this.saveValue(taskletId, "tui", tui);
		this.saveValue(taskletId, "html", html);
		this.saveValue(taskletId, "controllerFunc", controllerFunc);
		this.saveValue(taskletId, "instanceNo", instanceNo);
		this.saveValue(taskletId, "width", tasklet.width);
		this.saveValue(taskletId, "height", tasklet.height);
	}
	
	this.stringify = function(o){
		if(o == null){
			return null;
		}else{
			return Sapl.dynamicToString(o);
		}
	}
		
	this.loadTasklet = function(taskletId, o){
		o.st = this.loadValue(taskletId, "st");
		o.events = JSON.parse(this.loadValue(taskletId, "events"));
		o.resultFunc = this.loadValue(taskletId, "resultFunc");
		o.html = this.loadValue(taskletId, "html");
		o.tui = JSON.parse(this.loadValue(taskletId, "tui"));
		o.controllerFunc = this.loadValue(taskletId, "controllerFunc");
		o.instanceNo = this.loadValue(taskletId, "instanceNo");
		o.width = parseInt(this.loadValue(taskletId, "width"));
		o.height = parseInt(this.loadValue(taskletId, "height"));
	}
	
	this.updateTasklet = function(tasklet, html, tuistr){
	
		var taskletId = tasklet.taskId;	
		// evaluate it completely to avoid serialize JS objects as much as possible
		var st = DB.stringify(Sapl.heval(tasklet.st)); 
		
		this.saveValue(taskletId, "st", st);
		this.saveValue(taskletId, "tui", tuistr);
		this.saveValue(taskletId, "html", html);	
	}
	
	this.removeTasklet = function(taskletId){
		this.removeValue(taskletId, "st");
		this.removeValue(taskletId, "events");
		this.removeValue(taskletId, "resultFunc");
		this.removeValue(taskletId, "tui");
		this.removeValue(taskletId, "html");
		this.removeValue(taskletId, "controllerFunc");
		this.removeValue(taskletId, "instanceNo");
		this.removeValue(taskletId, "width");
		this.removeValue(taskletId, "height");			
	}
}

