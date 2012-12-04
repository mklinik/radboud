/**
* eventType can be "edit" or "commit" or "init". It is necessary because eventValue can be null
* even in the case of "edit" event.
*/
function controllerWrapper(taskletId,controllerFunc,taskId,eventType,eventName,eventValue){
	
	console.time('controllerWrapper timer: eval');
	
	var state = controller.tasklets[taskletId].st;

	var tmp = [controllerFunc,[]];
	tmp[1].push(taskId);
	tmp[1].push(state);
	
	if(eventType == "edit" || eventType == "commit"){
		tmp[1].push([1, 'Just', eventName])
	}else{
		tmp[1].push([0, 'Nothing']);
	}

	if(eventType == "edit"){
		// convert eventValue to JSON to mediate type information to the controller (e.g. Int?)
		tmp[1].push([1, 'Just', JSON.stringify(eventValue)])
	}else{
		tmp[1].push([0, 'Nothing']);
	}	

	// result is a tuple of mbTUI and state
	var ys = Sapl.feval(tmp);
	state = Sapl.heval(ys[3]);

	controller.tasklets[taskletId].st = state;	// save it
	
	// toJS to make the result hyperstrict
	var newres = Sapl.toJS(Sapl.feval([controller.tasklets[taskletId].resultFunc,[state]]));	
	
	var mbTUI = Sapl.feval(ys[2]);
		
	// If mbTUI is Nothing, the task is finished
	if(mbTUI[0] == 0){
		DB.removeTasklet(taskletId);
		controller.onEdit(taskletId, "finalize", newres);
	}else{		
		var tuistr = Sapl.feval(mbTUI[2]);

		console.timeEnd('controllerWrapper timer: eval');
		
		console.time('controllerWrapper timer: serialization');
		DB.updateTasklet(controller.tasklets[taskletId], 
						 null,
						 tuistr);				
		console.timeEnd('controllerWrapper timer: serialization');
		
		console.time('controllerWrapper timer: apply TUI');
		eval("var tui = " + tuistr + ";");		
		applytui(controller.tasklets[taskletId].tui, tui);
		console.timeEnd('controllerWrapper timer: apply TUI');
		
		// Send result to the client if it is changed only
		if(!geq(controller.tasklets[taskletId].lastResult, newres)){
			controller.tasklets[taskletId].lastResult = newres;
			controller.onEdit(taskletId, "result", newres);
		}
		
				
	}
	
}

function applytui(widget,tui){

	if(tui.xtype !== widget.xtype){
		throw "ERROR: TUI/Object structure doesn't match!";
		return;
	}
			
	for(var prop in tui) {
		if(tui.hasOwnProperty(prop)){

			var val = tui[prop];
			if(prop === "xtype") continue;
				
			if(isString(val) || isBoolean(val) || isNumber(val)){
				
				if(widget[prop] !== val){
					//console.log("set "+widget.xtype+" property \""+prop+"\" to \""+val+"\"");
					
					var setter = "widget.set"+prop.capitalize();
					
					if (eval("typeof " + setter + " == 'function'")) {
					
						//console.log("...done");
						
						// instead of widget[prop] = val; to fire change event
						if(isString(val)){
							eval(setter+"(\""+val+"\");");
						}else{
							eval(setter+"("+val+");");
						}
					}else{
						//console.log("...setter is not found");
					}
				}
				
			}else if(isArray(val)){
				
				try{				
				
					if(prop === "items"){
						var children = widget.items ? widget.items.items : [];
					}else{
						var children = widget[prop];
					}
					if(children.length != val.length) throw "fallback";
				
					for(var i=0;i<val.length;i++){					
						applytui(children[i],val[i]);
					}
				
				}catch(e){
					// Fallback to replace. TODO: works for "items" only
					widget.removeAll();
					for(var i=0;i<val.length;i++){
						var a = widget.lookupComponent(val[i]);
						widget.add(a);
					}
				}				
			}else{
				
				console.error("Unhandled type for property \""+prop+"\"");
			}
			
		}			
	}	
}

function __SaplHtml_handleJSEvent(expr,taskId,event){
	
	var state = controller.tasklets[taskId].st;
	
	// Returns a tuple of the JS document and HtmlEventResult	
	// Looks like: [0, "Tuple2", document,HtmlEventResult]	
	var ys = Sapl.feval([expr,[state,taskId,event,document]]);
	
	// The result is only in HNF, so both part of the tuple must be forced,
	// but the document can be dropped after that.
	Sapl.feval(ys[2]);
	
	var newstate = Sapl.feval(ys[3]);
	controller.tasklets[taskId].st = newstate;
	
	try{
		DB.updateTasklet(controller.tasklets[taskId], 
						 controller.tasklets[taskId].getEl().dom.innerHTML,
						 null);
	}catch(e){
		// can happen that "dom" is null, but why? 
	}
						 
	// toJS to make the result hyperstrict
	var newres = Sapl.toJS(Sapl.feval([controller.tasklets[taskId].resultFunc,[newstate]]));
	
	// Send result to the client if it is changed only
	if(!geq(controller.tasklets[taskId].lastResult, newres)){
		controller.tasklets[taskId].lastResult = newres;
		controller.onEdit(taskId, "result", newres);
	}
}
