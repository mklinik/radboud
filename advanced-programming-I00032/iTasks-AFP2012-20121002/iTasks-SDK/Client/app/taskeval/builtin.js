// TO BE SURE
function ___predefined__Tuple3(__a1, __a2, __a3) {
	return [0, '_predefined._Tuple3', __a1, __a2, __a3];
};

function ___predefined__Tuple2(__a1, __a2) {
	return [0, '_predefined._Tuple2', __a1, __a2];
};
// TO BE SURE

// --------- Primitive functions -----------------------------

function __atan(a){
    return Math.atan(Sapl.feval(a));
}

function __mult(a,b){
    return Sapl.feval(a)*Sapl.feval(b);
}

function __div(a,b){
    return Math.floor(Sapl.feval(a)/Sapl.feval(b));
}

function __divreal(a,b){
    return Sapl.feval(a)/Sapl.feval(b);
}

function __add(a,b){
    return Sapl.feval(a)+Sapl.feval(b);
}

function __sub(a,b){
    return Sapl.feval(a)-Sapl.feval(b);
}

function __eq(a,b){
    return Sapl.feval(a)==Sapl.feval(b);
}	

function __neq(a,b){
    return Sapl.feval(a)!=Sapl.feval(b);
}	

function __mod(a,b){
    return Sapl.feval(a)%Sapl.feval(b);
}	

function __gt(a,b){
    return Sapl.feval(a)>Sapl.feval(b);
}	

function __ge(a,b){
    return Sapl.feval(a)>=Sapl.feval(b);
}

function __not(a){
    return !Sapl.feval(a);
}

function __error(str){
    throw "ERROR: "+str;
}

function __abort(str){
    throw "ABORT: "+str;
}

function __dynamic_handler(){
	return "DYNVAL";
}

function __sapldebug(str, f){
    if(confirm(str)){
        return f;
    }else{
        _error();
    }
}

function __strlen(str){
    return Sapl.feval(str).length;
}

function __string_select(str, pos){
    return Sapl.feval(str).charAt(Sapl.feval(pos));
}  

function ___string_create(length){
    var nl = Sapl.feval(length);
    var r=""; for (var a=0;a<nl;a++) r+="\0"; 
    return r;
}

function __string_create(length, c){
    var nl = Sapl.feval(length);
    var nc = Sapl.feval(c);
    var r=""; for (var a=0;a<nl;a++) r+=nc; 
    return r;
}

function __string_update(str, index, c){
    var nstr = Sapl.feval(str);
    var nc = Sapl.feval(c);
    var nindex = Sapl.feval(index);
    return nstr.substr(0, nindex) + nc + nstr.substr(nindex+nc.length);
}

function __string_slice(str, ind1, ind2){
    var nstr = Sapl.feval(str);
    var nind1 = Sapl.feval(ind1);
    var nind2 = Sapl.feval(ind2);
    
    return nstr.substr(nind1, nind2-nind1+1);
}

function __string_append(str1, str2){
    var nstr1 = Sapl.feval(str1);
    var nstr2 = Sapl.feval(str2);
    return nstr1+nstr2;
}

function __toInt_char(chr){
    var nchr = Sapl.feval(chr);
    return nchr.charCodeAt(0);
}

function __toInt_str(str){
    var nstr = Sapl.feval(str);
    return parseInt(nstr);
}

function __toInt_real(r){
    var nr = Sapl.feval(r);
    return Math.floor(nr);
}

function __toReal(str){
    var nstr = Sapl.feval(str);
    return parseFloat(nstr);
}

function __sqrt(num){
    return Math.sqrt(Sapl.feval(num));
}

function __toChar(code){
    var ncode = Sapl.feval(code);
    return String.fromCharCode(ncode);
}

function __toString(a){
    var na = Sapl.feval(a);
    return na + "";
}

function __bitand(a,b){
    return Sapl.feval(a) & Sapl.feval(b);
}

function __shiftleft(a,b){
    return Sapl.feval(a) << Sapl.feval(b);
}

function __shiftright(a,b){
    return Sapl.feval(a) >>> Sapl.feval(b);
}

// --------- SaplHtml stuff -----------------------------

function __SaplHtml_getDomElement(d, id){
    d = Sapl.feval(d);
    id = Sapl.feval(id);
    
    return ___predefined__Tuple2(d, d.getElementById(id));
}

function __SaplHtml_getObjectAttr(d, e, attr){
    d = Sapl.feval(d);
    e = Sapl.feval(e);
    attr = Sapl.feval(attr);    
    
    var value = eval("e."+attr+";");
    
    return ___predefined__Tuple3(d, e, value);
}

function __SaplHtml_runObjectMethod(d, obj, method, params){
    d = Sapl.feval(d);
    params = Sapl.toJS(Sapl.feval(params));
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);

	var eargs = [obj, method];
	for(var i=0; i<params.length; i++){
		eargs.push(params[i]);
	}
	
    var value = streval.apply(null, eargs);
    return ___predefined__Tuple3(d, obj, value);
}

function __SaplHtml_setObjectAttr(d, e, attr, value){
    d = Sapl.feval(d);
    e = Sapl.feval(e);
    value = Sapl.toJS(Sapl.feval(value));
    attr = Sapl.feval(attr);      
    
	// unbox function value, boxed by Sapl.feval
	if(isArray(value) && value.length == 2 && typeof (value[0]) == "function" && value[1].length == 0){
		value = value[0];
	}
	
    eval("e."+attr+"=value;");
    return ___predefined__Tuple3(d, e, value);
}

function __SaplHtml_getDomAttr(d, id, attr){
    d = Sapl.feval(d);
    id = Sapl.feval(id);
    attr = Sapl.feval(attr);

    var value = eval("d.getElementById(\""+id+"\")."+attr+";");
    return ___predefined__Tuple2(d, value);
}

function __SaplHtml_setDomAttr(d, id, attr, value){
    d = Sapl.feval(d);
    value = Sapl.feval(value);
    id = Sapl.feval(id);
    attr = Sapl.feval(attr);
    
    eval("d.getElementById(\""+id+"\")."+attr+"=value;");
    return ___predefined__Tuple2(d, value);
}

function __SaplHtml_findObject(d, name){

    d = Sapl.feval(d);
    name = Sapl.feval(name);

    var obj;
	try{
		eval("obj = "+name+";");
	} catch (e) {	
		// possibly undefined. don't do anything. 
	}
	
    return ___predefined__Tuple2(d, obj);
}

function __SaplHtml_createObject(d, obj, params){
    d = Sapl.feval(d);
    params = Sapl.toJS(Sapl.feval(params));
    obj = Sapl.feval(obj);

	var eargs = [obj, null];
	for(var i=0; i<params.length; i++){
		eargs.push(params[i]);
	}
	
    var value = streval.apply(null, eargs);	
    return ___predefined__Tuple2(d, value);
}

function __SaplHtml_loadExternalJS(d, url, continuation){
    d = Sapl.feval(d);
	continuation = Sapl.feval(continuation);
    url = Sapl.feval(url);	
		
	var script=document.createElement('script');
	script.setAttribute("type","text/javascript");
	script.onload = continuation;
	
	script.setAttribute("src", url);
	document.getElementsByTagName("head")[0].appendChild(script);
	
	return d;
}

function __SaplHtml_createEventHandler(expr, taskId){
	expr = Sapl.feval(expr);
	taskId = Sapl.feval(taskId);
	
    // Creating a closure of 2. layer
    var eventHandler = function(expr, taskId){
		
		var h = function(source){
			return __SaplHtml_handleJSEvent(expr, taskId, source);
		};
		
		return h;
    }
	
	return eventHandler(expr, taskId);
}

function __SaplHtml_isUndefined(obj){
	obj = Sapl.feval(obj);
	
	return obj == null;
}

function __SaplHtml_toHtmlObject(val){
	val = Sapl.feval(val);
	return Sapl.toJS(val);
}

function __SaplHtml_fromHtmlObject(obj){
	return Sapl.feval(obj);
}

// --------- Function overrides -----------------------------

function __sapldebug_sapldebug(a,b){
	console.log("DEBUG: "+Sapl.toString(a));
	return b;
}

function __ClientOverride_onClient(){
	return true;
}

function __ClientOverride_cast_to_TaskValue(___vTC_0, ___vTC_1, __a_2) {
    return Sapl.feval(__a_2);
};

function __ClientOverride_cast(___vTC_0, ___vTC_1, __a_2) {
    return Sapl.feval(__a_2);
};

function __dynamic_string_copy_to_string(a){
	return Sapl.dynamicToString(Sapl.feval(a));
}

function __dynamic_string_copy_from_string(a){
	eval("var tmp="+Sapl.feval(a)+";");
	return ___predefined__Tuple2(tmp, a); // TODO: second?
}

function __Base64_base64Encode(a){
	return window.btoa(Sapl.feval(a));
}

function __Base64_base64Decode(a){
	return window.atob(Sapl.feval(a));
}
