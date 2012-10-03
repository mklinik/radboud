Ext.define('itwc.component.edit.GoogleMap',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itwc_edit_googlemap',
	mixins: ['itwc.component.edit.Editable'],

	width: 'flex',
	minWidth: 400,
	height: 'flex',
	minHeight: 300,

	statics: {
		googleApiStatus: 'unloaded',
		googleApiWaiters: [] // Track components that wait for the api to load
	},
	initComponent: function() {

		this.map = null;
		this.displayedMarkers = [];

		this.callParent(arguments);
		this.initEditable();
	},
	afterRender: function() {
		var me = this;
		
		me.callParent(arguments);
			
		switch(me.self.googleApiStatus) {
			case 'loaded':
				me.setupMap();
				break;
			case 'loading':
				this.self.googleApiWaiters.push(Ext.bind(me.setupMap,me));
				break;
			case 'unloaded':
				me.self.googleApiStatus = 'loading';
				me.self.googleApiWaiters.push(Ext.bind(me.setupMap,me));
				
				//Setup google maps callback handler
				window.googlemapsready = Ext.bind(me.apiLoaded,me);
				//Add google API to head tag
				var head = document.getElementsByTagName('head')[0];
				var tag = document.createElement('script');
				tag.type = 'text/javascript';
				tag.src = 'http://maps.google.com/maps/api/js?v=3.3&sensor=false&callback=googlemapsready';
				head.appendChild(tag);
				break;
		}
	},
	apiLoaded: function() {
		this.self.googleApiStatus = 'loaded';
		this.setupMap();
	},
	apiLoaded: function() {
		this.self.googleApiStatus = 'loaded';
		this.setupMap();
	},
	getMapType : function (mapType){
		return eval("google.maps.MapTypeId."+mapType);
	},
	getOptions : function() {
		var me = this, options = me.options;
		
		options.center = new google.maps.LatLng(me.center[0],me.center[1]);
		options.mapTypeId = this.getMapType(me.mapType);
		options.draggableCursor = "default";
		
		return options;
	},
	setupMap: function() {
		var me = this;
		
		if(!me.map) {
			
			me.map = new google.maps.Map(me.el.dom, me.getOptions());
			me.addMarkers();
			
			var updatePerspective = function() {
				var center = me.map.getCenter(),
					zoom = me.map.getZoom(),
					type = me.map.getMapTypeId().toUpperCase();
			
				var e = {center : [center.lat(),center.lng()], zoom: zoom, type : type}
			
				me.viewport = me.viewport || me.up('viewport');
				me.viewport.fireEvent('edit',me.taskId, me.editorId,e);
			
			};
			
			//Add perspective change
			google.maps.event.addListener(me.map,'dragend', updatePerspective);
			google.maps.event.addListener(me.map,'maptypeid_changed', updatePerspective);
			google.maps.event.addListener(me.map,'zoom_changed', updatePerspective);
		}
	},
	addMarkers: function() {
 		var	me = this,
			map = this.map,
			marker, infoWindow, clickHandler, dragHandler;

		for(var i=0; i<this.displayedMarkers.length; i++) {
			this.displayedMarkers[i].setMap(null);
		}
        
		this.displayedMarkers = new Array();
        
		for(var i=0; i<this.markers.length; i++) {
            
			marker = new google.maps.Marker({
				map : map,
				position : new google.maps.LatLng(this.markers[i].position[0],this.markers[i].position[1]),
				title : this.markers[i].title,
				draggable : this.markers[i].draggable,
				icon: this.markers[i].icon ? ('/googlemap-icons/' + this.markers[i].icon + '.png') : null
			});
                
			if(this.markers[i].infoWindow) {
				var markerText = this.markers[i].infoWindow;
				
                infoWindow = new google.maps.InfoWindow({
					content : this.markers[i].infoWindow
				}); 
            
				clickHandler = function(map,marker,infoWindow) {
					return function(e) {infoWindow.open(map,marker);};
				};

				google.maps.event.addListener(marker,'click',clickHandler(map,marker,infoWindow));
			}
            
			if(this.markers[i].draggable) {
				dragHandler = function(markerId) { return function(e) {
                    			me.viewport = me.viewport || me.up('viewport');
								me.viewport.fireEvent('edit',me.taskId, me.editorId,{index: markerId, point : [e.latLng.lat(),e.latLng.lng()]});
                		};};
                
				google.maps.event.addListener(marker,'dragend', dragHandler(i));
			}
            
			this.displayedMarkers[i] = marker;
		}
	},
    afterComponentLayout: function() {
    	if(this.map) {
    		google.maps.event.trigger(this.map, 'resize');
    		//Correct center after resize
    		this.map.setCenter(new google.maps.LatLng(this.center[0],this.center[1]));	
    	}
    	this.callParent(arguments);
    },
	onDestroy: function() {
		if(this.map) {
			google.maps.event.clearInstanceListeners(this.map);
			delete this.map;
		}
	}
});
