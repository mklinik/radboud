Ext.application({
	name: 'itwc', //iTasks Web Client
	appFolder: 'app',
	
	//Apply patches
	requires: ['itwc.patch.grid.View'],
	
	//Core controller that syncs with server-side session state
	controllers: ['Controller'],
	
	//On launch, create the viewport
	launch: function() {
		Ext.create('itwc.container.Viewport');
	}
});
