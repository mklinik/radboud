Ext.define('itwc.component.view.Progress',{
	extend: 'Ext.ProgressBar',
	alias: 'widget.itwc_view_progress',
	
	width: 'flex',
	waiting: false,
	
	initComponent: function() {		
		var me = this;
			
		if(me.value == 'undetermined') {
			me.value = 0.0;
			me.waiting = true;
		}
		me.callParent(arguments);
	},
	afterRender: function() {
		var me = this;
		
		
		me.callParent(arguments);
		if(me.waiting) {
			me.wait({animate: true, interval: 500, text: me.text});
		}
	}
});
