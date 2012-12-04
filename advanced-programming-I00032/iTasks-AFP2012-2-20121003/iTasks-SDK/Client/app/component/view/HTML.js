Ext.define('itwc.component.view.HTML',{
	extend: 'Ext.Component',
	alias: 'widget.itwc_view_html',
	autoScroll: true,
	initComponent: function() {
	
		this.html = this.value;	
		this.callParent(arguments);
	},
	setValue: function(value) {
		this.getEl().update(value);
	}
});
