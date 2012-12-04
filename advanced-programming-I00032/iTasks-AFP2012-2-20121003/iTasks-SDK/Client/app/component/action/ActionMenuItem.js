Ext.define('itwc.component.action.ActionMenuItem',{
	extend: 'Ext.menu.Item',
	alias: 'widget.itwc_actionmenuitem',
	floating: false,
	
	initComponent: function() {
		this.addEvents('action');
		this.callParent(arguments);
	},
	onClick: function() {
		this.viewport = this.viewport || this.up('viewport');
		this.viewport.fireEvent('action',this.taskId, this.actionId);
		return this.callParent(arguments);
	}
});
