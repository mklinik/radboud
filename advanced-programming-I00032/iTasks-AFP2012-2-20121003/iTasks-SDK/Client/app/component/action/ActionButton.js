Ext.define('itwc.component.action.ActionButton',{
	extend: 'Ext.Button',
	alias: 'widget.itwc_actionbutton',
	
	initComponent: function() {
		this.addEvents('action');
		this.callParent(arguments);
	},
	onClick: function() {
		this.viewport = this.viewport || this.up('viewport');
		this.viewport.fireEvent('action',this.taskId, this.actionId);
	}});
