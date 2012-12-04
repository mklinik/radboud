Ext.define('itwc.component.misc.Tab',{
	extend: 'Ext.tab.Tab',
	alias: 'widget.itwc_tab',

	active: false,
	closable: false,
	
	initComponent: function() {
	
		this.callParent(arguments);

		if(this.active) {
			this.activate();
		}
		if(this.closeTaskId) {
			this.closable = true;
		}		
	},
	onClick: function(e, target) {
		var me = this,
            tabEl = e.getTarget('.' + Ext.tab.Tab.prototype.baseCls),
            tab = tabEl && Ext.getCmp(tabEl.id),
            tabPanel = me.tabPanel,
            isCloseClick = tab && tab.closeEl && (target === tab.closeEl.dom);
			
		me.viewport = me.viewport || me.up('viewport');

		if(isCloseClick) {
			me.viewport.fireEvent('action',this.closeTaskId,'Close');
		} else {
			me.viewport.fireEvent('focus',this.focusTaskId);
		}
	}
});
