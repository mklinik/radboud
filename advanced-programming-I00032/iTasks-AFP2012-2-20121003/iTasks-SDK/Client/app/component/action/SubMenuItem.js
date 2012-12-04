Ext.define('itwc.component.action.SubMenuItem',{
	extend: 'Ext.menu.Item',
	alias: 'widget.itwc_submenuitem',
	floating: false,
	
	onClick: function() {
		return false;
	}
});
