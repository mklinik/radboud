Ext.define('itwc.component.edit.Slider',{
	alias: 'widget.itwc_edit_slider',
	extend: 'Ext.slider.Single',
	mixins: ['itwc.component.edit.Editable'],

	width: 'flex',	
	minWidth: 200,

	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	}
});
