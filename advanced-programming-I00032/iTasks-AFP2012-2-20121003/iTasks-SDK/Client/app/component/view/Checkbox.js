Ext.define('itwc.component.view.Checkbox',{
	extend: 'Ext.form.field.Checkbox',
	alias: 'widget.itwc_view_checkbox',
	disabled: true,
	initComponent: function() {
		this.checked = this.value;
		this.callParent(arguments);
	}
});
