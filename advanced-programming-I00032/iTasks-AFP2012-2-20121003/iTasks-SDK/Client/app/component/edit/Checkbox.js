Ext.define('itwc.component.edit.Checkbox',{
	extend: 'Ext.form.field.Checkbox',
	alias: 'widget.itwc_edit_checkbox',
	mixins: ['itwc.component.edit.Editable'],
	initComponent: function() {
		this.checked = this.value;

		this.callParent(arguments);
		this.initEditable();
	}
});
