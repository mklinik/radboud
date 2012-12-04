Ext.define('itwc.component.edit.Date',{
	alias: 'widget.itwc_edit_date',
	extend: 'Ext.form.field.Date',
	mixins: ['itwc.component.edit.Editable'],

	width: 100, //Default width
	format: 'Y-m-d',
	validateOnChange: false,
	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	},
	getEditorValue: function() {
		return this.getRawValue();
	},
	validate: function() {
		return true;
	}
});
