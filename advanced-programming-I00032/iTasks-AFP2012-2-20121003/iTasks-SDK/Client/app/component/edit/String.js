Ext.define('itwc.component.edit.String',{
	extend: 'Ext.form.field.Text',
	alias: 'widget.itwc_edit_string',
	mixins: ['itwc.component.edit.Editable'],

	width: 'flex',
	minWidth: 200,

	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	}
});
