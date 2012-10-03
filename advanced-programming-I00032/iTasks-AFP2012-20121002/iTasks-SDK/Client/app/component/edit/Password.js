Ext.define('itwc.component.edit.Password',{
	extend: 'Ext.form.field.Text',
	alias: 'widget.itwc_edit_password',
	mixins: ['itwc.component.edit.Editable'],
	inputType: 'password',

	width: 'flex',
	minWidth: 200,

	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	}
});
