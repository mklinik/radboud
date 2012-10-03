Ext.define('itwc.component.edit.Note',{
	extend: 'Ext.form.field.TextArea',
	alias: 'widget.itwc_edit_note',
	mixins: ['itwc.component.edit.Editable'],
	
	width: 'flex',
	minWidth: 200,
	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	}
});
