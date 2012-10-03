Ext.define('itwc.component.edit.Decimal',{
	alias: 'widget.itwc_edit_decimal',
	extend: 'Ext.form.field.Number',
	mixins: ['itwc.component.edit.Editable'],
	allowDecimals: true,
	hideTrigger: true,

	initComponent: function() {
		Ext.applyIf(this,{decimalPrecision: 20});

		this.callParent(arguments);
		this.initEditable();
	}
});
