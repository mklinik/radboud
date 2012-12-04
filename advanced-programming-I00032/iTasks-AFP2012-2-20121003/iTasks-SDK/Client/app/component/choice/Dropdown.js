Ext.define('itwc.component.choice.Dropdown',{
	extend: 'Ext.form.field.ComboBox',
	mixins: ['itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_dropdown',
	triggerAction: 'all',
	forceSelection: true,
	
	initComponent: function() {
		var me = this,
			store = [],
			numOptions = me.options.length, i;
		
		for(i=0; i < numOptions; i++) {
			store[store.length] = [i, me.options[i]];
		}
		
		me.store = store;
		me.value = Ext.isNumber(me.value) ? store[me.value][0] : null;
	
		me.callParent(arguments);
		me.initEditable();
	} 
});
