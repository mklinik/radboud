//This component is a lightweight variant of Ext's Time picker.
//It does add a picker with suggested times, but does not do any checking
//and treats the current value as a raw string
Ext.define('itwc.component.edit.Time',{
	alias: 'widget.itwc_edit_time',
	extend: 'Ext.form.field.ComboBox',
	mixins: ['itwc.component.edit.Editable'],

	width: 80, //Default width

	format: 'H:i:s',

	triggerCls: Ext.baseCSSPrefix + 'form-time-trigger',
	initDate: '1/1/2008',
	initDateFormat: 'j/n/Y',
	displayField: 'disp',
	valueField: 'date',
	queryMode: 'local',
	increment: 30,

	initComponent: function() {
		var me = this;

		me.displayTpl = new Ext.XTemplate(
			'<tpl for=".">' +
				'{[typeof values === "string" ? values : this.formatDate(values["' + me.displayField + '"])]}' +
				'<tpl if="xindex < xcount">' + me.delimiter + '</tpl>' +
			'</tpl>', {
			formatDate: Ext.Function.bind(me.formatDate, me)
		});
		this.callParent(arguments);
		this.initEditable();
	},/*
	getEditorValue: function() {
		return this.getRawValue();
	},*/
	createPicker: function() {
		var me = this,
			picker;

		me.listConfig = Ext.apply({
			xtype: 'timepicker',
			selModel: {
				mode: 'SINGLE'
			},
			cls: undefined,
			minValue: me.minValue,
			maxValue: me.maxValue,
			increment: me.increment,
			format: me.format,
			maxHeight: me.pickerMaxHeight
		}, me.listConfig);
		picker = me.callParent();
		me.store = picker.store;
		return picker;
	},
	onItemClick: function(picker, record){
		// The selection change events won't fire when clicking on the selected element. Detect it here.
		var me = this,
			selected = picker.getSelectionModel().getSelection();

		if (selected.length > 0) {
			selected = selected[0];
			if (selected && Ext.Date.isEqual(record.get('date'), selected.get('date'))) {
				me.collapse();
			}
		}
    },
    onListSelectionChange: function(list, recordArray) {
		var me = this,
			record = recordArray[0],
			val = record ? record.get('date') : null;
		if(val) {
			val = me.formatDate(val);
		}
      
		if (!me.ignoreSelection) {
			me.skipSync = true;
			me.setValue(val);
			me.skipSync = false;
			me.fireEvent('select', me, val);
			me.picker.clearHighlight();
			me.collapse();
			me.inputEl.focus();
        }
	},
	formatDate: function() {
		return Ext.form.field.Date.prototype.formatDate.apply(this, arguments);
	}
});
