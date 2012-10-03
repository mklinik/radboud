Ext.define('itwc.component.choice.Grid',{
	extend: 'Ext.grid.Panel',
	mixins: ['itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_grid',
	forceFit: true,
	sortableColumns: false,
	enableColumnHide: false,
	enableColumnMove: false,
	viewConfig: {loadMask: false},

	width: 'flex',
	minWidth: 'wrap',	
	height: 'flex',
	minHeight: 'wrap',

	options: [],
	columns: [],
	value: null,

	initComponent: function() {

		var me = this,
			fields = [], 
			columns = [], i;

		//Set shrinkWrap using width & height values
		me.shrinkWrap = (me.width === 'wrap' ? 1 : 0) | (me.height === 'wrap' ? 2 : 0);

		//Setup columns			
		for(i = 0; i < me.columns.length; i++) {
			columns[i] = {text: me.columns[i], dataIndex: i};
			fields[i] = {name: i, type: 'string'};
		}
		me.columns = columns;

		//Fill store with data
		this.store = Ext.create('Ext.data.Store',{
			fields: fields,
			data: {'options': me.options},
			proxy: { type: 'memory', reader: {type: 'json', root: 'options'}}
		});

		me.callParent(arguments);
		me.on('itemclick', me.onItemClick, me);		
		me.initEditable();
	},
	afterRender: function() {
		this.callParent(arguments);
		this.setValue(this.value);
	},
	onItemClick: function(view,rec) {
		this.value = rec.index;
		this.fireEvent('change');	
	},
	setValue: function(value) {
		if(Ext.isNumber(value) && value < this.store.count() && value >= 0) {
			this.getSelectionModel().select(value);
		} else {
			this.getSelectionModel().deselectAll();
		}
	},
	getValue: function() {
		return this.value;
	},
	onDestroy: function() {
		this.store.destroy();
		this.callParent(arguments);
	}
});
