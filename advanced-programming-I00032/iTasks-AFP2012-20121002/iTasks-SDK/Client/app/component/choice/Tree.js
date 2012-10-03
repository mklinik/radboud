Ext.define('itwc.component.choice.Tree',{
	extend: 'Ext.tree.Panel',
	mixins: ['itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_tree',
	rootVisible: false,

	width: 'flex',
	minWidth: 150,
	minHeight: 200,
	editBufferTime: 0,

	initComponent: function() {	
		var me = this,
			store;	
		
		store = Ext.create('Ext.data.TreeStore',{
			root : {xtype: 'treenode', text: 'tree', children: me.options}
		});
		
		me.store = store;
		me.selectedNode = -1;
		
		me.callParent(arguments);
		
		me.addManagedListener(me,'itemclick',me.onItemClick,me);
		me.addManagedListener(me,'beforeitemexpand',me.onItemExpand,me);
		me.addManagedListener(me,'beforeitemcollapse',me.onItemCollapse,me);
		me.initEditable();
	},
	afterRender: function() {
		this.callParent(arguments);
		
		if(Ext.isNumber(this.value)){
			this.setValue(this.value);
		}
	},
	onItemExpand: function(record) {
		this.viewport = this.viewport || this.up('viewport');
		this.viewport.fireEvent('edit', this.taskId, this.editorId, ["exp",record.raw.value,true]);
		return false;
	},
	onItemCollapse: function(record) {
		this.viewport = this.viewport || this.up('viewport');
		this.viewport.fireEvent('edit', this.taskId, this.editorId, ["exp",record.raw.value,false]);
		return false;
	},
	onItemClick: function(tree,record,item) {
		this.selectedNode = record.raw.value;
		this.viewport = this.viewport || this.up('viewport');
		
		if(record.isLeaf()) {
			this.viewport.fireEvent('edit', this.taskId, this.editorId, ["sel",record.raw.value,true]);
		}
	},
	getValue: function() {
		return this.selectedNode;
	},
	setValue: function(value) {
		var node;
		
		if(Ext.isNumber(value)) {
			this.selectedNode = value;
			node = this.getRootNode().findChildBy(function(node) {return (node.raw.value == value);},this,true);
			if(node) {
				this.getSelectionModel().select(node);
			}
		} else {
			this.selectedNode = -1;
			this.getSelectionModel().deselectAll();
		}
	},
	onDestroy: function() {
		this.store.destroy();
		this.callParent(arguments);
	}
});
