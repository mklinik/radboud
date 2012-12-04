/*
* 'Editable' components are components that represent an editable task from an iTasks perspective.
* Changes in their client-side state, are synced with the server as edit events. The component itself
* has to define the granularity of what constitutes an edit event.
*/
Ext.define('itwc.component.edit.Editable',{
	
	editable: true,
	editBufferTime: 200,

	syncEditsEnabled: false,
	
	initEditable: function() {
		var me = this;
	
		if(me.syncEditsEnabled)
			return;

		me.addEvents('edit');

		me.addManagedListener(me,'change',function () {
			this.viewport = this.viewport || this.up('viewport');
			this.viewport.fireEvent('edit',this.taskId,this.getEditorId(),this.getEditorValue());
		},me,{buffer: me.editBufferTime});

		me.syncEditsEnabled = true;
	},
	getTaskId: function() {
		return this.taskId;
	},
	getEditorId: function() {
		return this.editorId;
	},
	getEditorValue: function() {
		var v = this.getValue();
		if(v === "")
			return null;
		else
			return v;
	},
	setTaskId: function(taskId) {
		this.taskId = taskId;
	},
	setEditorId: function(editorId) {
		this.editorId = editorId;
	},
	setEditorValue: function(value) {
		this.suspendEvents();
		this.setValue(value);
		this.resumeEvents();
	}
});
