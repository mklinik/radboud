Ext.define('itwc.component.edit.Document',{
	extend: 'Ext.form.field.Trigger',
	alias: 'widget.itwc_edit_document',
	mixins: ['itwc.component.edit.Editable'],

	fieldBodyCls: Ext.baseCSSPrefix + 'form-file-wrap',

	childEls: ['formEl','fileInputEl'],

	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	
	},
	onRender: function() {
		var me = this;

		me.callParent(arguments);

		me.inputEl.dom.disabled = true;

		me.fileInputEl.on({
			scope: me,
			change: me.onFileSelected
		});
	},
	getTriggerMarkup: function() {
		var me = this,
			hasValue = isObject(this.value),
			inputElCfg = {
				id: me.id + '-fileInputEl',
				cls: Ext.baseCSSPrefix + 'form-file-input',
				style: 'height: 22px; width: 17px;',
				tag: 'input',
				type: 'file',
				name: 'upload',
				size: 1
			},
			triggerCfg = {
				id: me.id + '-triggerEl',
				ui: me.ui,
				tag: 'div',
				cls: ['x-form-trigger ',hasValue ? 'x-form-clear-trigger':'x-form-search-trigger',Ext.baseCSSPrefix + 'trigger-index-0'].join(' ')
			},
			formCfg = {
				id: me.id + '-formEl',
				ui: me.ui,
				tag: 'form',
				cls: 'x-form'
			};
		if(hasValue) {
			inputElCfg.hidden = true;
		}
		triggerCfg.cn = inputElCfg;
		formCfg.cn = triggerCfg
		return '<td class="x-trigger-cell">' + Ext.DomHelper.markup(formCfg) + '</td>';
    },
	onFileSelected: function() {
		var me = this,
			formEl = me.formEl,
			fileInputEl = me.getEl().child('file'),
			opts;
		//When a file is selected, we immediately start uploading it to get its info
		//only when the upload is completed we change the value of the field
		//console.log("Starting upload of ",this.fileInputEl.dom.value);
		opts = {
			isUpload: true,
			form: formEl,
			url: '?upload',
			method: 'POST',
			success: me.onUploadComplete,
			failure: me.onUploadFailed,
			scope: me
		};
		
		me.inputEl.dom.value = 'Uploading...';

		Ext.Ajax.request(opts);
		this.disableUpload();
    },
	getValue: function() {
		return this.value;
    },
	setValue: function(value) {
		var me = this;
	
		//Toggle the icon on the trigger and enable file selection
		if(me.rendered) {
			if(value == null) {
			
				me.enableUpload();

				me.triggerEl.removeCls('x-form-clear-trigger');
				me.triggerEl.addCls('x-form-search-trigger');
			} else {
				me.triggerEl.removeCls('x-form-search-trigger');
				me.triggerEl.addCls('x-form-clear-trigger');
			}

		}

		return me.callParent(arguments);
	},
	valueToRaw: function (val) {
		return (val && val.name) || '';
	},
	rawToValue: function(raw) {
		if(raw == '') {
			return null;
		} else {
			return {name: raw, size: 0, content: ''};
		}
	},
	enableUpload: function() {
		var me = this,
			fileInputEl = me.fileInputEl;
		fileInputEl.show();	
	},
	disableUpload: function() {
		var me = this,
			fileInputEl = me.fileInputEl;
		fileInputEl.hide();
	},
	onUploadComplete: function(rsp) {
		var docs = Ext.decode(rsp.responseText);
		if(isArray(docs) && docs.length == 1) {
			this.setValue(docs[0]);	
		} else {
			this.setValue(null);
		}
	},
	onUploadFailed: function() {
		this.setValue(null);
	},
	onTriggerClick: function() {
		var me = this;

		if(!(me.value == null)) {
			me.setValue(null);
		}
	}
});
