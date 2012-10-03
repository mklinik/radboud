Ext.define('itwc.component.misc.Label',{
	extend: 'Ext.form.Label',
	alias: 'widget.itwc_label',
	afterRender: function() {
		var next;
		this.callParent(arguments);
		
		//Make this label target the next component
		if(next = this.nextSibling()) {
				this.forId = next.getId() + '-inputEl';
				this.el.set({for: this.forId});
		}
	}
});
