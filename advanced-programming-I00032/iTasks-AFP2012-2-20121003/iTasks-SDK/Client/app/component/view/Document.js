Ext.define('itwc.component.view.Document',{
	extend: 'Ext.Component',
	alias: 'widget.itwc_view_document',
	
	renderTpl: ['<a href="{url}" target="_blank">{name}</a>'],

	initComponent: function() {
		this.renderData['name'] = Ext.htmlEncode(this.value.name);
		this.renderData['url'] = this.value.contentUrl;

		this.callParent(arguments);
	}
});
