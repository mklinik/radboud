/**
* This patch fixes horizontal shrink-wrapping of grids.
* It replaces the autocomponent layout on the grid view by a specialized layout
* that takes the column spec into account.
*/
Ext.define('itwc.patch.grid.GridViewLayout',{
	extend: 'Ext.layout.component.Auto',
	alias: 'layout.gridviewlayout',
	type: 'gridviewlayout',

//	waitForOuterHeightInDom: true,

	calculate: function(ownerContext) {
		var me = this,
			measurement = me.measureAutoDimensions(ownerContext),
			heightModel = ownerContext.heightModel,
			widthModel = ownerContext.widthModel,
			width, height;

		// It is generally important to process widths before heights, since widths can
		// often effect heights...
		if (measurement.gotWidth) {
			if (widthModel.shrinkWrap) {
				me.publishOwnerWidth(ownerContext, ownerContext.target.headerCt.getFullWidth()); //SCARY WORKAROUND
				//me.publishOwnerWidth(ownerContext, measurement.contentWidth);
			} else if (me.publishInnerWidth) {
				me.publishInnerWidth(ownerContext, measurement.width);
			}
		} else if (!widthModel.auto && me.publishInnerWidth) {
			width = me.waitForOuterWidthInDom ? ownerContext.getDomProp('width')
					: ownerContext.getProp('width');
			if (width === undefined) {
				me.done = false;
			} else {
				me.publishInnerWidth(ownerContext, width);
			}
		}
		if (measurement.gotHeight) {
			if (heightModel.shrinkWrap) {
				me.publishOwnerHeight(ownerContext, measurement.contentHeight);
			} else if (me.publishInnerHeight) {
				me.publishInnerHeight(ownerContext, measurement.height);
			}
		} else if (!heightModel.auto && me.publishInnerHeight) {
			height = me.waitForOuterHeightInDom ? ownerContext.getDomProp('height')
					: ownerContext.getProp('height');
			if (height === undefined) {
				me.done = false;
			} else {
				me.publishInnerHeight(ownerContext, height);
			}
		}
		if (!measurement.gotAll) {
			me.done = false;
		}
    }
});

Ext.define('itwc.patch.grid.View',{
	override: 'Ext.grid.View',
	componentLayout: 'gridviewlayout'
});
