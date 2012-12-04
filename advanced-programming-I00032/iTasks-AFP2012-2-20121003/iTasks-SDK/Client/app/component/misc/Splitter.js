/*
* Splitter component for resizing two adjacent components in a container at once.
*
* This component can be dropped in between
*/
Ext.define('itwc.component.misc.Splitter', {
	extend: 'Ext.resizer.Splitter',
	alias: 'widget.itwc_splitter',


	//Overwritten getCollapseDirecion:
	getCollapseDirection: function() {
		var me = this,
			dir = me.collapseDirection,
			collapseTarget, idx, items, type;

		if (!dir) {
			collapseTarget = me.collapseTarget;
			if (collapseTarget.isComponent) {
				dir = collapseTarget.collapseDirection;
			}

			if (!dir) {
                // Avoid duplication of string tests.
                // Create a two bit truth table of the configuration of the Splitter:
                // Collapse Target | orientation
                //        0              0             = next, horizontal
                //        0              1             = next, vertical
                //        1              0             = prev, horizontal
                //        1              1             = prev, vertical
				if(me.ownerCt.direction == 'horizontal') {
					dir = (me.collapseTarget == 'prev') ? 'left' : 'right';
				} else {
					dir = (me.collapseTarget == 'prev') ? 'top' : 'bottom';					
	            }

				me.collapseDirection = dir;
			}
		}
        me.orientation = (dir == 'top' || dir == 'bottom') ? 'horizontal' : 'vertical';
        me[me.orientation] = true;

        return dir;
    }
});
