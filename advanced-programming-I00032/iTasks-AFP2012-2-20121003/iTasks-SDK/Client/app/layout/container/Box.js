/**
 * Customized all-in-one Box layout
 */
Ext.define('itwc.layout.container.Box', {

    /* Begin Definitions */

	alias: 'layout.itwc_box',
    extend: 'Ext.layout.container.Container',

    requires: [
        'Ext.layout.container.boxOverflow.None',
        'Ext.layout.container.boxOverflow.Menu',
        'Ext.layout.container.boxOverflow.Scroller',
        'Ext.util.Format',
        'Ext.dd.DragDropManager'
    ],

    /* End Definitions */

    /**
     * @cfg {Object} defaultMargins
     * If the individual contained items do not have a margins property specified or margin specified via CSS, the
     * default margins from this property will be applied to each item.
     *
     * This property may be specified as an object containing margins to apply in the format:
     *
     *     {
     *         top: (top margin),
     *         right: (right margin),
     *         bottom: (bottom margin),
     *         left: (left margin)
     *     }
     *
     * This property may also be specified as a string containing space-separated, numeric margin values. The order of
     * the sides associated with each value matches the way CSS processes margin values:
     *
     *   - If there is only one value, it applies to all sides.
     *   - If there are two values, the top and bottom borders are set to the first value and the right and left are
     *     set to the second.
     *   - If there are three values, the top is set to the first value, the left and right are set to the second,
     *     and the bottom is set to the third.
     *   - If there are four values, they apply to the top, right, bottom, and left, respectively.
     */
    defaultMargins: {
        top: 0,
        right: 0,
        bottom: 0,
        left: 0
    },

    /**
     * @cfg {String} padding
     * Sets the padding to be applied to all child items managed by this layout.
     *
     * This property must be specified as a string containing space-separated, numeric padding values. The order of the
     * sides associated with each value matches the way CSS processes padding values:
     *
     *   - If there is only one value, it applies to all sides.
     *   - If there are two values, the top and bottom borders are set to the first value and the right and left are
     *     set to the second.
     *   - If there are three values, the top is set to the first value, the left and right are set to the second,
     *     and the bottom is set to the third.
     *   - If there are four values, they apply to the top, right, bottom, and left, respectively.
     */
    padding: 0,

	/**
     * @cfg {String} halign
     * Controls how the child items of the container are aligned vertically. 
     * Valid values of this property are:
     *
     *   - **left** - child items are aligned to the **left** of the container (**default**)
     *   - **center** - child items are aligned to the **center** of the container
     *   - **right** - child items are aligned to the **right** of the container
     */
	halign: 'left',

    /**
     * @cfg {String} valign
     * Controls how the child items of the container are aligned vertically. 
     * Valid values of this property are:
     *
     *   - **top** - child items are aligned to the **middle** of the container (**default**)
     *   - **middle** - child items are aligned to the **middle** of the container
     *   - **bottom** - child items are aligned to the **bottom** of the container
     */
    valign: 'top',

    /**
     * @cfg {String} direction 
	 * Controls the direction in which child items are layed out
	 * Valid values of this property are:
	 *   - **vertical** - child items are layed out vertically from top to bottom (**default**)
	 *   - **horizontal** - child items are layed out horizontally from left to right (**default**)
	 */
    direction: 'vertical',

    type: 'box',
    scrollOffset: 0,
    itemCls: Ext.baseCSSPrefix + 'box-item',
    targetCls: Ext.baseCSSPrefix + 'box-layout-ct',
    innerCls: Ext.baseCSSPrefix + 'box-inner',

    // availableSpaceOffset is used to adjust the availableWidth, typically used
    // to reserve space for a scrollbar
    availableSpaceOffset: 0,

    // whether or not to reserve the availableSpaceOffset in layout calculations
    reserveOffset: true,

    manageMargins: true,

    horizontal: false,

    childEls: [
        'innerCt',
        'targetEl'
    ],

    renderTpl: [
        '{%var oc,l=values.$comp.layout,oh=l.overflowHandler;',
        'if (oh.getPrefixConfig!==Ext.emptyFn) {',
            'if(oc=oh.getPrefixConfig())dh.generateMarkup(oc, out)',
        '}%}',
        '<div id="{ownerId}-innerCt" class="{[l.innerCls]} {[oh.getOverflowCls()]}" role="presentation">',
            '<div id="{ownerId}-targetEl" style="position:absolute;',
                    // This width for the "CSS container box" of the box child items gives
                    // them the room they need to avoid being "crushed" (aka, "wrapped").
                    // On Opera, elements cannot be wider than 32767px or else they break
                    // the scrollWidth (it becomes == offsetWidth) and you cannot scroll
                    // the content.
                    'width:20000px;',
                    // On IE quirks and IE6/7 strict, a text-align:center style trickles
                    // down to this el at times and will cause it to move off the left edge.
                    // The easy fix is to just always set left:0px here. The top:0px part
                    // is just being paranoid. The requirement for targetEl is that its
                    // origin align with innerCt... this ensures that it does!
                    'left:0px;top:0px;',
                    // If we don't give the element a height, it does not always participate
                    // in the scrollWidth.
                    'height:1px">',
                '{%this.renderBody(out, values)%}',
            '</div>',
        '</div>',
        '{%if (oh.getSuffixConfig!==Ext.emptyFn) {',
            'if(oc=oh.getSuffixConfig())dh.generateMarkup(oc, out)',
        '}%}',
        {
            disableFormats: true,
            definitions: 'var dh=Ext.DomHelper;'
        }
    ],

    constructor: function(config) {
        var me = this,
            type;

        me.callParent(arguments);

		//Set direction as boolean flag
		me.horizontal = me.direction == 'horizontal';

        me.initOverflowHandler();

        type = typeof me.padding;
        if (type == 'string' || type == 'number') {
            me.padding = Ext.util.Format.parseBox(me.padding);
            me.padding.height = me.padding.top  + me.padding.bottom;
            me.padding.width  = me.padding.left + me.padding.right;
        }

    },
    getItemSizePolicy: function (item, ownerSizeModel) {
		var widthWrap = ownerSizeModel && ownerSizeModel.width && ownerSizeModel.width.shrinkWrap,
			heightWrap = ownerSizeModel && ownerSizeModel.height && ownerSizeModel.height.shrinkWrap;

		return {
			setsWidth: item.width === 'flex' && !widthWrap,
			setsHeight: item.height === 'flex' && !heightWrap,
			readsWidth: false,
			readsHeight: false
		};
    },
    flexSort: function (horizontal, a, b) { //(This is actually a static method)
        var infiniteValue = Infinity;

		//If flexing in the horizontal direction, we sort on the maximum width, else the maximum height
        a = (horizontal ? a.target.maxWidth : a.target.maxHeight) || infiniteValue;
        b = (horizontal ? b.target.maxWidth : b.target.maxHeight) || infiniteValue;

        // IE 6/7 Don't like Infinity - Infinity...
        if (!isFinite(a) && !isFinite(b)) {
            return 0;
        }

        return a - b;
    },

    isItemBoxParent: function (itemContext) {
        return true;
    },

    isItemShrinkWrap: function (item) {
        return true;
    },

    roundFlex: function(width) {
        return Math.ceil(width);
    },

    /**
     * @private
     * Called by an owning Panel before the Panel begins its collapse process.
     * Most layouts will not need to override the default Ext.emptyFn implementation.
     */
    beginCollapse: function(child) {
		/*
        var me = this;

		if(child.width === 'flex' && child.collapsedHorizontal()) {
			child.collapseMemento.capture(['width']);
			child.width = 'wrap';
		} 
		if(child.height === 'flex' && child.collapsedVertical()) {
			child.collapseMemento.capture(['height']);
			child.height = 'wrap';
		}
		*/
    },

    /**
     * @private
     * Called by an owning Panel before the Panel begins its expand process.
     * Most layouts will not need to override the default Ext.emptyFn implementation.
     */
    beginExpand: function(child) {
		/*
        // Restores the flex if we used to be flexed before
        child.collapseMemento.restore(['width','height']);
		*/
    },

    beginLayout: function (ownerContext) {
        var me = this,
            style = me.innerCt.dom.style;

        // this must happen before callParent to allow the overflow handler to do its work
        // that can effect the childItems collection...
        me.overflowHandler.beginLayout(ownerContext);

        me.callParent(arguments);

        ownerContext.innerCtContext = ownerContext.getEl('innerCt', me);

        // Capture whether the owning Container is scrolling in the parallel direction
        me.scrollParallel = !!(me.owner.autoScroll || me.owner.overflowY); //TODO: fix

        // Capture whether the owning Container is scrolling in the perpendicular direction
        me.scrollPerpendicular = !!(me.owner.autoScroll || me.owner.overflowX); //TODO: fix

        // If we *are* scrolling parallel, capture the scroll position of the encapsulating element
        if (me.scrollParallel) {
            me.scrollPos = me.owner.getTargetEl().dom.scrollTop;
        }

        // Don't allow sizes burned on to the innerCt to influence measurements.
        style.width = '';
        style.height = '';

        me.cacheFlexes(ownerContext);
    },

    beginLayoutCycle: function (ownerContext, firstCycle) {
        var me = this,
			valign = me.valign,
			halign = me.halign,
            childItems, childItemsLength, childContext, i, shrinkWrap, calculated;

        // this must happen before callParent to allow the overflow handler to do its work
        // that can effect the childItems collection...
        me.overflowHandler.beginLayoutCycle(ownerContext, firstCycle);

        me.callParent(arguments);

        // Cache several of our string concat/compare results (since width/heightModel can
        // change if we are invalidated, we cannot do this in beginLayout)
		ownerContext.valign = { //Default is 'top'
			middle: valign == 'middle',
			bottom: valign == 'bottom'
		};
		ownerContext.halign = { //Default is 'left'
			center: halign == 'center',
			right: halign == 'right'
		};

		// If a dimension is set to shrink wrap we have to figure out how big child items that
		// have that dimension set to flex should be. If they have a minSize configured,
		// we can use that value. If the minSize is set to 'wrap' then we need to switch the
		// sizeModel of those child items to shrinkWrap
		childItems = ownerContext.childItems;
		childItemsLength = childItems.length;
		shrinkWrap = me.sizeModels.shrinkWrap;
		calculated = me.sizeModels.calculated;
		for(i = 0; i < childItemsLength; i++) {	
			childContext = childItems[i];

			if(childContext.target.width === 'flex') {
				if(ownerContext.widthModel.shrinkWrap && childContext.target.minWidth === 'wrap') {
					childContext.widthModel = shrinkWrap;
				} else {
					childContext.widthModel = calculated; //TODO: Check if this is necessary?
				}
			} 
			if(childContext.target.height === 'flex') {
				if(ownerContext.heightModel.shrinkWrap && childContext.target.minHeight === 'wrap') {
					childContext.heightModel = shrinkWrap;
				} else {
					childContext.heightModel = calculated;
				}
			}
		}
    },

    /**
     * This method is called to (re)cache our understanding of flexes. This happens during beginLayout and may need to
     * be called again if the flexes are changed during the layout (e.g., like ColumnLayout).
     * @param {Object} ownerContext
     * @protected
     */
    cacheFlexes: function (ownerContext) {
        var me = this,
			vtotalFlex = 0,
			htotalFlex = 0,
            childItems = ownerContext.childItems,
            i = childItems.length,
            vflexedItems = [],
            hflexedItems = [],
            vminSize = 0,
			hminSize = 0,
            child, childContext, flex;

        while (i--) {
            childContext = childItems[i];
            child = childContext.target;

			childContext.vflex = flex = (child.height === 'flex') ? (child.vweight || 1) : 0;
			if (flex) {
				vtotalFlex += flex;
				vminSize += child.minHeight || 0;
				vflexedItems.push(childContext);
			}
			
			childContext.hflex = flex = (child.width === 'flex') ? (child.hweight || 1) : 0;
			if(flex) {
				htotalFlex += flex;
				hminSize += child.minWidth || 0;
				hflexedItems.push(childContext);
			}
        }
        ownerContext.vflexedItems = vflexedItems;
        ownerContext.vflexedMinSize = vminSize;
        ownerContext.vtotalFlex = vtotalFlex;

        ownerContext.hflexedItems = hflexedItems;
        ownerContext.hflexedMinSize = hminSize;
        ownerContext.htotalFlex = htotalFlex;

        // The vertically flexed boxes need to be sorted in ascending order of maxSize to work properly
        // so that unallocated space caused by maxWidth being less than flexed width can be
        // reallocated to subsequent flexed boxes.
		if(me.horizontal) {
        	Ext.Array.sort(hflexedItems, function(a,b) { me.flexSort(true,a,b); });
		} else {
        	Ext.Array.sort(vflexedItems, function(a,b) { me.flexSort(false,a,b); });
		}
    },

    calculate: function(ownerContext) {
        var me = this,
            targetSize = me.getContainerSize(ownerContext),
            state = ownerContext.state,
            plan = state.boxPlan || (state.boxPlan = {}),
            extraHeight = Ext.getScrollbarSize().height;

        plan.targetSize = targetSize;
	
        // Set an extra scrollbar-sized increment to add if...
        if (extraHeight && //... Scrollbars take up space
            me.scrollPerpendicular && //... and we are scrolling in the perpendicular direction
            (me.horizontal ? ownerContext.widthModel.shrinkWrap : ownerContext.heightModel.shrinkWrap) && //... and shrinkWrapping in the parallel direction
            (me.horizontal ? !ownerContext.heightModel.shrinkWrap : !ownerContext.widthModel.shrinkWrap) //... and NOT shrinkWrapping in the perpendicular direction
		   ) { // publishInnerCtSize may need to add this to contentSize if the perpendicular maxSize overflows

            // Set the flag/possible extra height for shrinkWrap
            state.additionalScrollbarWidth = extraHeight;

            // In this mode, we MUST have a perpendicular size to proceed.
            if (me.horizontal ? (!targetSize.gotHeight) : (!targetSize.gotWidth)) {
                me.done = false;
                return;
            }
        } else {
            state.additionalScrollbarWidth = 0;
        }

        if (!state.parallelDone) {
            state.parallelDone = me.calculateParallel(ownerContext, plan);
        }
        if (!state.perpendicularDone) {
            state.perpendicularDone = me.calculatePerpendicular(ownerContext, plan);
        }

        if (state.parallelDone && state.perpendicularDone) {
            // Fix for left and right docked Components in a dock component layout. This is for docked Headers and docked Toolbars.
            // Older Microsoft browsers do not size a position:absolute element's width to match its content.
            // So in this case, in the publishInnerCtSize method we may need to adjust the size of the owning Container's element explicitly based upon
            // the discovered max width. So here we put a calculatedWidth property in the metadata to facilitate this.
            if (me.owner.dock && (Ext.isIE6 || Ext.isIE7 || Ext.isIEQuirks) && !me.owner.width && !me.horizontal) {
                plan.isIEVerticalDock = true;
                plan.calculatedWidth = plan.maxSize + ownerContext.getPaddingInfo().width + ownerContext.getFrameInfo().width;
            }

            if (me.done && (me.horizontal ? ownerContext.vtotalFlex : ownerContext.htotalFlex) && !state.perpendicularFlexedDone) {
				me.calculatePerpendicularFlexed(ownerContext, plan);
                state.perpendicularFlexedDone = true;
            }

            me.publishInnerCtSize(ownerContext, me.reserveOffset ? me.availableSpaceOffset : 0);
        } else {
            me.done = false;
        }
    },

    calculateParallel: function(ownerContext, plan) {
        var me = this,
            shrinkWrap = me.horizontal ? ownerContext.widthModel.shrinkWrap : ownerContext.heightModel.shrinkWrap,
            childItems = ownerContext.childItems,
            childItemsLength = childItems.length,
            flexedItems = me.horizontal ? ownerContext.hflexedItems : ownerContext.vflexedItems,
            flexedItemsLength = flexedItems.length,
			isCenter = me.horizontal ? ownerContext.halign.center : ownerContext.valign.middle,
			isEnd = me.horizontal ? ownerContext.halign.right : ownerContext.valign.bottom,
            padding = me.padding,
            start = padding[me.horizontal ? 'left':'top'],
            nonFlexSize = start + padding[me.horizontal ? 'right':'bottom'] + me.scrollOffset +
                                    (me.reserveOffset ? me.availableSpaceOffset : 0),
            i, childMargins, childSize, remainingSize, remainingFlex, childContext, flex, flexedSize, contentSize;

        // Gather the total size taken up by non-flexed items:
        for (i = 0; i < childItemsLength; ++i) {
            childContext = childItems[i];
            childMargins = childContext.marginInfo || childContext.getMarginInfo();

            nonFlexSize += me.horizontal ? childMargins.width : childMargins.height;

            if (childContext[me.horizontal ? 'widthModel':'heightModel'].shrinkWrap || (me.horizontal ? !childContext.hflex : !childContext.vflex)) {
				childSize = childContext.getProp(me.horizontal ? 'width':'height'); // min/maxWidth safe
                nonFlexSize += childSize;
                if (isNaN(nonFlexSize)) {
                    return false;
                }
            }
        }
        // if we get here, we have all the childWidths/childHeights for non-flexed items...

        if (shrinkWrap) {
            plan.availableSpace = 0;
            plan.tooNarrow = false;
        } else {
            plan.availableSpace = (me.horizontal ? plan.targetSize.width : plan.targetSize.height) - nonFlexSize;

            // If we're going to need space for a parallel scrollbar, then we need to redo the perpendicular measurements
            plan.tooNarrow = plan.availableSpace < (me.horizontal ? ownerContext.hflexedMinSize : ownerContext.vflexedMinSize);
            if (plan.tooNarrow && Ext.getScrollbarSize().width && me.scrollParallel && ownerContext.state.perpendicularDone) {
                ownerContext.state.perpendicularDone = false;
                for (i = 0; i < childItemsLength; ++i) {
                    childItems[i].invalidate();
                }
            }
        }

        contentSize = nonFlexSize;
        remainingSize = plan.availableSpace;
        remainingFlex = me.horizontal ? ownerContext.htotalFlex : ownerContext.vtotalFlex;

        // Calculate flexed item sizes:
        for (i = 0; i < flexedItemsLength; i++) {
            childContext = flexedItems[i];
            flex         = me.horizontal ? childContext.hflex : childContext.vflex;
			//Don't flex the items that are set to shrinkWrap 
			if(childContext[me.horizontal ? 'widthModel':'heightModel'].shrinkWrap)	{
				continue;
			}
            flexedSize   = me.roundFlex((flex / remainingFlex) * remainingSize);
            flexedSize   = childContext[me.horizontal ? 'setWidth':'setHeight'](flexedSize); // constrained

            // for shrinkWrap w/flex, the item will be reduced to minWidth (maybe 0)
            // due to minWidth/minHeight constraints, it may be that flexedSize > remainingSize
            contentSize += flexedSize;
            // Remaining space has already had margins subtracted, so just subtract size
            remainingSize = Math.max(0, remainingSize - flexedSize); // no negatives!
            remainingFlex -= flex;
        }

		// Check if we have all the information needed to position the items:
		// If we are not shrinkWrap in the parallel dimension, we need its size before we can lay out boxes
        if (me.horizontal ? (!ownerContext.widthModel.shrinkWrap && !plan.targetSize.gotWidth)
						  : (!ownerContext.heightModel.shrinkWrap && !plan.targetSize.gotHeight)) {
            return false;
        }

        if (isCenter) {
            start += remainingSize / 2;

            // If content is too wide to pack to center, do not allow the centering calculation to place it off the left edge.
            if (start < 0) {
                start = 0;
            }
        } else if (isEnd) {
            start += remainingSize;
        }

        // Assign parallel position for the boxes:
        for (i = 0; i < childItemsLength; ++i) {
            childContext = childItems[i];
			childMargins = childContext.marginInfo;

            start += childMargins[me.horizontal ? 'left':'top'];

            childContext.setProp(me.horizontal ? 'x':'y', start);

            // We can read directly from "props.width/props.height" because we have already properly
            // requested it in the calculation of nonFlexedWidths or we calculated it.
            // We cannot call getProp because that would be inappropriate for flexed items
            // and we don't need any extra function call overhead:
            start += childMargins[me.horizontal ? 'right':'bottom'] + childContext.props[me.horizontal ? 'width':'height'];
        }

        // Stash the contentSize on the state so that it can always be accessed later in the calculation
        ownerContext.state.contentSize = contentSize + ownerContext.targetContext.getPaddingInfo()[me.horizontal ? 'width':'height'];

        // If we may have to increase our contentSize to accommodate shrinkwrapping a scrollbar, we cannot publish our contentWidth/contentHeight
        // Until we know whether that scrollbar is neeeded. Which won't be until we've calculated perpendicular
        if (!ownerContext.state.additionalScrollbarWidth) {
            ownerContext[me.horizontal ? 'setContentWidth':'setContentHeight'](ownerContext.state.contentSize);
        }

        return true;
    },

    calculatePerpendicular: function(ownerContext, plan) {
        var me = this,
            shrinkWrap = me.horizontal ? ownerContext.heightModel.shrinkWrap : ownerContext.widthModel.shrinkWrap,
            targetSize = plan.targetSize,
            childItems = ownerContext.childItems,
            childItemsLength = childItems.length,
            mmax = Math.max,
            padding = me.padding,
            start = padding[me.horizontal ? 'top':'left'],
            availSize = me.horizontal ? (targetSize.height - start - padding.bottom) : (targetSize.width - start - padding.right),
            isCenter = me.horizontal ? ownerContext.valign.middle : ownerContext.halign.center,
            isEnd = me.horizontal ? ownerContext.valign.bottom : ownerContext.halign.right,
            maxSize = 0,
            childStart, i, childSize, childMinSize, childMargins, diff, size, childContext,
            scrollbarHeight;

        if (!shrinkWrap && (isCenter || isEnd || (me.horizontal ? ownerContext.vtotalFlex : ownerContext.htotalFlex))) {
            if (isNaN(availSize)) {
                return false;
            }
        }

        // If the intention is to horizontally scroll height-fitted child components, but the container is too narrow,
        // then we must allow for the parallel scrollbar to intrude into the perpendicular dimension
        if ((me.horizontal ? ownerContext.vtotalFlex : ownerContext.htotalFlex) && me.scrollParallel && plan.tooNarrow) {
            scrollbarHeight = Ext.getScrollbarSize().height;
            availSize -= scrollbarHeight;
            plan.targetSize[me.horizontal ? 'height':'width'] -= scrollbarHeight;
        }

		//Find out what the biggest non-flexible element is
		for (i = 0; i < childItemsLength; i++) {
			childContext = childItems[i];
			childMargins = childContext.marginInfo || childContext.getMarginInfo();
			//Only measure if we are not going to set the size
            if (me.horizontal ? (childContext.vflex && !childContext.heightModel.shrinkWrap) : (childContext.hflex && !childContext.widthModel.shrinkWrap)) {
				childSize = availSize - childMargins[me.horizontal ? 'height':'width'] || 0;
			} else {
				childSize = childContext.getProp(me.horizontal ? 'height' : 'width');
			}

			childMinSize = childContext.target[me.horizontal ? 'minHeight':'minWidth'] || 0;
			childMinSize = (childMinSize === 'wrap') ? 0 : childMinSize;
			
			// Max perpendicular measurement (used for horizontal flexing) must take
			// the min perpendicular size of each child into account in case any fall short.
			if (isNaN(maxSize = mmax(maxSize, childSize + childMargins[me.horizontal ? 'height' : 'width'], childMinSize))) {
				return false; 
			}
		}

		plan.maxSize = maxSize;
		ownerContext[me.horizontal ? 'setContentHeight':'setContentWidth'](maxSize + me.padding[me.horizontal ? 'height' : 'width'] +
			ownerContext.targetContext.getPaddingInfo()[me.horizontal ? 'height' : 'width']);

		
		if (isCenter || isEnd) {		
			// When calculating a centered position within the content box of the innerCt,
			// the width of the borders must be subtracted from the size to yield the
			// space available to center within. The publishInnerCtSize method explicitly
			// adds the border widths to the set size of the innerCt.
			size = shrinkWrap ? maxSize : availSize;
			size = size - ownerContext.innerCtContext.getBorderInfo()[me.horizontal ? 'height' : 'width'];
		} 
	
        for (i = 0; i < childItemsLength; i++) {
            childContext = childItems[i];
            childMargins = childContext.marginInfo || childContext.getMarginInfo();

            childStart = start + childMargins[me.horizontal ? 'top':'left'];
			
            if (me.horizontal ? (childContext.vflex && !childContext.heightModel.shrinkWrap)
							  : (childContext.hflex && !childContext.widthModel.shrinkWrap)) {
                childContext[me.horizontal ? 'setHeight' : 'setWidth'](maxSize - childMargins[me.horizontal ? 'height':'width']);
            } else if (isCenter) {
				if(isNaN(size)) { //If we need to align an element we need the container size first
					return false;
				}
                diff = size - childContext.props[me.horizontal ? 'height':'width']; //Already read in previous loop
                if (diff > 0) {
                    childStart = start + Math.round(diff / 2);
                }
            } else if (isEnd) {
				if(isNaN(size)) { //If we need to align an element we need the container size first
					return false;
				}
				childStart = start + mmax(0,size - childContext.props[me.horizontal ? 'height':'width'] - childMargins[me.horizontal ? 'bottom':'right']);
			}

            childContext.setProp(me.horizontal ? 'y':'x', childStart);
        }
        return true;
    },

	calculatePerpendicularFlexed: function(ownerContext, plan) {
		var me = this,
			childItems = me.horizontal ? ownerContext.vflexedItems : ownerContext.hflexedItems,
            childItemsLength = childItems.length,
			maxSize = plan.maxSize,
			onBeforeInvalidateChild = me.onBeforeInvalidateChild,
			onAfterInvalidateChild = me.onAfterInvalidateChild,
			childContext, props, i, childWidth,childHeight;

		for (i = 0; i < childItemsLength; ++i) {
			childContext = childItems[i];

			props = childContext.props;
			if(me.horizontal) {
				childWidth  = props.width;
				childHeight = maxSize - childContext.getMarginInfo().height;
			} else {
				childWidth  = maxSize - childContext.getMarginInfo().width;
				childHeight = props.height;
			}
			// When we invalidate a child, since we won't be around to size or position
			// it, we include an after callback that will be run after the invalidate
			// that will (re)do that work. The good news here is that we can read the
			// results of all that from the childContext props.
			//
			// We also include a before callback to change the sizeModel to calculated
			// prior to the layout being invoked.
			if (childHeight != props.height || childWidth != props.width) { //Only invalidate if we have to
		
				childContext.invalidate({
					before: onBeforeInvalidateChild,
					after: onAfterInvalidateChild,
					layout: me,
					horizontal: me.horizontal,
					childWidth: childWidth,
					childHeight: childHeight,
					childX: props.x,
					childY: props.y
				});
			}
        }
	},

    completeLayout: function(ownerContext) {
        var me = this;

        me.overflowHandler.completeLayout(ownerContext);

        // If we are scrolling parallel, restore the saved scroll position
        if (me.scrollParallel) {
            me.owner.getTargetEl().dom.scrollTop = me.scrollPos;
        }
    },

    finishedLayout: function(ownerContext) {
        this.overflowHandler.finishedLayout(ownerContext);
        this.callParent(arguments);
    },

	onBeforeInvalidateChild: function (childContext, options) {
		// Change the childItem to calculated (i.e., "set by ownerCt"). The component layout
        // of the child can course-correct (like dock layout does for a collapsed panel),
        // so we must make these changes here before that layout's beginLayoutCycle is
        // called.
        if (!childContext[options.horizontal ? 'heightModel' : 'widthModel'].constrainedMax) {
            // if the child hit a max constraint, it needs to be at its configured size, so
            // we leave the sizeModel alone...
            childContext[options.horizontal ? 'heightModel' : 'widthModel'] = Ext.layout.SizeModel.calculated;
        }
	},

	onAfterInvalidateChild: function (childContext, options) {

		childContext.setProp('x', options.childX);
		childContext.setProp('y', options.childY);

		if (childContext.heightModel.calculated) { 
			childContext.setHeight(options.childHeight);
		} 
       	if (childContext.widthModel.calculated) {
			childContext.setWidth(options.childWidth);
		}
	},

    publishInnerCtSize: function(ownerContext, reservedSpace) {
        var me = this,
			isCenter = me.horizontal ? ownerContext.valign.middle : ownerContext.halign.center,
			isEnd = me.horizontal ? ownerContext.valign.bottom : ownerContext.halign.right,
            dock = me.owner.dock,
            padding = me.padding,
            plan = ownerContext.state.boxPlan,
            targetSize = plan.targetSize,
			parallelShrinkWrap = ownerContext[me.horizontal ? 'widthModel':'heightModel'].shrinkWrap,
			perpendicularShrinkWrap = ownerContext[me.horizontal ? 'heightModel':'widthModel'].shrinkWrap,
            innerCtContext = ownerContext.innerCtContext,
            innerCtParallel = (parallelShrinkWrap || (plan.tooNarrow && me.scrollParallel)
                    ? ownerContext.state.contentSize
                    : targetSize[me.horizontal ? 'width':'height']) - (reservedSpace || 0),
            innerCtPerpendicular;

        if (!perpendicularShrinkWrap && (me.horizontal ? ownerContext.vtotalFlex : ownerContext.htotalFlex)) { //If there are flexed elements, use full space
            innerCtPerpendicular = targetSize[me.horizontal ? 'height':'width'];
        } else { //Make the innerCt as big as necessary
            innerCtPerpendicular = plan.maxSize + (
				me.horizontal ? (padding.top + padding.bottom + innerCtContext.getBorderInfo().height)
							  : (padding.left + padding.right + innerCtContext.getBorderInfo().width));
            if (!perpendicularShrinkWrap && (isCenter || isEnd)) {
                innerCtPerpendicular = Math.max(targetSize[me.horizontal ? 'height':'width'], innerCtPerpendicular);
            }
        }

        innerCtContext.setHeight(me.horizontal ? innerCtPerpendicular : innerCtParallel);
        innerCtContext.setWidth(me.horizontal ? innerCtParallel : innerCtPerpendicular);

        // If we are scrolling in the perpendicular dimension
        // AND we are shrinkWrapping the parallel dimension (Imagine stacking boxes on top of each other and stretching the container height),
        // AND the perpendicular is *not* shrinkWrapped, and overflows
        // AND scrollbars take up space
        //    Then the shrink wrap size must extend to include the scrollbar
        if (ownerContext.state.additionalScrollbarWidth) {
            if (innerCtPerpendicular > plan.targetSize[me.horizontal ? 'height':'width']) {
                ownerContext.setProp(me.horizontal ? 'contentWidth':'contentHeight', ownerContext.state.contentSize + ownerContext.state.additionalScrollbarWidth);

                // Scrollbar does not stretch the container in IE6, 7 and quirks, so we must explicitly extend the container to accommodate the scrollbar, otherwise it "cuts into" the content.
                if (Ext.isIE6 || Ext.isIE7 || Ext.isIEQuirks) {
                    ownerContext[me.horizontal ? 'setWidth':'setHeight'](ownerContext.props[me.horizontal ? 'contentWidth':'contentHeight'] + ownerContext.getPaddingInfo()[me.horizontal ? 'width':'height'] + ownerContext.getBorderInfo()[me.horizontal ? 'width':'height']);
                }
            } else {
                ownerContext.setProp(me.horizontal ? 'contentWidth':'contentHeight', ownerContext.state.contentSize);
            }
        }

        // If unable to publish both dimensions, this layout needs to run again
        if (isNaN(innerCtParallel + innerCtPerpendicular)) {
            me.done = false;
        }

        // If a calculated width has been found (this only happens for widthModel.shrinkWrap
        // vertical docked Components in old Microsoft browsers) then, if the Component has
        // not assumed the size of its content, set it to do so.
        //
        // We MUST pass the dirty flag to get that into the DOM, and because we are a Container
        // layout, and not really supposed to perform sizing, we must also use the force flag.
        if (plan.calculatedWidth && (dock == 'left' || dock == 'right')) {
            ownerContext.setWidth(plan.calculatedWidth, true, true);
        }
    },
    onRemove: function(comp){
        var me = this;
        me.callParent(arguments);
        if (me.overflowHandler) {
            me.overflowHandler.onRemove(comp);
        }
        if (comp.layoutMarginCap == me.id) {
            delete comp.layoutMarginCap;
        }
    },
    /**
     * @private
     */
    initOverflowHandler: function() {
        var me = this,
            handler = me.overflowHandler,
            handlerType,
            constructor;

        if (typeof handler == 'string') {
            handler = {
                type: handler
            };
        }

        handlerType = 'None';
        if (handler && handler.type !== undefined) {
            handlerType = handler.type;
        }

        constructor = Ext.layout.container.boxOverflow[handlerType];
        if (constructor[me.type]) {
            constructor = constructor[me.type];
        }

        me.overflowHandler = Ext.create('Ext.layout.container.boxOverflow.' + handlerType, me, handler);
    },

    // Overridden method from Ext.layout.container.Container.
    // Used in the beforeLayout method to render all items into.
    getRenderTarget: function() {
        return this.targetEl;
    },

    // Overridden method from Ext.layout.container.Container.
    // Used by Container classes to insert special DOM elements which must exist in addition to the child components
    getElementTarget: function() {
        return this.innerCt;
    },

    //<debug>
    calculateChildBox: Ext.deprecated(),
    calculateChildBoxes: Ext.deprecated(),
    updateChildBoxes: Ext.deprecated(),
    //</debug>

    /**
     * @private
     */
    destroy: function() {
        Ext.destroy(this.innerCt, this.overflowHandler);
        this.callParent(arguments);
    }
});
