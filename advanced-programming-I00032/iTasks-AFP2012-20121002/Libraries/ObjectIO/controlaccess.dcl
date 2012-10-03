definition module controlaccess

//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************

import	ossystem
import	wstate


getcontrolstypes		::		![WElementHandle`] -> [(ControlType,Maybe Id)]
getcompoundstypes		:: !Id	![WElementHandle`] -> [(ControlType,Maybe Id)]
getcontrolslayouts		:: !Point2					![WElementHandle`] !(![Id],![(Id,Bool,(Maybe ItemPos,Vector2))])
																	 -> (![Id],![(Id,Bool,(Maybe ItemPos,Vector2))])
getcontrolsviewsizes	:: !OSWindowMetrics			![WElementHandle`] !(![Id],![(Id,Bool,Size)])
																	 -> (![Id],![(Id,Bool,Size)])
getcontrolsoutersizes	:: !OSWindowMetrics			![WElementHandle`] !(![Id],![(Id,Bool,Size)])
																	 -> (![Id],![(Id,Bool,Size)])
getcontrolsselects		::							![WElementHandle`] !(![Id],![(Id,Bool,SelectState)])
																	 -> (![Id],![(Id,Bool,SelectState)])
getcontrolsshowstates	::							![WElementHandle`] !(![Id],![(Id,Bool,Bool)])			
																	 -> (![Id],![(Id,Bool,Bool)])
getcontrolstexts		::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe String)])
																	 -> (![Id],![(Id,Bool,Maybe String)])
getcontrolsnrlines		::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe Int)])
																	 -> (![Id],![(Id,Bool,Maybe Int)])
getcontrolslooks		::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe (Bool,Look))])
																	 -> (![Id],![(Id,Bool,Maybe (Bool,Look))])
getcontrolsminsizes		::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe Size)])
																	 -> (![Id],![(Id,Bool,Maybe Size)])
getcontrolsresizes		::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe ControlResizeFunction)])
																	 -> (![Id],![(Id,Bool,Maybe ControlResizeFunction)])
getpopupitems			::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe [String])])
																	 -> (![Id],![(Id,Bool,Maybe [String])])
getselectedpopupitems	::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe Index)])
																	 -> (![Id],![(Id,Bool,Maybe Index)])
getradioitems			::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe [String])])
																	 -> (![Id],![(Id,Bool,Maybe [String])])
getradiocontrolsmarks	::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe Index)])
																	 -> (![Id],![(Id,Bool,Maybe Index)])
getcheckitems			::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe [String])])
																	 -> (![Id],![(Id,Bool,Maybe [String])])
getcheckcontrolsmarks	::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe [Index])])
																	 -> (![Id],![(Id,Bool,Maybe [Index])])
getslidersdirections	::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe Direction)])
																	 -> (![Id],![(Id,Bool,Maybe Direction)])
getslidersstates		::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe SliderState)])
																	 -> (![Id],![(Id,Bool,Maybe SliderState)])
getcontrolsframes		:: !OSWindowMetrics 		![WElementHandle`] !(![Id],![(Id,Bool,Maybe ViewFrame)])
																	 -> (![Id],![(Id,Bool,Maybe ViewFrame)])
getcontrolsdomains		::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe ViewDomain)])
																	 -> (![Id],![(Id,Bool,Maybe ViewDomain)])
getscrollfunctions		::							![WElementHandle`] !(![Id],![(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))])
																	 -> (![Id],![(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))])
getcontrolsspaces		:: (Int,Int)				![WElementHandle`] !(![Id],![(Id,Bool,Maybe (Int,Int))])
															  		 -> (![Id],![(Id,Bool,Maybe (Int,Int))])
getcontrolsmargins		:: ((Int,Int),(Int,Int))	![WElementHandle`] !(![Id],![(Id,Bool,Maybe ((Int,Int),(Int,Int)))])
																	 -> (![Id],![(Id,Bool,Maybe ((Int,Int),(Int,Int)))])
