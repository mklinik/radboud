definition module HTML
/**
* This module provides data types for easy construction of Html documents.
* All tags and attributes of Xhtml 1.0 transitional are captured
* in the HtmlTag and HtmlAttribute type. This library does not control
* **how** you assemble these tags into a document. It only discerns
* between tags that contain other tags and tags that are empty.
*
* For information on how to construct valid html pages with these types,
* see the document definition at:
*  http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd
*/

import StdString, Maybe

/**
* This type provides an enumeration of all html tags.
*/
:: HtmlTag	= Text				!String					//Text, in which special characters should be automatically escaped.
			| Html				!String					//Text, which should be used without any conversions.
			| ATag				![HtmlAttr] ![HtmlTag]
			| AbbrTag			![HtmlAttr] ![HtmlTag]
			| AcronymTag		![HtmlAttr] ![HtmlTag]
			| AddressTag		![HtmlAttr] ![HtmlTag]
			| AppletTag			![HtmlAttr] ![HtmlTag]
			| AreaTag			![HtmlAttr]
			| BTag				![HtmlAttr] ![HtmlTag]
			| BaseTag			![HtmlAttr] ![HtmlTag]
			| BasefontTag		![HtmlAttr]
			| BdoTag			![HtmlAttr] ![HtmlTag]
			| BigTag			![HtmlAttr] ![HtmlTag]
			| BlockquoteTag		![HtmlAttr] ![HtmlTag]
			| BodyTag			![HtmlAttr] ![HtmlTag]
			| BrTag				![HtmlAttr]
			| ButtonTag			![HtmlAttr] ![HtmlTag]
			| CaptionTag		![HtmlAttr] ![HtmlTag]
			| CanvasTag 		![HtmlAttr] ![HtmlTag]
			| CenterTag			![HtmlAttr] ![HtmlTag]
			| CiteTag			![HtmlAttr] ![HtmlTag]
			| CodeTag			![HtmlAttr] ![HtmlTag]
			| ColTag			![HtmlAttr] ![HtmlTag]
			| ColgroupTag		![HtmlAttr] ![HtmlTag]
			| DdTag				![HtmlAttr] ![HtmlTag]
			| DelTag			![HtmlAttr] ![HtmlTag]
			| DfnTag			![HtmlAttr] ![HtmlTag]
			| DirTag			![HtmlAttr] ![HtmlTag]
			| DivTag			![HtmlAttr] ![HtmlTag]
			| DlTag				![HtmlAttr] ![HtmlTag]
			| DtTag				![HtmlAttr] ![HtmlTag]
			| EmTag				![HtmlAttr] ![HtmlTag]
			| FieldsetTag		![HtmlAttr] ![HtmlTag]
			| FontTag			![HtmlAttr] ![HtmlTag]
			| FormTag			![HtmlAttr] ![HtmlTag]
			| H1Tag				![HtmlAttr] ![HtmlTag]
			| H2Tag				![HtmlAttr] ![HtmlTag]
			| H3Tag				![HtmlAttr] ![HtmlTag]
			| H4Tag				![HtmlAttr] ![HtmlTag]
			| H5Tag				![HtmlAttr] ![HtmlTag]
			| H6Tag				![HtmlAttr] ![HtmlTag]
			| HeadTag			![HtmlAttr] ![HtmlTag]
			| HrTag				![HtmlAttr]
			| HtmlTag			![HtmlAttr] ![HtmlTag]
			| ITag				![HtmlAttr] ![HtmlTag]
			| IframeTag			![HtmlAttr] ![HtmlTag]
			| ImgTag			![HtmlAttr]
			| InputTag			![HtmlAttr]
			| InsTag			![HtmlAttr] ![HtmlTag]
			| IsindexTag		![HtmlAttr]
			| KdbTag			![HtmlAttr] ![HtmlTag]
			| LabelTag			![HtmlAttr] ![HtmlTag]
			| LegendTag			![HtmlAttr] ![HtmlTag]
			| LiTag				![HtmlAttr] ![HtmlTag]
			| LinkTag			![HtmlAttr] ![HtmlTag]
			| MapTag			![HtmlAttr] ![HtmlTag]
			| MenuTag			![HtmlAttr] ![HtmlTag]
			| MetaTag			![HtmlAttr] ![HtmlTag]
			| NoframesTag		![HtmlAttr] ![HtmlTag]
			| NoscriptTag		![HtmlAttr] ![HtmlTag]
			| ObjectTag			![HtmlAttr] ![HtmlTag]
			| OlTag				![HtmlAttr] ![HtmlTag]
			| OptgroupTag		![HtmlAttr] ![HtmlTag]
			| OptionTag			![HtmlAttr] ![HtmlTag]
			| PTag				![HtmlAttr] ![HtmlTag]
			| ParamTag			![HtmlAttr] ![HtmlTag]
			| PreTag			![HtmlAttr] ![HtmlTag]
			| QTag				![HtmlAttr] ![HtmlTag]
			| STag				![HtmlAttr] ![HtmlTag]
			| SampTag			![HtmlAttr] ![HtmlTag]
			| ScriptTag			![HtmlAttr] ![HtmlTag]
			| SelectTag			![HtmlAttr] ![HtmlTag]
			| SmallTag			![HtmlAttr] ![HtmlTag]
			| SpanTag			![HtmlAttr] ![HtmlTag]
			| StrikeTag			![HtmlAttr] ![HtmlTag]
			| StrongTag			![HtmlAttr] ![HtmlTag]
			| StyleTag			![HtmlAttr] ![HtmlTag]
			| SubTag			![HtmlAttr] ![HtmlTag]
			| SupTag			![HtmlAttr] ![HtmlTag]
			| TableTag			![HtmlAttr] ![HtmlTag]
			| TbodyTag			![HtmlAttr] ![HtmlTag]
			| TdTag				![HtmlAttr] ![HtmlTag]
			| TextareaTag		![HtmlAttr] ![HtmlTag]
			| TfootTag			![HtmlAttr] ![HtmlTag]
			| ThTag				![HtmlAttr] ![HtmlTag]
			| TheadTag			![HtmlAttr] ![HtmlTag]
			| TitleTag			![HtmlAttr] ![HtmlTag]
			| TtTag				![HtmlAttr] ![HtmlTag]
			| TrTag				![HtmlAttr] ![HtmlTag]
			| UTag				![HtmlAttr] ![HtmlTag]
			| UlTag	 			![HtmlAttr] ![HtmlTag]
			| VarTag			![HtmlAttr] ![HtmlTag]

/**
* This type provides an enumeration of all attributes that can occur in html tags.
*/
:: HtmlAttr	= AbbrAttr			!String
			| AcceptAttr		!String
			| AcceptcharsetAttr	!String
			| AccesskeyAttr		!String
			| ActionAttr		!String
			| AlignAttr			!String
			| AlinkAttr			!String
			| AltAttr			!String
			| ArchiveAttr		!String
			| AxisAttr			!String
			| BackgroundAttr	!String
			| BgcolorAttr		!String
			| BorderAttr		!String
			| CellspacingAttr	!String
			| CellpaddingAttr	!String
			| CharAttr			!String
			| CharoffAttr		!String
			| CharsetAttr		!String
			| CheckedAttr
			| CiteAttr			!String
			| ClassAttr			!String
			| ClassidAttr		!String
			| ColorAttr			!String
			| ColsAttr			!String
			| ColspanAttr		!String
			| CodebaseAttr		!String
			| CodetypeAttr		!String
			| ContentAttr		!String
			| CompactAttr
			| CoordsAttr		!String
			| DataAttr			!String
			| DatetimeAttr		!String
			| DeclareAttr
			| DeferAttr			!String
			| DirAttr			!String
			| DisabledAttr
			| EnctypeAttr		!String
			| FaceAttr			!String
			| ForAttr			!String
			| FrameAttr			!String
			| FrameborderAttr	!String
			| HeadersAttr		!String
			| HeightAttr		!String
			| HrefAttr			!String
			| HreflangAttr		!String
			| HttpequivAttr		!String
			| HspaceAttr		!String
			| IdAttr			!String
			| IsmapAttr
			| LabelAttr			!String
			| LangAttr			!String
			| LanguageAttr		!String
			| LinkAttr			!String
			| LongdescAttr		!String
			| MarginheightAttr	!String
			| MarginwidthAttr	!String
			| MaxlengthAttr		!String
			| MediaAttr			!String
			| MethodAttr		!String
			| MultipleAttr
			| NameAttr			!String
			| NohrefAttr
			| NoshadeAttr
			| NowrapAttr
			| OnblurAttr		!String
			| OnchangeAttr		!String
			| OnclickAttr		!String
			| OndblclickAttr	!String
			| OnfocusAttr		!String
			| OnloadAttr		!String
			| OnmousedownAttr	!String
			| OnmousemoveAttr	!String
			| OnmouseoutAttr	!String
			| OnmouseoverAttr	!String
			| OnmouseupAttr		!String
			| OnkeydownAttr		!String
			| OnkeypressAttr	!String
			| OnkeyupAttr		!String
			| OnresetAttr		!String
			| OnselectAttr		!String
			| OnsubmitAttr		!String
			| OnunloadAttr		!String
			| ProfileAttr		!String
			| PromptAttr		!String
			| ReadonlyAttr
			| RelAttr			!String
			| RevAttr			!String
			| RowsAttr			!String
			| RowspanAttr		!String
			| RulesAttr			!String
			| SchemeAttr		!String
			| ScopeAttr			!String
			| ScrollingAttr		!String
			| SelectedAttr
			| ShapeAttr			!String
			| SizeAttr			!String
			| SpanAttr			!String
			| SrcAttr			!String
			| StandbyAttr		!String
			| StartAttr			!String
			| StyleAttr			!String
			| SummaryAttr		!String
			| TabindexAttr		!String
			| TargetAttr		!String
			| TextAttr			!String
			| TitleAttr			!String
			| TypeAttr			!String
			| UsemapAttr		!String
			| ValignAttr		!String
			| ValueAttr			!String
			| ValuetypeAttr		!String
			| VlinkAttr			!String
			| VspaceAttr		!String
			| WidthAttr			!String
			| XmllangAttr		!String
			| XmlspaceAttr		!String

instance toString HtmlTag

/*
* This html class makes it possible to use either strings, or html as description/message/instruction
*/
class html a  
where
	html :: !a -> HtmlTag
	
instance html String
instance html HtmlTag
instance html [a] | html a
instance html (Maybe a) | html a

//BACKWARDS COMPATIBILITY
RawText :== Html