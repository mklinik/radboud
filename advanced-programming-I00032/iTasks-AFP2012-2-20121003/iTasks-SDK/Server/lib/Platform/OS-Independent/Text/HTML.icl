implementation module HTML

import StdString, StdArray, StdList, StdTuple, StdBool, Maybe

instance toString HtmlTag
where
	toString tag
		# tagsize = tagSize tag								//Calculate the size of the string we need
		# tagstring = createArray tagsize '\0'				//Create an empty buffer
		# tagstring = fst (serializeTag tag tagstring 0)	//Serialize the html tree
		= tagstring

//Calculate the size (in chars) that the take will be when
//serialize to a string.

tagSize :: HtmlTag -> Int
tagSize (Text t)			= escapedSize t
tagSize (Html t)			= size t
tagSize (ATag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (AbbrTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (AcronymTag a t)	= 19 + (attrsSize a) + (tagsSize t) 
tagSize (AddressTag a t)	= 19 + (attrsSize a) + (tagsSize t) 
tagSize (AppletTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (AreaTag a)			=  7 + (attrsSize a)
tagSize (BTag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (BaseTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (BasefontTag a)		= 11 + (attrsSize a)
tagSize (BdoTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (BigTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (BlockquoteTag a t)	= 25 + (attrsSize a) + (tagsSize t) 
tagSize (BodyTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (BrTag a)			=  5 + (attrsSize a)
tagSize (ButtonTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (CanvasTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (CaptionTag a t)	= 19 + (attrsSize a) + (tagsSize t) 
tagSize (CenterTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (CiteTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (CodeTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (ColTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (ColgroupTag a t)	= 15 + (attrsSize a) + (tagsSize t) 
tagSize (DdTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (DelTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (DfnTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (DirTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (DivTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (DlTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (DtTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (EmTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (FieldsetTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (FontTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (FormTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (H1Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H2Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H3Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H4Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H5Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H6Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (HeadTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (HrTag a)			=  5 + (attrsSize a)
tagSize (HtmlTag a t)		= 171 + (attrsSize a) + (tagsSize t) 
tagSize (ITag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (IframeTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (ImgTag a)			=  6 + (attrsSize a)
tagSize (InputTag a)		=  8 + (attrsSize a)
tagSize (InsTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (IsindexTag a)		= 10 + (attrsSize a)
tagSize (KdbTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (LabelTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (LegendTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (LiTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (LinkTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (MapTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (MenuTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (MetaTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (NoframesTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (NoscriptTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (ObjectTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (OlTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (OptgroupTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (OptionTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (PTag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (ParamTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (PreTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (QTag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (STag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (SampTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (ScriptTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (SelectTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (SmallTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (SpanTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (StrikeTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (StrongTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (StyleTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (SubTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (SupTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (TableTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TbodyTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TdTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (TextareaTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (TfootTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (ThTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (TheadTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TitleTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TtTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (TrTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (UTag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (UlTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (VarTag a t)		= 11 + (attrsSize a) + (tagsSize t) 

tagsSize :: [HtmlTag] -> Int
tagsSize tags = sum (map tagSize tags)

//Calculates the number of chars this attribute will take once serialized
attrSize :: HtmlAttr -> Int
attrSize (AbbrAttr a)			=  8 + (escapedSize a)
attrSize (AcceptAttr a)			= 10 + (escapedSize a)
attrSize (AcceptcharsetAttr a)	= 18 + (escapedSize a)
attrSize (AccesskeyAttr a)		= 13 + (escapedSize a)
attrSize (ActionAttr a)			= 10 + (escapedSize a)
attrSize (AlignAttr a)			=  9 + (escapedSize a)
attrSize (AlinkAttr a)			=  9 + (escapedSize a)
attrSize (AltAttr a)			=  7 + (escapedSize a)
attrSize (ArchiveAttr a)		= 11 + (escapedSize a)
attrSize (AxisAttr a)			=  8 + (escapedSize a)
attrSize (BackgroundAttr a)		= 14 + (escapedSize a)
attrSize (BgcolorAttr a)		= 11 + (escapedSize a)
attrSize (BorderAttr a)			= 10 + (escapedSize a)
attrSize (CellspacingAttr a)	= 15 + (escapedSize a)
attrSize (CellpaddingAttr a)	= 15 + (escapedSize a)
attrSize (CharAttr a)			=  8 + (escapedSize a)
attrSize (CharoffAttr a)		= 11 + (escapedSize a)
attrSize (CharsetAttr a)		= 11 + (escapedSize a)
attrSize (CheckedAttr)			= 18
attrSize (CiteAttr a)			=  8 + (escapedSize a)
attrSize (ClassAttr a)			=  9 + (escapedSize a)
attrSize (ClassidAttr a)		= 11 + (escapedSize a)
attrSize (ColorAttr a)			=  9 + (escapedSize a)
attrSize (ColsAttr a)			=  8 + (escapedSize a)
attrSize (ColspanAttr a)		= 11 + (escapedSize a)
attrSize (CodebaseAttr a)		= 12 + (escapedSize a)
attrSize (CodetypeAttr a)		= 12 + (escapedSize a)
attrSize (ContentAttr a)		= 11 + (escapedSize a)
attrSize (CompactAttr)			= 18
attrSize (CoordsAttr a)			= 10 + (escapedSize a)
attrSize (DataAttr a)			=  8 + (escapedSize a)
attrSize (DatetimeAttr a)		= 12 + (escapedSize a)
attrSize (DeclareAttr)			= 18
attrSize (DeferAttr a)			=  9 + (escapedSize a)
attrSize (DirAttr a)			=  7 + (escapedSize a)
attrSize (DisabledAttr)			= 20
attrSize (EnctypeAttr a)		= 11 + (escapedSize a)
attrSize (FaceAttr a)			=  8 + (escapedSize a)
attrSize (ForAttr a)			=  7 + (escapedSize a)
attrSize (FrameAttr a)			=  9 + (escapedSize a)
attrSize (FrameborderAttr a)	= 15 + (escapedSize a)
attrSize (HeadersAttr a)		= 11 + (escapedSize a)
attrSize (HeightAttr a)			= 10 + (escapedSize a)
attrSize (HrefAttr a)			=  8 + (escapedSize a)
attrSize (HreflangAttr a)		= 12 + (escapedSize a)
attrSize (HttpequivAttr a)		= 14 + (escapedSize a)
attrSize (HspaceAttr a)			= 10 + (escapedSize a)
attrSize (IdAttr a)				=  6 + (escapedSize a)
attrSize (IsmapAttr)			= 14
attrSize (LabelAttr a)			=  9 + (escapedSize a)
attrSize (LangAttr a)			=  8 + (escapedSize a)
attrSize (LanguageAttr a)		= 12 + (escapedSize a)
attrSize (LinkAttr a)			=  8 + (escapedSize a)
attrSize (LongdescAttr a)		= 12 + (escapedSize a)
attrSize (MarginheightAttr a)	= 16 + (escapedSize a)
attrSize (MarginwidthAttr a)	= 15 + (escapedSize a)
attrSize (MaxlengthAttr a)		= 13 + (escapedSize a)
attrSize (MediaAttr a)			=  9 + (escapedSize a)
attrSize (MethodAttr a)			= 10 + (escapedSize a)
attrSize (MultipleAttr)			= 20
attrSize (NameAttr a)			=  8 + (escapedSize a)
attrSize (NohrefAttr)			= 16
attrSize (NoshadeAttr)			= 18
attrSize (NowrapAttr)			= 16
attrSize (OnblurAttr a)			= 10 + (escapedSize a)
attrSize (OnchangeAttr a)		= 12 + (escapedSize a)
attrSize (OnclickAttr a)		= 11 + (escapedSize a)
attrSize (OndblclickAttr a)		= 14 + (escapedSize a)
attrSize (OnfocusAttr a)		= 11 + (escapedSize a)
attrSize (OnloadAttr a)			= 10 + (escapedSize a)
attrSize (OnmousedownAttr a)	= 15 + (escapedSize a)
attrSize (OnmousemoveAttr a)	= 15 + (escapedSize a)
attrSize (OnmouseoutAttr a)		= 14 + (escapedSize a)
attrSize (OnmouseoverAttr a)	= 15 + (escapedSize a)
attrSize (OnmouseupAttr a)		= 13 + (escapedSize a)
attrSize (OnkeydownAttr a)		= 13 + (escapedSize a)
attrSize (OnkeypressAttr a)		= 14 + (escapedSize a)
attrSize (OnkeyupAttr a)		= 11 + (escapedSize a)
attrSize (OnresetAttr a)		= 11 + (escapedSize a)
attrSize (OnselectAttr a)		= 12 + (escapedSize a)
attrSize (OnsubmitAttr a)		= 12 + (escapedSize a)
attrSize (OnunloadAttr a)		= 12 + (escapedSize a)
attrSize (ProfileAttr a)		= 11 + (escapedSize a)
attrSize (PromptAttr a)			= 10 + (escapedSize a)
attrSize (ReadonlyAttr)			= 20
attrSize (RelAttr a)			=  7 + (escapedSize a)
attrSize (RevAttr a)			=  7 + (escapedSize a)
attrSize (RowsAttr a)			=  8 + (escapedSize a)
attrSize (RowspanAttr a)		= 11 + (escapedSize a)
attrSize (RulesAttr a)			=  9 + (escapedSize a)
attrSize (SchemeAttr a)			= 10 + (escapedSize a)
attrSize (ScopeAttr a)			=  9 + (escapedSize a)
attrSize (ScrollingAttr a)		= 13 + (escapedSize a)
attrSize (SelectedAttr)			= 20
attrSize (ShapeAttr a)			=  9 + (escapedSize a)
attrSize (SizeAttr a)			=  8 + (escapedSize a)
attrSize (SpanAttr a)			=  8 + (escapedSize a)
attrSize (SrcAttr a)			=  7 + (escapedSize a)
attrSize (StandbyAttr a)		= 11 + (escapedSize a)
attrSize (StartAttr a)			=  9 + (escapedSize a)
attrSize (StyleAttr a)			=  9 + (escapedSize a)
attrSize (SummaryAttr a)		= 11 + (escapedSize a)
attrSize (TabindexAttr a)		= 12 + (escapedSize a)
attrSize (TargetAttr a)			= 10 + (escapedSize a)
attrSize (TextAttr a)			=  8 + (escapedSize a)
attrSize (TitleAttr a)			=  9 + (escapedSize a)
attrSize (TypeAttr a)			=  8 + (escapedSize a)
attrSize (UsemapAttr a)			= 10 + (escapedSize a)
attrSize (ValignAttr a)			= 10 + (escapedSize a)
attrSize (ValueAttr a)			=  9 + (escapedSize a)
attrSize (ValuetypeAttr a)		= 13 + (escapedSize a)
attrSize (VlinkAttr a)			=  9 + (escapedSize a)
attrSize (VspaceAttr a)			= 10 + (escapedSize a)
attrSize (WidthAttr a)			=  9 + (escapedSize a)
attrSize (XmllangAttr a)		= 12 + (escapedSize a)
attrSize (XmlspaceAttr a)		= 13 + (escapedSize a)

attrsSize :: ![HtmlAttr] -> Int
attrsSize attrs = sum (map attrSize attrs)

//Calculates the number of chars in a string when html special characters are escaped
escapedSize :: !{#Char} -> Int
escapedSize s = escapedSize` s (size s) 0
where
	escapedSize` s n i
		| i == n = 0
		| s.[i] == '<' = 4 + escapedSize` s n (i + 1)
		| s.[i] == '>' = 4 + escapedSize` s n (i + 1)
		| s.[i] == '&' = 5 + escapedSize` s n (i + 1)
		| otherwise = 1 + escapedSize` s n (i + 1)

serializeTag :: !HtmlTag !*{#Char} !Int -> (!*{#Char}, !Int)
serializeTag (Text t) s i				= copyChars t 0 True s i
serializeTag (Html t) s i				= copyChars t 0 False s i
serializeTag (ATag a t) s i				= writeTag "a" a t s i
serializeTag (AbbrTag a t) s i			= writeTag "abbr" a t s i
serializeTag (AcronymTag a t) s i		= writeTag "acronym" a t s i
serializeTag (AddressTag a t) s i		= writeTag "address" a t s i
serializeTag (AppletTag a t) s i		= writeTag "applet" a t s i
serializeTag (AreaTag a) s i			= writeEmptyTag "area" a s i
serializeTag (BTag a t) s i				= writeTag "b" a t s i
serializeTag (BaseTag a t) s i			= writeTag "base" a t s i
serializeTag (BasefontTag a) s i		= writeEmptyTag "basefont" a s i
serializeTag (BdoTag a t) s i			= writeTag "bdo" a t s i
serializeTag (BigTag a t) s i			= writeTag "big" a t s i
serializeTag (BlockquoteTag a t) s i	= writeTag "blockquote" a t s i
serializeTag (BodyTag a t) s i			= writeTag "body" a t s i
serializeTag (BrTag a) s i				= writeEmptyTag "br" a s i
serializeTag (ButtonTag a t) s i		= writeTag "button" a t s i
serializeTag (CanvasTag a t) s i		= writeTag "canvas" a t s i
serializeTag (CaptionTag a t) s i		= writeTag "caption" a t s i
serializeTag (CenterTag a t) s i		= writeTag "center" a t s i
serializeTag (CiteTag a t) s i			= writeTag "cite" a t s i
serializeTag (CodeTag a t) s i			= writeTag "code" a t s i
serializeTag (ColTag a t) s i			= writeTag "col" a t s i
serializeTag (ColgroupTag a t) s i		= writeTag "colgroup" a t s i
serializeTag (DdTag a t) s i			= writeTag "dd" a t s i
serializeTag (DelTag a t) s i			= writeTag "del" a t s i
serializeTag (DfnTag a t) s i			= writeTag "dfn" a t s i
serializeTag (DirTag a t) s i			= writeTag "dir" a t s i
serializeTag (DivTag a t) s i			= writeTag "div" a t s i
serializeTag (DlTag a t) s i			= writeTag "dl" a t s i
serializeTag (DtTag a t) s i			= writeTag "dt" a t s i
serializeTag (EmTag a t) s i			= writeTag "em" a t s i
serializeTag (FieldsetTag a t) s i		= writeTag "fieldset" a t s i
serializeTag (FontTag a t) s i			= writeTag "font" a t s i
serializeTag (FormTag a t) s i			= writeTag "form" a t s i
serializeTag (H1Tag a t) s i			= writeTag "h1" a t s i
serializeTag (H2Tag a t) s i			= writeTag "h2" a t s i
serializeTag (H3Tag a t) s i			= writeTag "h3" a t s i
serializeTag (H4Tag a t) s i			= writeTag "h4" a t s i
serializeTag (H5Tag a t) s i			= writeTag "h5" a t s i
serializeTag (H6Tag a t) s i			= writeTag "h6" a t s i
serializeTag (HeadTag a t) s i			= writeTag "head" a t s i
serializeTag (HrTag a) s i				= writeEmptyTag "hr" a s i
serializeTag (HtmlTag a t) s i			= writeRootTag a t s i
serializeTag (ITag a t) s i				= writeTag "i" a t s i
serializeTag (IframeTag a t) s i		= writeTag "iframe" a t s i
serializeTag (ImgTag a) s i				= writeEmptyTag "img" a s i
serializeTag (InputTag a) s i			= writeEmptyTag "input" a s i
serializeTag (InsTag a t) s i			= writeTag "ins" a t s i
serializeTag (IsindexTag a) s i			= writeEmptyTag "index" a s i
serializeTag (KdbTag a t) s i			= writeTag "kdb" a t s i
serializeTag (LabelTag a t) s i			= writeTag "label" a t s i
serializeTag (LegendTag a t) s i		= writeTag "legend" a t s i
serializeTag (LiTag a t) s i			= writeTag "li" a t s i
serializeTag (LinkTag a t) s i			= writeTag "link" a t s i
serializeTag (MapTag a t) s i			= writeTag "map" a t s i
serializeTag (MenuTag a t) s i			= writeTag "menu" a t s i
serializeTag (MetaTag a t) s i			= writeTag "meta" a t s i
serializeTag (NoframesTag a t) s i		= writeTag "noframes" a t s i
serializeTag (NoscriptTag a t) s i		= writeTag "noscript" a t s i
serializeTag (ObjectTag a t) s i		= writeTag "object" a t s i
serializeTag (OlTag a t) s i			= writeTag "ol" a t s i
serializeTag (OptgroupTag a t) s i		= writeTag "optgroup" a t s i
serializeTag (OptionTag a t) s i		= writeTag "option" a t s i
serializeTag (PTag a t) s i				= writeTag "p" a t s i
serializeTag (ParamTag a t) s i			= writeTag "param" a t s i
serializeTag (PreTag a t) s i			= writeTag "pre" a t s i
serializeTag (QTag a t) s i				= writeTag "q" a t s i
serializeTag (STag a t) s i				= writeTag "s" a t s i
serializeTag (SampTag a t) s i			= writeTag "samp" a t s i
serializeTag (ScriptTag a t) s i		= writeTag "script" a t s i
serializeTag (SelectTag a t) s i		= writeTag "select" a t s i
serializeTag (SmallTag a t) s i			= writeTag "small" a t s i
serializeTag (SpanTag a t) s i			= writeTag "span" a t s i
serializeTag (StrikeTag a t) s i		= writeTag "strike" a t s i
serializeTag (StrongTag a t) s i		= writeTag "strong" a t s i
serializeTag (StyleTag a t) s i			= writeTag "style" a t s i
serializeTag (SubTag a t) s i			= writeTag "sub" a t s i
serializeTag (SupTag a t) s i			= writeTag "sup" a t s i
serializeTag (TableTag a t) s i			= writeTag "table" a t s i
serializeTag (TbodyTag a t) s i			= writeTag "tbody" a t s i
serializeTag (TdTag a t) s i			= writeTag "td" a t s i
serializeTag (TextareaTag a t) s i		= writeTag "textarea" a t s i
serializeTag (TfootTag a t) s i			= writeTag "tfoot" a t s i
serializeTag (ThTag a t) s i			= writeTag "th" a t s i
serializeTag (TheadTag a t) s i			= writeTag "thead" a t s i
serializeTag (TitleTag a t) s i			= writeTag "title" a t s i
serializeTag (TtTag a t) s i			= writeTag "tt" a t s i
serializeTag (TrTag a t) s i			= writeTag "tr" a t s i
serializeTag (UTag a t) s i				= writeTag "u" a t s i
serializeTag (UlTag a t) s i			= writeTag "ul" a t s i
serializeTag (VarTag a t) s i			= writeTag "var" a t s i

serializeTags :: [HtmlTag] *{#Char} Int -> (*{#Char}, Int)
serializeTags [] dest dest_i = (dest,dest_i)
serializeTags [x:xs] dest dest_i
	# (dest, dest_i) = serializeTag x dest dest_i
	= serializeTags xs dest dest_i

serializeAttr :: HtmlAttr *{#Char} Int -> (*{#Char}, Int)
serializeAttr (AbbrAttr a) s i			= writeAttr "abbr" a s i
serializeAttr (AcceptAttr a) s i		= writeAttr "accept" a s i
serializeAttr (AcceptcharsetAttr a) s i	= writeAttr "accept-charset" a s i
serializeAttr (AccesskeyAttr a) s i		= writeAttr "accesskey" a s i
serializeAttr (ActionAttr a) s i		= writeAttr "action" a s i
serializeAttr (AlignAttr a) s i			= writeAttr "align" a s i
serializeAttr (AlinkAttr a) s i			= writeAttr "alink" a s i
serializeAttr (AltAttr a) s i			= writeAttr "alt" a s i
serializeAttr (ArchiveAttr a) s i		= writeAttr "archive" a s i
serializeAttr (AxisAttr a) s i			= writeAttr "axis" a s i
serializeAttr (BackgroundAttr a) s i	= writeAttr "background" a s i
serializeAttr (BgcolorAttr a) s i		= writeAttr "bgcolor" a s i
serializeAttr (BorderAttr a) s i		= writeAttr "border" a s i
serializeAttr (CellspacingAttr a) s i	= writeAttr "cellspacing" a s i
serializeAttr (CellpaddingAttr a) s i	= writeAttr "cellpadding" a s i
serializeAttr (CharAttr a) s i			= writeAttr "char" a s i
serializeAttr (CharoffAttr a) s i		= writeAttr "charoff" a s i
serializeAttr (CharsetAttr a) s i		= writeAttr "charset" a s i
serializeAttr (CheckedAttr) s i			= writeAttr "checked" "checked" s i
serializeAttr (CiteAttr a) s i			= writeAttr "cite" a s i
serializeAttr (ClassAttr a) s i			= writeAttr "class" a s i
serializeAttr (ClassidAttr a) s i		= writeAttr "classid" a s i
serializeAttr (ColorAttr a) s i			= writeAttr "color" a s i
serializeAttr (ColsAttr a) s i			= writeAttr "cols" a s i
serializeAttr (ColspanAttr a) s i		= writeAttr "colspan" a s i
serializeAttr (CodebaseAttr a) s i		= writeAttr "codebase" a s i
serializeAttr (CodetypeAttr a) s i		= writeAttr "codetype" a s i
serializeAttr (ContentAttr a) s i		= writeAttr "content" a s i
serializeAttr (CompactAttr) s i			= writeAttr "compact" "compact" s i
serializeAttr (CoordsAttr a) s i		= writeAttr "coords" a s i
serializeAttr (DataAttr a) s i			= writeAttr "data" a s i
serializeAttr (DatetimeAttr a) s i		= writeAttr "datetime" a s i
serializeAttr (DeclareAttr) s i			= writeAttr "declare" "declare" s i
serializeAttr (DeferAttr a) s i			= writeAttr "defer" a s i
serializeAttr (DirAttr a) s i			= writeAttr "dir" a s i
serializeAttr (DisabledAttr) s i		= writeAttr "disabled" "disabled" s i
serializeAttr (EnctypeAttr a) s i		= writeAttr "enctype" a s i
serializeAttr (FaceAttr a) s i			= writeAttr "face" a s i
serializeAttr (ForAttr a) s i			= writeAttr "for" a s i
serializeAttr (FrameAttr a) s i			= writeAttr "frame" a s i
serializeAttr (FrameborderAttr a) s i	= writeAttr "frameborder" a s i
serializeAttr (HeadersAttr a) s i		= writeAttr "headers" a s i
serializeAttr (HeightAttr a) s i		= writeAttr "height" a s i
serializeAttr (HrefAttr a) s i			= writeAttr "href" a s i
serializeAttr (HreflangAttr a) s i		= writeAttr "hreflang" a s i
serializeAttr (HttpequivAttr a) s i		= writeAttr "http-equiv" a s i
serializeAttr (HspaceAttr a) s i		= writeAttr "hspace" a s i
serializeAttr (IdAttr a) s i			= writeAttr "id" a s i
serializeAttr (IsmapAttr) s i			= writeAttr "ismap" "ismap" s i
serializeAttr (LabelAttr a) s i			= writeAttr "label" a s i
serializeAttr (LangAttr a) s i			= writeAttr "lang" a s i
serializeAttr (LanguageAttr a) s i		= writeAttr "language" a s i
serializeAttr (LinkAttr a) s i			= writeAttr "link" a s i
serializeAttr (LongdescAttr a) s i		= writeAttr "longdesc" a s i
serializeAttr (MarginheightAttr a) s i	= writeAttr "marginheight" a s i
serializeAttr (MarginwidthAttr a) s i	= writeAttr "marginwidth" a s i
serializeAttr (MaxlengthAttr a) s i		= writeAttr "maxlength" a s i
serializeAttr (MediaAttr a) s i			= writeAttr "media" a s i
serializeAttr (MethodAttr a) s i		= writeAttr "method" a s i
serializeAttr (MultipleAttr) s i		= writeAttr "multiple" "multiple" s i
serializeAttr (NameAttr a) s i			= writeAttr "name" a s i
serializeAttr (NohrefAttr) s i			= writeAttr "nohref" "nohref" s i
serializeAttr (NoshadeAttr) s i			= writeAttr "noshade" "noshade" s i
serializeAttr (NowrapAttr) s i			= writeAttr "nowrap" "nowrap" s i
serializeAttr (OnblurAttr a) s i		= writeAttr "onblur" a s i
serializeAttr (OnchangeAttr a) s i		= writeAttr "onchange" a s i
serializeAttr (OnclickAttr a) s i		= writeAttr "onclick" a s i
serializeAttr (OndblclickAttr a) s i	= writeAttr "ondblclick" a s i
serializeAttr (OnfocusAttr a) s i		= writeAttr "onfocus" a s i
serializeAttr (OnloadAttr a) s i		= writeAttr "onload" a s i
serializeAttr (OnmousedownAttr a) s i	= writeAttr "onmousedown" a s i
serializeAttr (OnmousemoveAttr a) s i	= writeAttr "onmousemove" a s i
serializeAttr (OnmouseoutAttr a) s i	= writeAttr "onmouseout" a s i
serializeAttr (OnmouseoverAttr a) s i	= writeAttr "onmouseover" a s i
serializeAttr (OnmouseupAttr a) s i		= writeAttr "onmouseup" a s i
serializeAttr (OnkeydownAttr a) s i		= writeAttr "onkeydown" a s i
serializeAttr (OnkeypressAttr a) s i	= writeAttr "onkeypress" a s i
serializeAttr (OnkeyupAttr a) s i		= writeAttr "onkeyup" a s i
serializeAttr (OnresetAttr a) s i		= writeAttr "onreset" a s i
serializeAttr (OnselectAttr a) s i		= writeAttr "onselect" a s i
serializeAttr (OnsubmitAttr a) s i		= writeAttr "onsubmit" a s i
serializeAttr (OnunloadAttr a) s i		= writeAttr "onunload" a s i
serializeAttr (ProfileAttr a) s i		= writeAttr "profile" a s i
serializeAttr (PromptAttr a) s i		= writeAttr "prompt" a s i
serializeAttr (ReadonlyAttr) s i		= writeAttr "readonly" "readonly" s i
serializeAttr (RelAttr a) s i			= writeAttr "rel" a s i
serializeAttr (RevAttr a) s i			= writeAttr "rev" a s i
serializeAttr (RowsAttr a) s i			= writeAttr "rows" a s i
serializeAttr (RowspanAttr a) s i		= writeAttr "rowspan" a s i
serializeAttr (RulesAttr a) s i			= writeAttr "rules" a s i
serializeAttr (SchemeAttr a) s i		= writeAttr "scheme" a s i
serializeAttr (ScopeAttr a) s i			= writeAttr "scope" a s i
serializeAttr (ScrollingAttr a) s i		= writeAttr "scrolling" a s i
serializeAttr (SelectedAttr) s i		= writeAttr "selected" "selected" s i
serializeAttr (ShapeAttr a) s i			= writeAttr "shape" a s i
serializeAttr (SizeAttr a) s i			= writeAttr "size" a s i
serializeAttr (SpanAttr a) s i			= writeAttr "span" a s i
serializeAttr (SrcAttr a) s i			= writeAttr "src" a s i
serializeAttr (StandbyAttr a) s i		= writeAttr "standby" a s i
serializeAttr (StartAttr a) s i			= writeAttr "start" a s i
serializeAttr (StyleAttr a) s i			= writeAttr "style" a s i
serializeAttr (SummaryAttr a) s i		= writeAttr "summary" a s i
serializeAttr (TabindexAttr a) s i		= writeAttr "tabindex" a s i
serializeAttr (TargetAttr a) s i		= writeAttr "target" a s i
serializeAttr (TextAttr a) s i			= writeAttr "text" a s i
serializeAttr (TitleAttr a) s i			= writeAttr "title" a s i
serializeAttr (TypeAttr a) s i			= writeAttr "type" a s i
serializeAttr (UsemapAttr a) s i		= writeAttr "usemap" a s i
serializeAttr (ValignAttr a) s i		= writeAttr "valign" a s i
serializeAttr (ValueAttr a) s i			= writeAttr "value" a s i
serializeAttr (ValuetypeAttr a) s i		= writeAttr "valuetype" a s i
serializeAttr (VlinkAttr a) s i			= writeAttr "vlink" a s i
serializeAttr (VspaceAttr a) s i		= writeAttr "vspace" a s i
serializeAttr (WidthAttr a) s i			= writeAttr "width" a s i
serializeAttr (XmllangAttr a) s i		= writeAttr "xml:lang" a s i
serializeAttr (XmlspaceAttr a) s i		= writeAttr "xml:space" a s i

serializeAttrs :: [HtmlAttr] *{#Char} Int -> (*{#Char}, Int)
serializeAttrs [] dest dest_i = (dest, dest_i)
serializeAttrs [x:xs] dest dest_i
	# (dest, dest_i) = serializeAttr x dest dest_i
	= serializeAttrs xs dest dest_i

copyChars :: !{#Char} !Int !Bool !*{#Char} !Int -> (!*{#Char},!Int)
copyChars src src_i escape dest dest_i
	| src_i == (size src) = (dest, dest_i)
	| otherwise	
		| escape && (src.[src_i] == '<')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'l', [dest_i + 2] = 't', [dest_i + 3] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 4)
		| escape && (src.[src_i] == '>')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'g', [dest_i + 2] = 't', [dest_i + 3] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 4)
		| escape && (src.[src_i] == '&')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'a', [dest_i + 2] = 'm', [dest_i + 3] = 'p', [dest_i + 4] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 5)
		| otherwise
			# dest = {dest & [dest_i] = src.[src_i]}
			= copyChars src (src_i + 1) escape dest (dest_i + 1)

writeTag :: !{#Char} [HtmlAttr] [HtmlTag] !*{#Char} !Int -> (!*{#Char},!Int)
writeTag name attrs tags dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = copyChars name 0 False dest dest_i
	# (dest, dest_i) = serializeAttrs attrs dest dest_i
	# dest = {dest & [dest_i] = '>'}
	# dest_i = dest_i + 1
	# (dest, dest_i) = serializeTags tags dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest = {dest & [dest_i + 1] = '/'}
	# dest_i = dest_i + 2
	# (dest,dest_i) = copyChars name 0 False dest dest_i
	# dest = {dest & [dest_i] = '>'}
	# dest_i = dest_i + 1
	= (dest,dest_i)

writeEmptyTag :: !{#Char} [HtmlAttr] !*{#Char} !Int -> (!*{#Char},!Int)
writeEmptyTag name attrs dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = copyChars name 0 False dest dest_i
	# (dest, dest_i) = serializeAttrs attrs dest dest_i
	# dest = {dest & [dest_i] = '/'}
	# dest_i = dest_i + 1
	# dest = {dest & [dest_i] = '>'}
	# dest_i = dest_i + 1
	= (dest,dest_i)

writeRootTag :: [HtmlAttr] [HtmlTag] !*{#Char} !Int -> (!*{#Char},!Int)
writeRootTag attrs tags dest dest_i
	# (dest,dest_i) = copyChars "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" 0 False dest dest_i
	# (dest,dest_i) = copyChars "<html xmlns=\"http://www.w3.org/1999/xhtml\"" 0 False dest dest_i
	# (dest, dest_i) = serializeAttrs attrs dest dest_i
	# dest = {dest & [dest_i] = '>'}
	# dest_i = dest_i + 1
	# (dest, dest_i) = serializeTags tags dest dest_i
	# (dest, dest_i) = copyChars "</html>" 0 False dest dest_i
	= (dest,dest_i)

writeAttr :: !{#Char} !{#Char} !*{#Char} !Int -> (!*{#Char},!Int)
writeAttr name value dest dest_i
	# dest = {dest & [dest_i] = ' '}
	# dest_i = dest_i + 1
	# (dest,dest_i) = copyChars name 0 False dest dest_i
	# dest = {dest & [dest_i] = '='}
	# dest_i = dest_i + 1
	# dest = {dest & [dest_i] = '"'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = copyChars value 0 True dest dest_i
	# dest = {dest & [dest_i] = '"'}
	# dest_i = dest_i + 1
	= (dest,dest_i)

class html a 
where
	html :: !a -> HtmlTag
	
instance html String
where
	html s = Text s

instance html HtmlTag
where
	html h = h
	
instance html [a] | html a
where
	html [h]	= html h
	html h		= SpanTag [] (map html h)
		
instance html (Maybe a) | html a
where
	html Nothing	= SpanTag [] []
	html (Just h)	= html h
