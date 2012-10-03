implementation module menudefaccess

import StdBool, StdMisc, StdTuple
import StdMenuAttribute, commondef


menuDefGetMenuId :: !(Menu m .ls .pst) -> (!Maybe Id, !Menu m .ls .pst)
menuDefGetMenuId menu=:(Menu _ _ atts)
	# (hasIdAtt,idAtt)	= cselect isMenuId undef atts
	| not hasIdAtt		= (Nothing,menu)
	| otherwise			= (Just (getMenuIdAtt idAtt), menu)

menuDefGetSelectState :: !(Menu m .ls .pst) -> (!SelectState, !Menu m .ls .pst)
menuDefGetSelectState menu=:(Menu _ _ atts)
	= (getMenuSelectStateAtt (snd (cselect isMenuSelectState (MenuSelectState Able) atts)), menu)

menuDefSetAbility :: !(Menu m .ls .pst) !SelectState -> Menu m .ls .pst
menuDefSetAbility (Menu name items atts) able
	= Menu name items (setselectstate able atts)
where
	setselectstate :: !SelectState ![MenuAttribute .pst] -> [MenuAttribute .pst]
	setselectstate select atts
		| found			= atts1
		| otherwise		= [att:atts1]
	where
		att				= MenuSelectState select
		(found,atts1)	= creplace isMenuSelectState att atts

menuDefGetTitle :: !(Menu m .ls .pst) -> (!Title, !Menu m .ls .pst)
menuDefGetTitle menu=:(Menu name _ _) = (name, menu)

menuDefGetIndex :: !(Menu m .ls .pst) -> (!Maybe Index,!Menu m .ls .pst)
menuDefGetIndex menu=:(Menu _ _ atts)
	# (hasIndexAtt,indexAtt)	= cselect isMenuIndex undef atts
	| not hasIndexAtt			= (Nothing,menu)
	| otherwise					= (Just (getMenuIndexAtt indexAtt),menu)
