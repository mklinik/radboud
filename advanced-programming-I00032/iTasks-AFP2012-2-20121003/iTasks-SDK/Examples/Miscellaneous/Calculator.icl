implementation module Calculator

import iTasks

calculatorExample :: [Workflow]
calculatorExample = [workflow "Examples/Miscellaneous/Calculator" "A simple calculator demonstrating how to layout buttons." calculator]

calculator :: Task Int
calculator = (updateInformation "Calculator" views initSt >>* [quit]) /* <<@ calculatorLayout */
where
	initSt =	{ display		= 0
				, x				= 0
				, y				= 0
				, op			= +
				, showsResult	= False
				}
				
	views =[]
		//[ DisplayView (GetLocal (\{display} -> display))
		/*
		, UpdateTrigger "7" (UpdateLocal (enterDigit 7))
		, UpdateTrigger "8" (UpdateLocal (enterDigit 8))
		, UpdateTrigger "9" (UpdateLocal (enterDigit 9))
		, UpdateTrigger "C" (UpdateLocal (const initSt))
		, UpdateTrigger "4" (UpdateLocal (enterDigit 4))
		, UpdateTrigger "5" (UpdateLocal (enterDigit 5))
		, UpdateTrigger "6" (UpdateLocal (enterDigit 6))
		, UpdateTrigger "/" (UpdateLocal (calc (/) False))
		, UpdateTrigger "1" (UpdateLocal (enterDigit 1))
		, UpdateTrigger "2" (UpdateLocal (enterDigit 2))
		, UpdateTrigger "3" (UpdateLocal (enterDigit 3))
		, UpdateTrigger "*" (UpdateLocal (calc (*) False))
		, UpdateTrigger "0" (UpdateLocal (enterDigit 0))
		, UpdateTrigger "+" (UpdateLocal (calc (+) False))
		, UpdateTrigger "-" (UpdateLocal (calc (-) False))
		, UpdateTrigger "=" (UpdateLocal (\st -> calc st.op True st))
		*/
		//]
	where
		enterDigit d st = {st & display = newV, y = newV, showsResult = False}
		where
			newV = if st.showsResult d (st.display*10 + d)
				
		calc nop alwaysCalc st =	{ st
									& display		= v
									, x				= v
									, op			= nop
									, showsResult	= True
									}
		where
			v = if (not st.showsResult || alwaysCalc) (st.op st.x st.y) st.display
			
	quit = WithResult ActionQuit (const True) (\st -> return st.x)
	
	/*
	calculatorLayout {TUIInteraction|title,content=[display:stButtons],actions}
		# (buttons,actions) = defaultButtons actions
		= ( defaultFormPanel
			title
			""
			(WrapContent 0)
			(defaultContent [display,columnLayout 4 stButtons] buttons), actions)
	*/
:: CalculatorState =	{ display		:: !Int
						, x				:: !Int
						, y				:: !Int
						, op			:: !(Int Int -> Int)
						, showsResult	:: !Bool
						}
derive class iTask CalculatorState
