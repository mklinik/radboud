implementation module SmallExamples

import iTasks

smallExamples :: [Workflow]
smallExamples = [workflow	"Examples/Miscellaneous/Calculate sum" "Calculate the sum of two numbers" calculateSum
				,workflow	"Examples/Miscellaneous/Calculate sum (with stepback)" "Calculate the sum of two numbers, with stepback possibility" calculateSumSteps
				,workflow	"Examples/Miscellaneous/Calculate sum (parameterised)" "Calculate the sum of two numbers given as parameter" calculateSumParam
				]

calculateSum :: Task Int
calculateSum
  =   enterInformation ("Number 1","Enter a number") []
  >>= \num1 ->
      enterInformation ("Number 2","Enter another number") []
  >>= \num2 ->
      viewInformation ("Sum","The sum of those numbers is:") [] (num1 + num2)
        
calculateSumSteps :: Task Int
calculateSumSteps = step1First
where
	step1First			=	enterInformation ("Number 1","Enter a number") []
						>>*	[WithResult ActionNext (const True) (\num -> step2First num)]
						  
	step1Back num1		=	updateInformation ("Number 1","Enter a number") [] num1
						>>*	[WithResult ActionNext (const True) (\num -> step2First num)]
	
	step2First num1		=	enterInformation ("Number 2","Enter another number") []
						>>*	[AnyTime ActionPrevious (\_ -> step1Back num1)
						  	,WithResult ActionNext (const True) (\num2 -> step3 num1 num2)
						  	]
						  							
	step2Back num1 num2	=	updateInformation ("Number 2","Enter another number") [] num2
						>>*	[AnyTime ActionPrevious	(\_ -> step1Back num1)
							,WithResult ActionNext (const True) (\num2` -> step3 num1 num2`)
							]
	
	step3 num1 num2		=	viewInformation("Sum","The sum of those numbers is:") [] (num1 + num2)
						>>*	[AnyTime ActionPrevious (\_ -> step2Back num1 num2)
							,WithResult ActionOk (const True) return
							]

calculateSumParam :: !(Int,Int) -> Task Int
calculateSumParam (num1,num2) = viewInformation ("Sum","The sum of those numbers is:") [] (num1 + num2)
