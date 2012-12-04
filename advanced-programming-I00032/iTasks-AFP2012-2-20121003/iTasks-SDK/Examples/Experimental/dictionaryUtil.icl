implementation module dictionaryUtil

import iTasks

:: Dictionary_iData a = Dictionary_iData a

class BoxediData a | iData a

get_iDataDictionaries :: a -> (!Dictionary_iData a,a) | BoxediData a
get_iDataDictionaries a = code {
pop_a 0
}

get_iDataDictionaries2Dyn ::  a -> Dynamic | BoxediData a
get_iDataDictionaries2Dyn val = dynamic (get_iDataDictionaries val)

iDataFun2Dynamic :: (A.a: (Dictionary_iData a) -> (b -> Task a)) -> Dynamic | TC b
iDataFun2Dynamic f = dynamic f :: (A.a: (Dictionary_iData a) -> (b^ -> Task a))

(OO) infixr 9:: (dict a -> b) (dict -> a) -> (dict -> b) 
(OO) dfun darg = applyTask`
where
	applyTask` dictonary
	# fun = dfun dictonary
	# arg = darg dictonary
	= fun arg

applyDynamicTask :: Dynamic (Dictionary_iData b,a) -> Task b | iData b & TC a
applyDynamicTask 
	(edit::A.c: (Dictionary_iData c) -> (a^ -> Task c)) (dict,val) = edit dict val 
applyDynamicTask _ _ = return createDefault

applyDynamicTask2 :: Dynamic a -> Task a | BoxediData a
applyDynamicTask2 
	(edit::A.c: (Dictionary_iData c) -> (a^ -> Task c)) val
	# (dict,val) = get_iDataDictionaries val
	= edit dict val 
applyDynamicTask2 _ _ = return createDefault


f :: !(A.a: a | BoxediData a) -> ((Dictionary_iData a) -> a)
f g = code {
	pop_a 0
	}

h :: !((Dictionary_iData a) -> a) -> a | BoxediData a
h g = code {
	pop_a 0
	}

/*
// ***************************************

iTaskEditor :: (a -> Task a) | BoxediData a
iTaskEditor = \a -> editTask "Editor" a

d_iTaskEditor :: !(Dictionary_iData a) -> (a -> Task a)
d_iTaskEditor fun = code {
    .d 2 0
          jmp e_dictionaryUtil_siTaskEditor
    .o 1 0
} 

// ***************************************

iTaskDelegate :: ((a -> Task b) a -> Task b) | BoxediData b
iTaskDelegate = \ataskb vala -> delegateTask ataskb vala
where
	delegateTask ataskb vala
	=							[Text "Choose persons you want to delegate work to:",BrTag [],BrTag []] 
								?>>	chooseUser
		>>= \(wid,worker) -> 	getCurrentUser
		>>= \(_,me) ->			wid @: ("Task for " +++ me, ataskb vala)
		>>= \result -> 			[Text ("Result from " +++ worker), toHtml result] 
								?>> editTask "OK" Void 
		>>= \_ ->				return result


d_iTaskDelegate :: !(Dictionary_iData b) -> ((a -> Task b) a -> Task b)
d_iTaskDelegate fun = code {
    .d 2 0
          jmp e_dictionaryUtil_siTaskDelegate
    .o 1 0
} 

*/