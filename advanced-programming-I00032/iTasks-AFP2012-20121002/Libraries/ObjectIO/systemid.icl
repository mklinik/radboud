implementation module systemid


import	StdInt, StdBool, StdOverloaded


::	SystemId
	=	SystemId [Int] Int


worldSystemId :: !Int -> SystemId	// This systemid is always associated with the World
worldSystemId nrCreated = SystemId [0] nrCreated

worldChildId :: !Int -> SystemId	// This systemid is used for creating systemid from World
worldChildId nrCreated = SystemId [nrCreated,0] 0

initSystemId :: SystemId			// This systemid is always associated with the initial IOState
initSystemId = SystemId [1] 0

nullSystemId :: SystemId			// Dummy systemid
nullSystemId = SystemId [] 0

incrSystemId :: !SystemId -> (!SystemId,!SystemId)
incrSystemId id=:(SystemId idpath nrCreated)
	= (SystemId idpath idmax`,SystemId [idmax`:idpath] 0)
where
	idmax`	= nrCreated+1

instance == SystemId where
	(==) (SystemId idpath1 _) (SystemId idpath2 _)
			= eqidpath idpath1 idpath2
		 where
			eqidpath [x:xs] [y:ys]	= x==y && eqidpath xs ys
			eqidpath []		[]		= True
			eqidpath _		_		= False

instance < SystemId where
	(<) (SystemId idpath1 _) (SystemId idpath2 _)
			= lessidpath ids1 ids2
		where
			(ids1,ids2)		= removecommonprefix idpath1 idpath2
			
			removecommonprefix xs`=:[x:xs] ys`=:[y:ys]
				| x==y		= removecommonprefix xs ys
				| otherwise	= (xs`,ys`)
			removecommonprefix xs ys
				= (xs,ys)
			
			lessidpath [x:xs]	[y:ys]	= x<y
			lessidpath []		[_:_]	= True
			lessidpath _		_		= False
