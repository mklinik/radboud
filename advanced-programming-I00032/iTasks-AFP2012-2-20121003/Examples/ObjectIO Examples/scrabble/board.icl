implementation module board

import	StdBool, StdOrdList, StdTuple
import	language, types

/***************************************************************************************************************
	The dimensions of the board have range of 0..14 to 0..14.
****************************************************************************************************************/

::	Board	:==	(![[Char]],![[Char]])

initboard :: Board
initboard
	= (emptyboard,emptyboard)
where
	emptyboard = repeatn 15 (repeatn 15 ' ')

getplacedletters :: !Board -> [Char]
getplacedletters (h,_)
	= sort (removeDup (flatten h))


/***************************************************************************************************************
	The bonus fields on the scrabble board.
****************************************************************************************************************/

doubleletterpositions :: [Position]
doubleletterpositions
	=: [(0,3),(0,11),(2,6),(2,8),(3,7),(3,14),(6,2),(6,6),(6,8),(6,12),(7,3),(7,11),(8,2),(8,6),(8,8),(8,12),(11,7),(11,14),(12,6),(12,8),(14,3),(14,11)]

tripleletterpositions :: [Position]
tripleletterpositions
	=: [ (1,5),(1,9),(5,1),(5,5),(5,9),(5,13),(9,1),(9,5),(9,9),(9,13),(13,5),(13,9) ]

doublewordpositions :: [Position]
doublewordpositions
	=: [ (7,7),(1,1),(2,2),(3,3),(4,4),(1,13),(2,12),(3,11),(4,10),(10,4),(10,10),(11,3),(11,11),(12,12),(12,2),(13,13),(13,1) ]

triplewordpositions :: [Position]
triplewordpositions
	=: [ (0,0),(0,7),(0,14),(7,0),(7,14),(14,0),(14,7),(14,14) ]

lettervalueat :: !Char !Position -> Int
lettervalueat l pos
	| isMember pos doubleletterpositions	= lw*2
	| isMember pos tripleletterpositions	= lw*3
	| otherwise								= lw
where
	lw = lettervalue l

wordvalueat :: !Position -> Int
wordvalueat pos
	| isMember pos doublewordpositions	= 2
	| isMember pos triplewordpositions	= 3
	| otherwise							= 1

grab :: ![Char] !Int [Int] -> (![Char],![Char],[Int])
grab lb 0 rs
	= (lb,[],rs)
grab [] n rs
	= ([],[],rs)
grab lb n rs
	= (restbox,[l:rest],rs2)
where
	(r,rs1)				= random (length lb) rs
	l					= lb!!r
	(restbox,rest,rs2)	= grab (removeFirst l lb) (n-1) rs1
	
	random :: !Int ![Int] -> (!Int,![Int])
	random i [r:rs] = (r rem i,rs)
	
	removeFirst :: !x ![x] -> [x]	| Eq x
	removeFirst x [y:ys]
		| x==y		= ys
		| otherwise	= [y:removeFirst x ys]
	removeFirst _ _
		= []

getfreehorpositions :: !Board !Char -> [Position]
getfreehorpositions (h,_) l
	= flatten (map (freepositions True l 0) (zip2 [0..] h))

getfreeverpositions :: !Board !Char -> [Position]
getfreeverpositions (_,v) l
	= map swap (flatten (map (freepositions True l 0) (zip2 [0..] v)))

freepositions :: !Bool !Char !Int !(!Int,![Char]) -> [Position]
freepositions _ _ _ (_,[])
	= []
freepositions True k x (y,[l:ls])
	| l==k		= [(x,y) : freepositions False k (x+1) (y,ls)]
	| l==' '	= freepositions True  k (x+1) (y,ls)
	| otherwise	= freepositions False k (x+1) (y,ls)
freepositions False k x (y,[l:ls])
	| l==' '	= freepositions True  k (x+1) (y,ls)
	| otherwise	= freepositions False k (x+1) (y,ls)


/***************************************************************************************************************
	seekfreepositions is used by seekfree(hor/ver)positions. It determines the positions on a given board that 
	are valid starting positions for a word starting with a particular letter.
	
	The function is useful when atleast one letter on the board has to be used because a (some) letter(s) is 
	(are) missing on the letter bar.
****************************************************************************************************************/
seekfreehorpositions :: !Board !Char !Int -> [Position]
seekfreehorpositions (h,v) k p
	= flatten (map (seekfreepositions 0 p k 0) (zip2 [0..] h))

swap :: !(.a,.b) -> (.b,.a)
swap (a,b) = (b,a)

seekfreeverpositions :: !Board !Char !Int -> [Position]
seekfreeverpositions (h,v) k p
	= map swap (flatten (map (seekfreepositions 0 p k 0) (zip2 [0..] v) ))

seekfreepositions :: !Int !Int !Char !Int !(!Int,![Char]) -> [Position]
seekfreepositions a p k x (y,[l:ls])
	| a>=p && k==l		= [(x-p,y) : seekfreepositions 0 p k (x+1) (y,ls)]
	| l==' '			= seekfreepositions (a+1) p k (x+1) (y,ls)
	| otherwise			= seekfreepositions 0 p k (x+1) (y,ls)
seekfreepositions _ _ _ _ _
	= []


/***************************************************************************************************************
	tryaddword board word position direction adds word at position in direction to board. 
	The Board	result is the new board.
	The Boolean	result reports whether the word could be placed.
	The [Char]	result are the letters that have been used.
	The Int		result is the score by placing this word.
	The [Word]	result are the possibly new formed words.
	
	After tryaddword it should be verified if the new formed words are legal. 
	After tryaddword it also should be verified if a bonus should be added to the score in case all letters 
	have been used.
****************************************************************************************************************/
tryaddword :: !Board !Word !Position !Direction -> (!Board,!Bool,[Char],Int,[Word])
tryaddword board=:(h,v) w (x,y) Horizontal
	| w==""				= (board,True, [],0,[])
	| size w+x>15		= (board,False,[],0,[])
	| otherwise			= ((nh,nv),possible,newletters,score,newwords)
where
	(nh,possible,scorenewletters,worddoubling)
						= tryaddtolines h w (x,y) y
	(nv,vscore,newwords)= tryaddtransversetolines v (0,y) scorenewletters
	newletters			= map fst3 scorenewletters
	scoreoldletters		= oldletterscore w newletters
	hscore				= worddoubling*(sum (map thd3 scorenewletters) + scoreoldletters)
	score
		| not possible	= 0
		| otherwise		= hscore+vscore
tryaddword board=:(h,v) w (x,y) Vertical
	| w==""				= (board,True, [],0,[])
	| size w+y>15		= (board,False,[],0,[])
	| otherwise			= ((nh,nv),possible,newletters,vscore+hscore,newwords)
where
	(nv,possible,scorenewletters,worddoubling)
						= tryaddtolines v w (y,x) x
	(nh,hscore,newwords)= tryaddtransversetolines h (0,x) scorenewletters
	newletters			= map fst3 scorenewletters
	scoreoldletters		= oldletterscore w newletters
	vscore				= worddoubling*(sum (map thd3 scorenewletters) + scoreoldletters)

oldletterscore :: !Word ![Char] -> Int		// Sum the lettervalue of the chars in word that are not member of letters
oldletterscore word letters
	= sum (map lettervalue (removeMembers [c\\c<-:word] letters))

tryaddtolines :: ![[Char]] Word !Position !Int -> (![[Char]],!Bool,![(Char,Int,Int)],Int)
tryaddtolines [r:rs] w p=:(x,_) 0
	# (r,possible,scorenewletters,worddoubling) = tryaddtoline r w p x
	= ([r:rs],possible,scorenewletters,worddoubling)
where
	tryaddtoline :: ![Char] !Word !Position !Int -> (![Char],!Bool,![(Char,Int,Int)],Int)
	tryaddtoline rs word p 0
		| nrchars==1
		= addtoline
		with
			addtoline
				|	l==' ' && (ls==[] || hd ls==' ')= ([w:ls],True, [lwrd],wwrd)
				|	l==w   && (ls==[] || hd ls==' ')= ([w:ls],True, [],1)
				|	otherwise						= ([l:ls],False,[],1)
		| nrchars>=1
		= addtoline
		with
			(nr,possible,lwrds,wwrds)				= tryaddtoline ls (word%(1,nrchars-1)) (x+1,y) 0
			addtoline
				| l==' '							= ([w:nr],possible,[lwrd:lwrds],wwrd*wwrds)
				| l==w								= ([w:nr],possible,lwrds,wwrds)
				| otherwise							= ([l:ls],False,[],1)
	where
		nrchars		= size word
		(l,ls)		= hdtl rs
		w			= word.[0]
		(x,y)		= p
		wvalue		= lettervalueat w p
		lwrd		= (w,x,wvalue)
		wwrd		= wordvalueat p
	tryaddtoline rs word p 1
		| l<>' '					= (rs,False,[],1)
		# (ls,possible,lwrds,wwrds)	= tryaddtoline ls word p 0
		| otherwise					= ([l:ls],possible,lwrds,wwrds)
	where
		(l,ls)						= hdtl rs
	tryaddtoline [l:ls] word p n
		# (ls,possible,lwrds,wwrds)	= tryaddtoline ls word p (n-1)
		= ([l:ls],possible,lwrds,wwrds)
	
	hdtl :: !v:[u:x] -> v:(u:x,v:[u:x]), [v<=u]
	hdtl [x:xs] = (x,xs)
tryaddtolines [r:rs] w p j
	# (rs,possible,scorenewletters,worddoubling) = tryaddtolines rs w p (j-1)
	= ([r:rs],possible,scorenewletters,worddoubling)
tryaddtolines _ _ _ _
	= ([],False,[],0)

tryaddtransversetolines :: ![[Char]] !Position ![(Char,Int,Int)] -> (![[Char]],Int,![Word])
tryaddtransversetolines rs _ []
	= (rs,0,[])
tryaddtransversetolines [r:rs] p=:(x,y) [(nl,nx,ls)]
	| x==nx
	= ([nr:rs],score,newword)
	with
		(nr,score,newword)	= addtoline r nl p y ""
	| otherwise	= ([r:nrs],score,newword)
	with
		(nrs,score,newword)	= tryaddtransversetolines rs (x+1,y) [(nl,nx,ls)]
tryaddtransversetolines [r:rs] p=:(x,y) [(nl,nx,ls):lwrds]
	| x==nx
	= ([nr:nrs],score+restscore,newword++newwords)
	with
		(nrs,restscore,newwords)= tryaddtransversetolines rs (x+1,y) lwrds
		(nr,score,newword)		= addtoline r nl p y ""
	| otherwise
	= ([r:nrs],restscore,newwords)
	with
		(nrs,restscore,newwords)= tryaddtransversetolines rs (x+1,y) [(nl,nx,ls):lwrds]
tryaddtransversetolines [r:rs] (x,y) lwrds
	= ([r:nrs],score,newwords)
where
	(nrs,score,newwords) = tryaddtransversetolines rs (x-1,y) lwrds

addtoline :: ![Char] !Char Position !Int !Word -> (![Char],Int,![Word])
addtoline [l:ls] w p 0 initword
	| initwordscore+restwordscore<>0
	= ([w:ls],(initwordscore+lscore+restwordscore)*wscore,[word])
	| otherwise
	= ([w:ls],0,[])
where
	lscore				= lettervalueat w p
	wscore				= wordvalueat p
	restword			= takeWhile ((<>) ' ') ls
	initwordscore		= sum [lettervalue c \\ c<-:initword]
	restwordscore		= sum (map lettervalue restword)
	word				= initword +++ toString w +++ toString restword
addtoline [l:ls] w p ry initword
	# initword			= if (l<>' ') (initword+++toString l) ""
	  (ls,score,word)	= addtoline ls w p (ry-1) initword
	= ([l:ls],score,word)


/***************************************************************************************************************
	newmaximumplacings _ lexicon letterbar _ (Letter l _) _ _ determines all valid words from lexicon that 
	start with l and are member of letterbar.
****************************************************************************************************************/
newmaximumplacings :: !Board Tree [Char] !(!Int,!Int,!Int,!Int) !Progress !Strength Bool -> Placing
newmaximumplacings board wordlist computerletters (minx,maxx,miny,maxy) progress=:(Letter l placing) strength firstturn
	= scoremax strength [placing:newfoundplacings1++newfoundplacings2]
where
	startwith				= membersStartingWith l wordlist
	uniquecomputerletters	= removeDup computerletters
	poshor					= getfreehorpositions board l
	posver					= getfreeverpositions board l
	newfoundplacings1
		= [ {word=nw,pos=p,dir=r,/*endscore*/score=if (length gl==7) (s+50) s}
								\\	nw	<- startwith
								,	let (firstmissingletter,position) = difference nw uniquecomputerletters 0
								,	r <- [Horizontal,Vertical]
								,	p <- if (position<>7) (if (r==Horizontal)
															(seekfreehorpositions board firstmissingletter position)
															(seekfreeverpositions board firstmissingletter position)
														  )
														  (if (r==Horizontal)
															[(i,j) \\ i <- [max 0 (minx-size nw) .. min (14-size nw) maxx]
																	, j <- [max 0 (miny-1)       .. min 14 (maxy+1)]]
															[(i,j) \\ i <- [max 0 (minx-1)       .. min 14 (maxx+1)]
																	, j <- [max 0 (miny-size nw) .. min (14-size nw) maxy]]
														  )
								,	let (_,m,gl,s,nws) = tryaddword board nw p r
								|	ok_solution m gl nws nw
		  ]
	ok_solution m gl nws nw
		= m															&& 
		  (not (isEmpty gl))										&&
		  isEmpty (removeMembers gl computerletters)				&&
		  ((not (isEmpty nws)) || length gl<>size nw || firstturn)	&&
		  allexist wordlist nws
	newfoundplacings2
		= addatpositions board wordlist computerletters (poshor,posver) progress firstturn
	
//	difference word letterbar determines which letter and its position in word that is not a member of letterbar. 
	difference :: !Word ![Char] !Int -> (!Char,!Int)
	difference word letters p
		| word==""
		= ('a',7)
		# l					= word.[0]
		  word				= word%(1,size word-1)
		| isMember l letters
		= difference word letters (p+1)
		| otherwise
		= (l,p)


/***************************************************************************************************************
	newmaximumplacing board lexicon letterbar (hor,ver) (Letter l _) _ _ determines all valid words from lexicon 
	that start with l and are not member of letterbar. The positions hor++ver are assumed to be valid free 
	positions on board starting with l.
****************************************************************************************************************/
newmaximumplacing :: !Board Tree [Char] ([Position],[Position]) !Progress !Strength Bool -> Placing
newmaximumplacing board wordlist computerletters poshv progress=:(Letter l placing) strength firstturn
	= scoremax strength [placing:addatpositions board wordlist computerletters poshv progress firstturn]
newmaximumplacing _ _ _ _ (Finish ready) _ _
	= ready

addatpositions :: !Board Tree [Char] !([Position],[Position]) !Progress Bool -> [Placing]
addatpositions board wordlist computerletters (poshor,posver) (Letter l _) firstturn
	= [ {word=nw,pos=p,dir=r,/*endscore*/score=if (length gl==7) (s+50) s}
							\\	nw<- startwith
							|	wordcontainsletters nw uniquecomputerletters
							,	r <- [Horizontal,Vertical]
							,	p <- if (r==Horizontal) poshor posver
							,	let (_,m,gl,s,nws) = tryaddword board nw p r
							|	ok_solution m gl nws nw
	]
where
	ok_solution m gl nws nw
		= m															&&
		  not (isEmpty gl)											&&
		  isEmpty (removeMembers gl computerletters)				&&
		  (not (isEmpty nws) || length gl<>size nw || firstturn)	&&
		  allexist wordlist nws
	startwith				= membersStartingWith l wordlist
	uniquecomputerletters	= removeDup computerletters
	
	wordcontainsletters :: !Word [Char] -> Bool
	wordcontainsletters word letters
		| word==""					= False
		| isMember word.[0] letters	= True
		| otherwise					= wordcontainsletters (word%(1,size word-1)) letters


/***************************************************************************************************************
	scoremax selects a Placing depending on the Strength of the player.
****************************************************************************************************************/
scoremax :: !Strength ![Placing] -> Placing
scoremax First ps
	# ps	= dropWhile ((==) initplacing) ps
	| isEmpty ps
	= initplacing
	| otherwise
	= hd ps
scoremax Maximum ps
	= getmaxscore ps
where
	getmaxscore :: ![Placing] -> Placing
	getmaxscore [p]			= p
	getmaxscore [p1:ps]
		| p1.score>p2.score
		= p1
		| otherwise
		= p2
	where
		p2	= getmaxscore ps
	getmaxscore []
		= initplacing
scoremax (Strength percent) ps
	= scoremax Maximum (take nr ps)
where
	nr	= toInt (toReal (length ps)*percent) + 1


/***************************************************************************************************************
	allexists is true only if each of the words in the [Word] argument can be found in the Tree argument.
****************************************************************************************************************/
allexist :: Tree ![Word] -> Bool
allexist wordlist words
	= and (map (\word->isMemberDictionary word wordlist) words)
