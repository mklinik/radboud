implementation module receiverdefaccess


import	StdTuple
import	StdReceiverAttribute, commondef


receiverDefAttributes :: !(Receiver m .ls .pst) -> [ReceiverAttribute *(.ls,.pst)]
receiverDefAttributes (Receiver _ _ atts) = atts

receiver2DefAttributes :: !(Receiver2 m r .ls .pst) -> [ReceiverAttribute *(.ls,.pst)]
receiver2DefAttributes (Receiver2 _ _ atts) = atts

receiverDefRId :: !(Receiver m .ls .pst) -> RId m
receiverDefRId (Receiver rid _ _) = rid

receiver2DefR2Id :: !(Receiver2 m r .ls .pst) -> R2Id m r
receiver2DefR2Id (Receiver2 r2id _ _) = r2id

receiverDefSelectState :: !(Receiver m .ls .pst) -> SelectState
receiverDefSelectState (Receiver _ _ atts) = getSelectState atts

receiver2DefSelectState :: !(Receiver2 m r .ls .pst) -> SelectState
receiver2DefSelectState (Receiver2 _ _ atts) = getSelectState atts

getSelectState :: ![ReceiverAttribute .pst] -> SelectState
getSelectState atts
	= getReceiverSelectStateAtt selectAtt
where
	(_,selectAtt)	= cselect isReceiverSelectState defSelect atts
	defSelect		= ReceiverSelectState Able

receiverDefFunction :: !(Receiver m .ls .pst) -> ReceiverFunction m *(.ls,.pst)
receiverDefFunction (Receiver _ f _) = f

receiver2DefFunction :: !(Receiver2 m r .ls .pst) -> Receiver2Function m r *(.ls,.pst)
receiver2DefFunction (Receiver2 _ f _) = f

receiverDefSetAbility :: !SelectState !(Receiver m .ls .pst) -> Receiver m .ls .pst
receiverDefSetAbility ability (Receiver rid f atts)
	= Receiver rid f (setSelectState ability atts)

receiver2DefSetAbility :: !SelectState !(Receiver2 m r .ls .pst) -> Receiver2 m r .ls .pst
receiver2DefSetAbility ability (Receiver2 r2id f atts)
	= Receiver2 r2id f (setSelectState ability atts)

setSelectState :: !SelectState ![ReceiverAttribute .pst] -> [ReceiverAttribute .pst]
setSelectState ability atts
	= snd (creplace isReceiverSelectState att atts)
where
	att	= ReceiverSelectState ability


receiverDefSetFunction :: !(ReceiverFunction m *(.ls,.pst)) !(Receiver m .ls .pst) -> Receiver m .ls .pst
receiverDefSetFunction f (Receiver rid _ atts)
	= Receiver rid f atts

receiver2DefSetFunction :: !(Receiver2Function m r *(.ls,.pst)) !(Receiver2 m r .ls .pst) -> Receiver2 m r .ls .pst
receiver2DefSetFunction f (Receiver2 r2id _ atts)
	= Receiver2 r2id f atts
