definition module BitSet;

from StdOverloaded import class toInt(toInt), class fromInt(fromInt), class + (+), class == (==),
		class - (-), class one (one);
from StdClass import <>, dec, not, class Eq;
from StdInt import <<, >>, bitand, bitor, bitnot, instance + (Int), instance == (Int);

BITS_IN_WORD	:== 32;
DIV_WSIZE x		:== x >> 5;
MOD_WSIZE x		:== x bitand (dec BITS_IN_WORD);

// FINITE SET REPRESENTATION
:: BitSet
	= {
		n_elements	:: !Int
	,	map			:: !.{#Int}
	};

EmptyBitSet :: .BitSet;

NewBitSet :: !Int -> .BitSet;
ClearBitSet :: !.BitSet -> .BitSet;

EqualBitSet :: !*BitSet !*BitSet -> (!Bool,!*BitSet,!*BitSet);
DelBitSet :: !*BitSet !a -> *BitSet | fromInt, toInt a;
AddBitSet :: !*BitSet !a -> *BitSet | toInt a;
ComplementBitSet :: !*BitSet -> *BitSet;
enum_setSt :: (Int -> .(.a -> .a)) !u:BitSet .a -> (v:BitSet,.a), [u <= v];

isBitSetMember :: !u:BitSet !Int -> (Bool,v:BitSet), [u <= v];

isBitSetMemberE	select_func s elem :== isBitSetMemberE2 select_func s elem;
where {
	isBitSetMemberE2 select_func s elem
		#! div_wsize
			= DIV_WSIZE elem;
		#! (map_elem,s)
			= select_func div_wsize s;
		#! map_elem
			= map_elem bitand (1 << (MOD_WSIZE elem));
		= (map_elem <> 0,s);
};

AddBitSetE select_func update_func s elem :== AddBitSetE select_func update_func s elem;
where {
	AddBitSetE select_func update_func s elem
		#! div_wsize
			= DIV_WSIZE elem;
		#! (map_elem,s)
			= select_func div_wsize s;
		#! map_elem
			= map_elem bitor (1 << (MOD_WSIZE elem));
		#! s
			= update_func map_elem div_wsize s;
		= s;
};

DelBitSetE select_func update_func s elem :== DelBitSetE select_func update_func s elem;
where {
	DelBitSetE select_func update_func s elem
		#! div_wsize
			= DIV_WSIZE elem;
		#! (map_elem,s)
			= select_func div_wsize s;
		#! map_elem
			= map_elem bitand (bitnot (1 << (MOD_WSIZE elem)));
		#! s
			= update_func map_elem div_wsize s;
		= s;		
};