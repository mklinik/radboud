implementation module BitSet;

// implementation of finite integer sets
import
	StdEnv;

class BitSetAccessors a
where {
	get_bit_set_word_at_index :: !Int !*a -> (!Int,!*a)
};

instance BitSetAccessors BitSet
where {
	get_bit_set_word_at_index index bitset=:{map}
		#! (word,map)
			= map![index];
		= (word,{bitset & map = map});
};
		

roundup_to_multiple s m :== (dec (s + m) / m) * m;

// MACROS	
BITS_IN_WORD	:== 32;
DIV_WSIZE x		:== x >> 5;
MOD_WSIZE x		:== x bitand (dec BITS_IN_WORD);

GBIT_UPDATE_FSET fs x op	:== gbit fs x op
where {
	gbit fs=:{map} x op
		#! div_wsize
			= DIV_WSIZE x;
		#! (map_elem,map)
			= map![div_wsize];
		= { fs & map = { map & [div_wsize] = (op) map_elem (1 << (MOD_WSIZE x)) } };
}

GBIT fs x op	:== gbit fs x op
where {
	gbit fs=:{map} x op
		#! div_wsize
			= DIV_WSIZE x;
		#! (map_elem,map)
			= map![div_wsize];
		= ((op) map_elem (1 << (MOD_WSIZE x)),{fs & map = map});
}

RANGE_CHECK s x	id :== range_check s x id
where {
	range_check s=:{n_elements} x id
		#! i_elem
			= toInt x;
		| i_elem < 0 || i_elem >= n_elements 
			= abort ("RANGE_CHECK!: " +++ id +++ " set t!oo! small, set size: " +++ toString n_elements +++ " asked for element: " +++ toString i_elem); 
			= (i_elem,s);
}

EmptyBitSet :: .BitSet;
EmptyBitSet
	= NewBitSet 0;

NewBitSet :: !Int -> .BitSet;
NewBitSet n_elements
	// compute amount of Clean INTs needed to represent the set
	#! n_map
		= (roundup_to_multiple n_elements BITS_IN_WORD) / BITS_IN_WORD;
	
	// create empty set
	#! set
		= { BitSet |
			n_elements	= n_elements
		,	map			= createArray n_map 0
	}; 
	= set;
	
ClearBitSet :: !.BitSet -> .BitSet;
ClearBitSet bitset=:{map}
	= { bitset & map =  { 0 \\ b <-: map } };
	
EqualBitSet :: !*BitSet !*BitSet -> (!Bool,!*BitSet,!*BitSet);
EqualBitSet fset1=:{n_elements=n_elements1,map=map1} fset2=:{n_elements=n_elements2,map=map2}
	| n_elements1 <> n_elements2
		= (False,fset1,fset2);
	
	// if a complemented set is equal to the same set but not constructed with
	// the complement operation, then the sets are *not* considered the same.	
	#! n_ints
		= (roundup_to_multiple n_elements1 BITS_IN_WORD) / 32;
	#! (equal_fsets,map1,map2)
		= equal_fset 0 n_ints map1 map2
	= (equal_fsets,{fset1 & map = map1},{fset2 & map = map2});
where {
	equal_fset i limit map1 map2		
		| i == limit	
			= (True,map1,map2);
		#! (i1,map1) = map1![i];
		#! (i2,map2) = map2![i];
		| i1 <> i2
			= (False,map1,map2);
		= equal_fset (inc i) limit map1 map2; 
}
		
DelBitSet :: !*BitSet !a -> *BitSet | fromInt, toInt a;
DelBitSet fset elem
	#! (elem,fset)
		= RANGE_CHECK fset elem	"DelBitSet";
	= GBIT_UPDATE_FSET fset elem (\a1 a2 -> a1 bitand (bitnot a2));
	
AddBitSet :: !*BitSet !a -> *BitSet | toInt a;
AddBitSet fset elem
	#! (elem,fset)
		= RANGE_CHECK fset elem	"AddBitSet";
	= GBIT_UPDATE_FSET fset elem (bitor);
	
isBitSetMember :: !u:BitSet !Int -> (Bool,v:BitSet), [u <= v];
isBitSetMember fset elem
	#! (elem,fset)
		= RANGE_CHECK fset elem "isBitSetMember";
	#! (elem,fset)
		= GBIT fset elem (bitand)
	= (elem <> 0,fset);
	
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

// FINITE SET REPRESENTATION
:: BitSet
	= {
		n_elements	:: !Int
	,	map			:: !.{#Int}
	};
	
ComplementBitSet :: !*BitSet -> *BitSet;
ComplementBitSet fset=:{map}
	#! (s_map,map)
		= usize map;
	#! fset
		= { fset &
			map = complement_fset_loop 0 s_map map
		};
	= fset;
where {
	complement_fset_loop :: !Int !Int !*{#Int} -> *{#Int};
	complement_fset_loop i limit map
		| i == limit
			= map;
			
		#! (e_i,map)
			= map![i]
		#! map
			= { map & [i] = bitnot e_i };
		= complement_fset_loop (inc i) limit map;
}

enum_setSt :: (Int -> .(.a -> .a)) !u:BitSet .a -> (v:BitSet,.a), [u <= v];
enum_setSt f fset=:{n_elements,map} state
	#! (fset,state) 
		= enum_setSt 0 fset state
	= (fset,state);
where {
	enum_setSt i fset state
		| i == n_elements
			= (fset,state)

		#! (is_member,fset)
			= isBitSetMember fset i;
		| is_member
			= enum_setSt (inc i) fset (f i state);
			= enum_setSt (inc i) fset state;
	}