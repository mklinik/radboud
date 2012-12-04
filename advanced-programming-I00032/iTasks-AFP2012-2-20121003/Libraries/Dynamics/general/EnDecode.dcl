definition module EnDecode;

from StdArray import class Array (createArray);
from StdTuple import fst, snd;
from DefaultElem import class DefaultElem;

// non-unique, straight-forward conversions
class EnDecode a 
where {
	to_size	:: a -> Int;
	to_string :: a !Int !*{#Char} -> (!Int,!*{#Char});
	from_string :: !Int !{#Char} -> (a,!Int);
	
	encode :: a -> *{#Char} | EnDecode a;
	encode a :== snd (to_string a 0 (createArray (to_size a) '@'));

	decode :: !{#Char} -> a | EnDecode a;
	decode buffer :== fst (from_string 0 buffer)
		
};

instance EnDecode Bool;

instance EnDecode Int;

instance EnDecode Char;

instance EnDecode [b] | EnDecode, DefaultElem b;

instance EnDecode (a e) | EnDecode, DefaultElem e & Array a e;

instance EnDecode {a} | EnDecode, DefaultElem a & Array {} a;

instance EnDecode (a,b) | EnDecode a & EnDecode b;

instance EnDecode (a,b,c,d) | EnDecode a & EnDecode b & EnDecode c & EnDecode d;

instance EnDecode (a,b,c,d,e) | EnDecode a & EnDecode b & EnDecode c & EnDecode d & EnDecode e;
