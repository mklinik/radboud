implementation module EnDecode;

import StdEnv;
import DefaultElem;
import DynamicUtilities;

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

ENDECODE_BOOL_SIZE	:== 1;
ENDECODE_BOOL_TRUE	:== 0;
ENDECODE_BOOL_FALSE	:== 1;
instance EnDecode Bool
where {
	to_size _				  = ENDECODE_BOOL_SIZE;
	to_string b offset buffer = (offset + ENDECODE_BOOL_SIZE,  { buffer & [offset] = toChar (if b ENDECODE_BOOL_TRUE ENDECODE_BOOL_FALSE) });

	from_string offset buffer
		# c
			= buffer.[offset];
		= (if ((toInt c) == ENDECODE_BOOL_TRUE) True False,offset + ENDECODE_BOOL_SIZE);
};

ENDECODE_INT_SIZE	:== 4;
instance EnDecode Int
where {
	to_size _				  	= ENDECODE_INT_SIZE;
	to_string i offset buffer 	= (offset + ENDECODE_INT_SIZE, WriteLong buffer offset i);
	
	from_string offset buffer	
		# i
			= FromStringToInt buffer offset;
		= (i,offset + ENDECODE_INT_SIZE);
};

ENDECODE_CHAR_SIZE	:== 1;
instance EnDecode Char
where {
	to_size _				  = ENDECODE_CHAR_SIZE;
	to_string i offset buffer = (offset + ENDECODE_CHAR_SIZE,  { buffer & [offset] = i });

	from_string offset buffer
		# c
			= buffer.[offset];
		= (c,offset + ENDECODE_CHAR_SIZE);
};

ENDECODE_LIST_SIZE	:== 4;

instance EnDecode [b] | EnDecode, DefaultElem b
where {
	to_size list
		= ENDECODE_LIST_SIZE + (to_size_list list 0);
	where {
		to_size_list [x:xs] total_size
			= to_size_list xs (to_size x + total_size);
		to_size_list [] total_size
			= total_size;
	};
	
	to_string list offset buffer
		# (last_offset,buffer,n_elements)
			= to_string2 list (ENDECODE_LIST_SIZE + offset) buffer 0;
		# buffer
			= WriteLong buffer offset n_elements;
		= (last_offset,buffer);
	where {
		to_string2 [x:xs] offset buffer i
			# (offset,buffer)
				= to_string x offset buffer;
			= to_string2 xs offset buffer (inc i);
		to_string2 [] offset buffer i
			= (offset,buffer,i);
	};

	from_string offset buffer
		# s_list
			= FromStringToInt buffer offset;
		= from_string2 0 s_list [] buffer (ENDECODE_LIST_SIZE + offset);
	where {
		from_string2 i limit accu buffer offset
			| i == limit
				= (reverse accu,offset);
			# (x,offset)
				= from_string offset buffer;
			= from_string2 (inc i) limit [x:accu] buffer offset;
	};
};

ENDECODE_ARRAY_SIZE	:== 4;

instance EnDecode (a e) | EnDecode, DefaultElem e & Array a e
where {
	to_size array
		= ENDECODE_ARRAY_SIZE + (to_size_array 0 (size array) 0);
	where {
		to_size_array :: !Int !Int !Int -> Int;
		to_size_array i limit total_size
			| i == limit
				= total_size;
			= to_size_array (inc i) limit (total_size + (to_size array.[i]));
	};
	
	to_string array offset buffer
		# buffer
			= WriteLong buffer offset s_array;
		= to_string_array 0 s_array (offset + ENDECODE_ARRAY_SIZE) buffer;
	where {
		to_string_array i limit offset buffer
			| i == limit
				= (offset,buffer);
			# (offset,buffer)
				= to_string array.[i] offset buffer;
			= to_string_array (inc i) limit offset buffer;
	
		s_array
			= size array
	};

	from_string offset buffer
		# s_array
			= FromStringToInt buffer offset;
		# a
			= createArray s_array default_elem;
		= from_string_array 0 s_array a (offset + ENDECODE_ARRAY_SIZE) buffer;
	where {
		from_string_array i limit array offset buffer
			| i == limit
				= (array,offset);

			# (elem,offset)
				= from_string offset buffer;
			= from_string_array (inc i) limit {array & [i] = elem} offset buffer;
	}
};

// copy of above but without #
instance EnDecode {a} | EnDecode, DefaultElem a & Array {} a
where {
	to_size array
		= ENDECODE_ARRAY_SIZE + (to_size_array 0 (size array) 0);
	where {
		to_size_array :: !Int !Int !Int -> Int;
		to_size_array i limit total_size
			| i == limit
				= total_size;
			= to_size_array (inc i) limit (total_size + (to_size array.[i]));
	};
	
	to_string array offset buffer
		# buffer
			= WriteLong buffer offset s_array;
		= to_string_array 0 s_array (offset + ENDECODE_ARRAY_SIZE) buffer;
	where {
		to_string_array i limit offset buffer
			| i == limit
				= (offset,buffer);
			# (offset,buffer)
				= to_string array.[i] offset buffer;
			= to_string_array (inc i) limit offset buffer;
	
		s_array
			= size array
	};

	from_string offset buffer
		# s_array
			= FromStringToInt buffer offset;
		# a
			= createArray s_array default_elem;
		= from_string_array 0 s_array a (offset + ENDECODE_ARRAY_SIZE) buffer;
	where {
		from_string_array i limit array offset buffer
			| i == limit
				= (array,offset);

			# (elem,offset)
				= from_string offset buffer;
			= from_string_array (inc i) limit {array & [i] = elem} offset buffer;
	}
};

instance EnDecode (a,b) | EnDecode a & EnDecode b;
where {
	to_size (a,b)
		= to_size a + to_size b;
	
	to_string (a,b) offset buffer
		# (offset,buffer)
			= to_string a offset buffer;
		# (offset,buffer)
			= to_string b offset buffer;
		= (offset,buffer);

	from_string offset buffer
		# (a,offset)
			= from_string offset buffer;
		# (b,offset)
			= from_string offset buffer;
		= ((a,b),offset);
};

instance EnDecode (a,b,c,d) | EnDecode a & EnDecode b & EnDecode c & EnDecode d;
where {
	to_size (a,b,c,d)
		= to_size a + to_size b + to_size c + to_size d;
	
	to_string (a,b,c,d) offset buffer
		# (offset,buffer)
			= to_string a offset buffer;
		# (offset,buffer)
			= to_string b offset buffer;
		# (offset,buffer)
			= to_string c offset buffer;
		# (offset,buffer)
			= to_string d offset buffer;
		= (offset,buffer);

	from_string offset buffer
		# (a,offset)
			= from_string offset buffer;
		# (b,offset)
			= from_string offset buffer;
		# (c,offset)
			= from_string offset buffer;
		# (d,offset)
			= from_string offset buffer;

		= ((a,b,c,d),offset);

};

instance EnDecode (a,b,c,d,e) | EnDecode a & EnDecode b & EnDecode c & EnDecode d & EnDecode e
where {
	to_size (a,b,c,d,e)
		= to_size a + to_size b + to_size c + to_size d + to_size e;
	
	to_string (a,b,c,d,e) offset buffer
		# (offset,buffer)
			= to_string a offset buffer;
		# (offset,buffer)
			= to_string b offset buffer;
		# (offset,buffer)
			= to_string c offset buffer;
		# (offset,buffer)
			= to_string d offset buffer;
		# (offset,buffer)
			= to_string e offset buffer;

		= (offset,buffer);

	from_string offset buffer
		# (a,offset)
			= from_string offset buffer;
		# (b,offset)
			= from_string offset buffer;
		# (c,offset)
			= from_string offset buffer;
		# (d,offset)
			= from_string offset buffer;
		# (e,offset)
			= from_string offset buffer;

		= ((a,b,c,d,e),offset);
};

