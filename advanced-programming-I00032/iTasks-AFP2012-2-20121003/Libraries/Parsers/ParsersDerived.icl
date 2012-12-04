implementation module ParsersDerived
import ParsersKernel
from StdEnv import o, abort, id
from StdEnv import const, instance == Char

// PARSER COMBINATORS:

(<&) infixr 6 :: (Parser s r t) (Parser s r` t) -> Parser s r t
(<&) p1 p2 = p1 <&> \r1 -> p2 <@ const r1

(&>) infixr 6 // :: (Parser s r t) (Parser s r` t) -> Parser s r` t
(&>) p1 p2 :== p1 <&> const p2

(<&&>) infixr 6	:: (Parser s r t) (Parser s u t) -> Parser s (r,u) t
(<&&>) p1 p2 = p1 <&> \r1 -> p2 <@ \r2 -> (r1,r2)

(<:&>) infixr 6	:: (Parser s r t) (Parser s [r] t) -> Parser s [r] t
(<:&>) p1 p2 = p1 <&> \r1 -> p2 <@ \r2 -> [r1:r2]

(<:&:>) infixr 6	:: (Parser s r t) (Parser s ([r]->[r]) t) -> Parser s ([r]->[r]) t
(<:&:>) p1 p2 = p1 <&> \r1 -> p2 <@ \r2 -> \rest -> [r1:r2 rest]

// PARSER TRANSFORMERS:

<*> :: (Parser s r t) -> Parser s [r] t
<*> p = (p <:&> <*> p) <|> yield []

<*:> :: (Parser s r t) -> Parser s ([r]->[r]) t
<*:> p = (p <:&:> <*:> p) <|> yield id

<+> :: (Parser s r t) -> Parser s [r] t
<+> p = p <:&> <*> p

<+:> :: (Parser s r t) -> Parser s ([r]->[r]) t
<+:> p = p <:&:> <*:> p

<!*> :: (Parser s r t) -> Parser s [r] t
<!*> p = (p <&-> \r -> <!*> p <@ \rs -> [r:rs]) <-!> yield []

<!*:> :: (Parser s r t) -> Parser s ([r]->[r]) t
<!*:> p = (p <&-> \r -> <!*:> p <@ \rs -> \rest -> [r:rs rest]) <-!> yield id

<!+> :: (Parser s r t) -> Parser s [r] t
<!+> p = p <:&> <!*> p

<!+:> :: (Parser s r t) -> Parser s ([r]->[r]) t
<!+:> p = p <:&:> <!+:> p

<?> :: (Parser s r t) (r -> u) u -> Parser s u t
<?> p f c = p <@ f <|> yield c

<!?> :: (Parser s r t) (r -> u) u -> Parser s u t
<!?> p f c = first (<?> p f c)

(@>) infix 7 //	:: (r -> r`) (Parser s r t) -> Parser s r` t
(@>) f p :== yield f <++> p

(<@) infixl 5 :: (Parser s r t) (r ->r`) -> Parser s r` t
(<@) p f = p <&> yield o f

(<=@) infixl 5 :: (u -> Parser s r t) (r ->r`) -> (u -> Parser s r` t)
(<=@) wp f = \u -> (wp u) <@ f

/* grazeTo by itself is NOT so useful. grazeOnce might be. If grazeTo finds a delimiter but the
   following component of the parser fails, one alternative of grazeTo is to continue scanning to
   the end of the input, going for another delimiter, and then reporting an unexpected end of input.
   And that error will 'win'. Not so helpful generally.*/ 

grazeTo :: (Parser s r t) -> Parser s [s] t
grazeTo until = p
where	p =	rewind until <@ const [] <|> anySymbol <:&> p

grazeOver :: (Parser s r t) -> Parser s [s] t
grazeOver until = p
where	p = until <@ const [] <|> anySymbol <:&> p

/*	grazeOnce should be used with care. Usually it should be enclosed in atMost, so it will not
	scan through to the end of the whole input. */

grazeOnce :: (Parser s r t) -> Parser s [s] t
grazeOnce until = first (grazeTo until)

skipTo :: (Parser s r t) -> Parser s u t
skipTo until = p
where	p =  rewind until <@ const u <|> anySymbol &> p
		u = abort "result of rewind-parser constructor accessed in skipTo"
		
skipOver :: (Parser s r t) -> Parser s u t
skipOver until = p
where	p = until <@ const u <|> anySymbol &> p
		u = abort "result of until-parser accessed in skipOver"
		
skipOnce :: (Parser s r t) -> Parser s u t
skipOnce until = first (skipTo until)

scrape :: (Parser s r t) (r ->Int) (r -> Parser s v t) -> Parser s v t
scrape p1 adv wp2 = getParsable <&> \pb -> p1 <&>
					\r1 -> setParsable pb &> advancePosition (adv r1) &> wp2 r1
