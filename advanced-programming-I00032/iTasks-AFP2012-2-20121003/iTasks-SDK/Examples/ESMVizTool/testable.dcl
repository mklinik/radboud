definition module testable

/*
	GAST: A Generic Automatic Software Test-system
	
	testable: the test algorithm for logical properties

	Pieter Koopman, 2002-2007
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import genLibTest
from stdProperty import ::Property // for instance of testable
import gen

//--- basics --//

:: Admin = {labels::![String], args::![String], name::![String], res::Result}
:: Result = Undef | Rej | Pass | OK | CE
:: RandomStream :== [Int]

derive gLess Result
instance == Result

:: Property = Prop (RandomStream Admin -> [Admin])

prop :: a -> Property | Testable a

class TestArg a | genShow{|*|}, ggen{|*|} a
class Testable a where evaluate :: a RandomStream !Admin -> [Admin]

instance Testable Bool
instance Testable Result
instance Testable Property
instance Testable (a->b) | Testable b & TestArg a  
instance Testable [a] | Testable a  

//derive bimap [], (,), (,,), (,,,), (,,,,), (,,,,,)

MaxExists	:== 500
NrOfTest	:== 1000

//--- for generating lists of elements ---//

aStream :: RandomStream

//--- for implementation of properties ---//

diagonal :: [[a]] -> [a]
forAll :: !(a->b) ![a] RandomStream !Admin -> [Admin] | Testable b & TestArg a
split :: !RandomStream -> (RandomStream,RandomStream)
generateAll :: !RandomStream -> [a] | ggen{|*|} a

//--- testing --//

verbose  ::      !RandomStream !p -> [String] | Testable p
verbosen :: !Int !RandomStream !p -> [String] | Testable p
concise  ::      !RandomStream !p -> [String] | Testable p
concisen :: !Int !RandomStream !p -> [String] | Testable p
quiet    ::      !RandomStream !p -> [String] | Testable p
quietn   :: !Int !RandomStream !p -> [String] | Testable p
quietnm  :: !Int !Int !RandomStream !p -> [String] | Testable p

test :: !p -> [String] | Testable p              // test p NrOfTest times
testn :: !Int !p -> [String] | Testable p        // maxnumber of tests
ttestn :: !Int !p -> [String] | Testable p       // maxnumber of tests, trace all arguments
testnm :: !Int !Int !p -> [String] | Testable p  // maxnumber of tests, max number of errors
ttestnm :: !Int !Int !p -> [String] | Testable p // maxnumber of tests, max number of errors
