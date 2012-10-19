module ratTestKlinik

import unitTest
import Rat

pRatPlusZeroIsItself :: Rat -> Bool
pRatPlusZeroIsItself x = x + zero == x

pRatMinusItselfIsZero :: Rat -> Bool
pRatMinusItselfIsZero x = x - x == zero

pRatMinusOneSmallerThanItself :: Rat -> Bool
pRatMinusOneSmallerThanItself x = x - one < x

Start = doTest
  ( testPred ["pRatPlusZeroIsItself"] pRatPlusZeroIsItself // bug in simplify
  ` testPred ["pRatMinusItselfIsZero"] pRatMinusItselfIsZero // bug in minus
  ` testPred ["pRatMinusOneSmallerThanItself"] pRatMinusOneSmallerThanItself // bug in smaller-than
  )
