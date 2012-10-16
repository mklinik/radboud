module ratTestKlinik

import unitTest
import Rat

pRatPlusZeroIsItself :: Rat -> Bool
pRatPlusZeroIsItself x = x + zero == x

pRatMinusItselfIsZero :: Rat -> Bool
pRatMinusItselfIsZero x = x - x == zero

pRatTimesOneIsItself :: Rat -> Bool
pRatTimesOneIsItself x = x * one == x

pRatDividedByItselfIsOne :: Rat -> Bool
pRatDividedByItselfIsOne x = x / x == one

pRatPowMinusOneIsTheInverse :: Rat -> Bool
pRatPowMinusOneIsTheInverse x = x ^ (toRat (-1)) == inv x

Start = doTest
  ( testPred ["pRatPlusZeroIsItself"] pRatPlusZeroIsItself
  ` testPred ["pRatMinusItselfIsZero"] pRatMinusItselfIsZero
  ` testPred ["pRatTimesOneIsItself"] pRatTimesOneIsItself
  ` testPred ["pRatDividedByItselfIsOne"] pRatDividedByItselfIsOne
  ` testPred ["pRatPowMinusOneIsTheInverse"] pRatPowMinusOneIsTheInverse
  )
