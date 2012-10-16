module main

import unitTest
import Rat

pPlusRat :: Rat Rat -> Bool
pPlusRat x y = y + x == x + y

pRatPlusZeroEqualsItself :: Rat -> Bool
pRatPlusZeroEqualsItself x = x + zero == x

pRatMinusItselfIsZero :: Rat -> Bool
pRatMinusItselfIsZero x = x - x == zero

pRatDividedByItselfIsOne :: Rat -> Bool
pRatDividedByItselfIsOne x = x / x == one


Start = doTest
  ( testPred ["pPlusRat"] pPlusRat
  ` testPred ["pRatPlusZeroEqualsItself"] pRatPlusZeroEqualsItself
  ` testPred ["pRatMinusItselfIsZero"] pRatMinusItselfIsZero
  ` testPred ["pRatDividedByItselfIsOne"] pRatDividedByItselfIsOne
  )
