module assignment07klinik

import StdEnv
import gast

/* === Model of the vending machine === */

:: Input
  = C Coin
  | D Digit // one of the digits on the screen
  | Reset
  | Return  // cancel purchase and return all money

:: Output = Change Coin | Product Product

:: ModelState =
  { products :: [AvailableProduct]
  , modBalance :: Int
  }

:: AvailableProduct =
  { product :: Product // this product
  , digits :: (Digit, Digit) // which digits it's assigned to
  , price :: Int // how much it costs
  }


:: Product = CaffeinatedBeverage | EnergyBar | Apple

:: Coin = Coin Int
:: Digit = Digit Int

vendingMachineModel :: ModelState Input -> [Trans Output ModelState]
vendingMachineModel s (C (Coin coin)) = [Pt [] { s & modBalance = s.modBalance + coin }]
vendingMachineModel s Return = [Pt [Change (Coin s.modBalance)] { s & modBalance = 0 }]
vendingMachineModel s _ = [Pt [] s] // everything else is WTF?

:: MachineState =
  { balance :: Int
  , digitsEntered :: (Maybe Digit, Maybe Digit)
  }

enterDigit :: Digit (Maybe Digit, Maybe Digit) -> (Maybe Digit, Maybe Digit)
enterDigit d (Nothing, Nothing) = (Just d, Nothing)
enterDigit d (Just d1, Nothing) = (Just d1, Just d)
enterDigit _ x = x // cannot enter more than two digits

derive genShow MachineState, ModelState, Input, Output, Digit, Coin, Product, AvailableProduct, Maybe
derive gEq ModelState, Output, Product, Coin, AvailableProduct, Digit
derive ggen Input
derive bimap []

ggen{|Digit|} n r = randomize (map Digit [0..9]) r 10 (const [])
ggen{|Coin|} n r = randomize (map Coin [5, 10, 20, 50, 100, 200]) r 10 (const [])

vendingMachine :: MachineState Input -> ([Output], MachineState)
vendingMachine s (C (Coin coin)) = ([], { s & balance = s.balance + coin })
vendingMachine s Return          = ([Change (Coin s.balance)], { s & balance = 0 })
vendingMachine s (D digit)       = ([], { s & digitsEntered = enterDigit digit s.digitsEntered })
vendingMachine s Reset           = ([], { s & digitsEntered = (Nothing, Nothing) })

(step) infixl :: ([Output], MachineState) Input -> ([Output], MachineState)
(step) (_, s) i = vendingMachine s i

Start2 = vendingMachine implStartState (D (Digit 4)) step (D (Digit 2)) step (C (Coin 5))

implStartState =
  { balance = 0
  , digitsEntered = (Nothing, Nothing)
  }

productsAvailable =
  [ { product = CaffeinatedBeverage
    , digits = (Digit 0, Digit 0)
    , price = 85
    }
  , { product = EnergyBar
    , digits = (Digit 1, Digit 8)
    , price = 120
    }
  , { product = Apple
    , digits = (Digit 4, Digit 2)
    , price = 5
    }
  ]

Start world =
  testConfSM
    [Ntests 10] // test options
    vendingMachineModel // specification
    { products = productsAvailable, modBalance = 0 } // spec start state
    vendingMachine // implementation
    implStartState // impl start state
    (const implStartState) // reset function
    world // world

// vendingMachine { balance = 0 } (C (Coin 20))

