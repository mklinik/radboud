module assignment07klinik

import StdEnv
import gast

/* === Model of the vending machine === */

:: Input
  = C Coin
  // | D Digit // one of the digits on the screen
  | Return  // cancel purchase and return all money

:: Output = Change Coin | Product Product

:: ModelState =
  { products :: [AvailableProduct]
  , modBalance :: Int
  }

:: AvailableProduct =
  { product :: Product // this product
  , digit :: Digit // which digit it's assigned to
  , price :: Coin // how much it costs
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
  }

derive genShow MachineState, ModelState, Input, Output, Digit, Coin, Product, AvailableProduct
derive gEq ModelState, Output, Product, Coin, AvailableProduct, Digit
derive ggen Input, Coin, Digit
derive bimap []

vendingMachine :: MachineState Input -> ([Output], MachineState)
vendingMachine s (C (Coin coin)) = ([], { s & balance = s.balance + coin })
vendingMachine s Return = ([Change (Coin s.balance)], { s & balance = 0 })

Start world =
  testConfSM
    [Ntests 10] // test options
    vendingMachineModel // specification
    { products = [], modBalance = 0 } // spec start state
    vendingMachine // implementation
    implStartState // impl start state
    (const implStartState) // reset function
    world // world
where
  implStartState = { balance = 0 }

// vendingMachine { balance = 0 } (C (Coin 20))

