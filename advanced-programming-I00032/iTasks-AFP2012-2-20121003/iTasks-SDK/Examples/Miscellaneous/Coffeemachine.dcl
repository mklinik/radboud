definition module Coffeemachine
// (c) MJP 2007
//
// This is a demo of a coffeemachine programmed with iTasks combinators.
// The persistent variant remembers the state in which the coffee machine was left.
// Garbage collection of unused tasks is done automatically.

// Some alternative coffee machine definitions have been added as example for the ICFP07 paper.

import iTasks

coffeemachineExample :: [Workflow]