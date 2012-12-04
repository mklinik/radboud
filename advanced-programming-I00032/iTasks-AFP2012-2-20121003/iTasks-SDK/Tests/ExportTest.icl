module ExportTest

import iTasks

test1 :: Task Document
test1 = enterInformation "Upload please" >>= exportDocument "test.doc"

test2 :: Task String
test2 = enterInformation "Enter content" >>= exportTextFile "test.txt"

test3 :: Task [[String]]
test3 = enterInformation "Enter rows" >>= exportCSVFile "test.txt"

test4 :: Task [Int]
test4 = enterInformation "Enter numbers" >>= exportJSONFile "test.txt"

Start :: *World -> *World
Start world = startEngine [workflow "Test" test4] world