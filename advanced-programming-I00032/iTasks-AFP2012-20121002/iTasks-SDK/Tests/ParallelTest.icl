module ParallelTest

import iTasks
from StdFunc import flip

derive bimap Maybe

assignTest :: Task Int
assignTest = getCurrentUser >>= \user -> user @: enterInformation ("Assigned Task","Value 1")

orTest :: Task Int
orTest = (enterInformation ("Value1","Enter value 1") -||- enterInformation ("Value2","Enter value 2"))

andTest :: Task (Int,String)
andTest = (enterInformation ("Value 1","Enter value 1") -&&- enterInformation ("Value 2","Enter value 2"))

anyTest :: Task Note
anyTest = anyTask [enterInformation ("Value 1","Enter value 1") ,enterInformation ("Value 2","Enter value 2"),enterInformation ("Value 3","Enter value 3")]

allTest :: Task [Note]
allTest = allTasks [enterInformation ("Value 1","Enter value 1"),enterInformation ("Value 2","Enter value 2"),enterInformation ("Value 3","Enter value 3")]

eitherTest :: Task (Either Note Int)
eitherTest = eitherTask (enterInformation ("Value 1","Enter value 1")) (enterInformation ("Value 2","Enter value 2"))

maybeTest :: Task (Maybe (Int,Note))
maybeTest = (enterInformation ("Value 1","Enter value 1") -&?&- enterInformation ("Value 2","Enter value 2"))

complexTest :: Task Int
complexTest
	= parallel "Complex test" 42 (flip const) [InBodyTask adder]
where
	adder _ control
		//= forever (showMessage "Press Ok to start another task!" Void >>| writeShared control [AppendTask (DetachedTask initManagerProperties noMenu foo)])
		= forever (showMessage "Press Ok to start another task!" Void >>| writeShared control [AppendTask (DetachedTask initManagerProperties noMenu foo)])
	foo _ _
		= showMessage "Foobar!" 42

Start :: *World -> *World
Start world = startEngine [
			workflow "Assign Test" "Assign Test" (assignTest >>= showMessageAbout ("Result","The result is:")),
			workflow "Or Test" "Or Test" (orTest  >>= showMessageAbout ("Result","The result is:")),
			workflow "And Test" "And Test" (andTest  >>= showMessageAbout ("Result","The result is:")),
			workflow "Any Test" "Any Test" (anyTest  >>= showMessageAbout ("Result","The result is:")),
			workflow "All Test" "All Test" (allTest  >>= showMessageAbout ("Result","The result is:")),
			workflow "Either Test" "Either Test" (eitherTest  >>= showMessageAbout ("Result","The result is:")),
			workflow "Maybe Test" "Maybe Test" (maybeTest >>= showMessageAbout ("Result","The result is:")),
			workflow "Complex Test" "Complex Test" (complexTest >>= showMessageAbout ("Result","The result is:"))
		] world 