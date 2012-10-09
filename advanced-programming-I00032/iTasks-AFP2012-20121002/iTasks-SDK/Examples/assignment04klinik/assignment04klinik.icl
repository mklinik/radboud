module assignment04klinik

import iTasks

enterInt :: Task Int
enterInt = enterInformation "enter one integer" []

enterInts :: Task [Int]
enterInts = enterInformation "enter some integers" []

updateInts :: [Int] -> Task [Int]
updateInts l = updateInformation "edit some integers" [] l

fooInts :: [Int] -> Task [Int]
fooInts l = updateInformation "foobar some integers" [] l @? makeStableIfMoreThan 3 >>= fooInts

viewInt :: String Int -> Task Int
viewInt name value = viewInformation name [] value

viewInts :: String [Int] -> Task [Int]
viewInts name value = viewInformation name [] value

//viewSum :: String (ReadOnlyShared (Maybe (Int, Int))) -> Task Int
//viewSum name value =
  //case read value of
    //Nothing = viewInformation name [] 0
    //Just (x, y) = viewInformation name [] (x + y)

studentTask :: Task [Student]
//studentTask = enterInformation "Enter student credentials" [EnterWith studentViewToStudent]
studentTask = enterInformation "Enter student credentials" [EnterWith id] @? (makeStableIfMoreThan 2)

makeStableIfMoreThan :: Int (TaskValue [a]) -> TaskValue [a]
makeStableIfMoreThan _ NoValue = NoValue
makeStableIfMoreThan num (Value l _) =
  if (length l > num)
     (Value l Stable)
     (Value l Unstable)

appendDupe :: (TaskValue [a]) -> TaskValue [a]
appendDupe NoValue = NoValue
appendDupe (Value [] stability) = Value [] stability
appendDupe (Value [x:y] stability) = Value [x:x:y] stability

Start :: *World -> *World
//Start world = startEngine (manageWorklist [workflow "new student" "Enter student credentials" studentTask]) world
//Start world = startEngine studentTask world
//Start world = startEngine (enterInt -&&- enterInt >&> viewSum "sum") world

//Start world = startEngine (enterInts) world
//Start world = startEngine (enterInts @ map (\x = x * 2) >>= viewInts "here is your result:") world
//Start world = startEngine ((enterInts <! \x = length x > 2) >>= viewInts "here is your result:") world
Start world = startEngine (fooInts []) world


:: Student =
  { person :: Person
  , studentNumber :: String
  }

:: Person =
  { firstName :: String
  , lastName :: String
  , dateOfBirth :: Date
  , gender :: Display Gender
  }

:: Gender = Male | Female

:: StudentView =
  { firstName :: String
  , studentNumber :: String
  , dateOfBirthh :: Date
  }

derive class iTask Student, Person, Gender, StudentView

studentViewToStudent sv =
    { person =
      { firstName = "foo"
      , lastName = "bar"
      , dateOfBirth = sv.dateOfBirthh
      , gender = Display Male
      }
    , studentNumber = "abc"
    }
