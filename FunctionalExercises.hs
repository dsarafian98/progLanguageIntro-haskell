--Danielle Sarafian
--Functional Exercises--4/20/2020
module DoesItWork where
import TestSuiteSupportModule

--1. Write a function that takes two parameters, an atom and a list, and returns true if the atom is an element in the list and false otherwise.
elementOf :: (Eq a) => a -> [ a ] -> Bool
--if the list is empty
elementOf item [] = False
elementOf item (hd: rest)
  --if this is the item you're looking for
  | hd == item = True
  --if it's not, search the rest of the list
  | otherwise = elementOf item rest

--2. Write a function that takes two parameters, a list and an atom, and returns a list identical to the first parameter except with all instances of the given atom deleted.
remove :: (Eq a) => a -> [ a ]-> [ a ]
-- if the list is empty
remove item [] = []
remove item (hd: rest)
  -- if this is the item, remove it
  | hd == item = rest
  -- if it's not, search the rest of the list
  | otherwise = hd : remove item rest

--3. Write a function similar to the previous one, except that it deletes instances of a list (the first parameter) from a list of lists (the second parameter).
removeList :: (Eq a) => [a] -> [[a]] -> [[a]]
--if the first list is empty, nothing to remove
removeList [] x = x
-- if the second list is empty
removeList x [[]] = [[]]
removeList x (hd: rest)
  -- if this is the item, remove it
  | hd == x = rest
  -- if this is the last item and it's not the item you want, return an empty list 
  | rest == [] = hd: []
  -- if it's not the item you want, check the rest of the list
  | otherwise = hd: removeList x rest

--4. Write a function that takes two parameters, each of which is a list of atoms, and returns a list similar to the first parameter except that all atoms in the first list that are elements in the second list have been removed from the first list.
removeAll :: (Eq a) => [a] -> [a] -> [a]
-- if the first list is empty
removeAll [] x = []
-- if the second list is empty, nothing to remove
removeAll x [] = x
removeAll x (hd : rest)
  -- if this item is in the list, remove it and check the resulting list for this element again
  | elementOf hd x = removeAll (remove hd x) (hd : rest)
  -- if this item isn't in the list, check the rest of the list 
  | otherwise = removeAll x rest

--5. Write a function that takes a list as a parameter and returns a list identical to the parameter except the last element has been deleted.
deleteLast :: (Eq a) => [a] ->[a]
-- nothing to delete
deleteLast [] = []
deleteLast (hd : rest)
  -- if it's the last element, delete it
  | rest == [] = []
  -- keep going until you reach the end of the list
  | otherwise = (hd : deleteLast rest)

--6. Write a function that returns the reverse of its simple list parameter.
myReverse :: (Eq a) => [a] -> [a]
-- nothing to reverse
myReverse [] = []
myReverse (hd : rest)
  -- if this is the last element, you're done, return the list
  | rest == [] = (hd : rest)
  -- get the last element of the list and remove it, then call myReverse to get the next last element
  | otherwise = (last (hd: rest)) : (myReverse (deleteLast (hd:rest)))

--7. Write a function that takes a list of numbers as a parameter and returns a list with the largest and smallest numbers in the input list.
minAndMax :: (Eq a) => (Ord a) => [a] -> [a]
-- empty list
minAndMax [] = []
minAndMax (hd:rest)
  -- if this is the last element, return  your list
  | rest == [] = (hd:rest)
  -- get min and max and put them in a list
  | otherwise = [myMin (hd:rest), myMax (hd:rest)]
--helper function to find maximum
myMax :: (Eq a) => (Ord a) => [a] -> a
myMax (hd:rest)
  --if you're at the end of the list, return
  | rest == [] = hd
  --check if this element is greater than all the rest
  | hd > myMax rest = hd
  --if it isn't, check the rest of the list
  | otherwise = myMax rest
--helper function to find minimum
myMin :: (Eq a) => (Ord a) => [a] -> a
myMin (hd:rest)
    --if you're at the end of the list, return
  | rest == [] = hd
    --check if this element is greater than all the rest
  | hd < myMin rest = hd
  --helper function to find maximum
  | otherwise = myMin rest


elementOfTestSuite = TestSuite "Test good elementOf examples"
  [Test "no elements in list" (elementOf 2 [] == False),
   Test "1 number in list" (elementOf 1 [1] == True),
   Test "multiple numbers in list" (elementOf 5 [1,2,3,4,5,6,7,8,9] == True),
   Test "1 character in list" (elementOf 'a' ['a'] == True),
   Test "multiple characters in list" (elementOf 'a' ['a','b','c','d'] == True),
   Test "number not in list of length 1" (elementOf 1 [2] == False),
   Test "number not in list longer than 1" (elementOf 1 [2,3,4,5,6] == False),
   Test "character not in list of length 1" (elementOf 'a' ['b'] == False),
    Test "character not in list of length 1" (elementOf 'a' ['b','c','d','e'] == False)
   ]

removeTestSuite = TestSuite "Test good remove examples"
  [Test "no elements in list" (remove 2 [] == []),
   Test "1 number in list" (remove 1 [1] == []),
   Test "multiple numbers in list" (remove 5 [1,2,3,4,5,6,7,8,9] == [1,2,3,4,6,7,8,9]),
   Test "remove first of multiple numbers in list" (remove 1 [1,2,3,4,5,6,7,8,9] == [2,3,4,5,6,7,8,9]),
   Test "remove last of multiple numbers in list" (remove 9 [1,2,3,4,5,6,7,8,9] == [1,2,3,4,5,6,7,8]),
   Test "1 character in list" (remove 'a' ['a'] == []),
   Test "remove first of multiple characters in list" (remove 'a' ['a','b','c','d'] == ['b','c','d']),
   Test "multiple characters in list" (remove 'c' ['a','b','c','d'] == ['a','b','d']),
   Test "remove last of multiple characters in list" (remove 'd' ['a','b','c','d'] == ['a','b','c']),
   Test "number not in list of length 1" (remove 1 [2] == [2]),
   Test "number not in list longer than 1" (remove 1 [2,3,4,5,6] == [2,3,4,5,6]),
   Test "character not in list of length 1" (remove 'a' ['b'] == ['b']),
    Test "character not in list of length 1" (remove 'a' ['b','c','d','e'] == ['b','c','d','e'])
   ]

removeListTestSuite = TestSuite "Test good removeList examples"
   [Test "no elements in first list" (removeList [] [[1,2],[2,3],[3,4]] == [[1,2],[2,3],[3,4]]),
   Test "no elements in second list" (removeList [1,2] [[]] == [[]]),
   Test "1 number pair in list" (removeList [1,2] [[1,2]] == [[]]),
   Test "multiple numbers in list" (removeList [5,6] [[1,2],[3,4],[5,6],[7,8]] == [[1,2],[3,4],[7,8]]),
   Test "remove first of multiple number pairs in list" (removeList [1,2] [[1,2],[3,4],[5,6],[7,8]] == [[3,4],[5,6],[7,8]]),
   Test "remove last of multiple number pairs in list" (removeList [7,8] [[1,2],[3,4],[5,6],[7,8]] == [[1,2],[3,4],[5,6]]),
   Test "1 character pair in list" (removeList ['a','b'] [['a','b']] == [[]]),
   Test "remove first of multiple character pairs in list" (removeList ['a','b'] [['a','b'],['c','d'],['e','f']] == [['c','d'],['e','f']]),
   Test "multiple character pairs in list" (removeList ['c','d'] [['a','b'],['c','d'],['e','f']] == [['a','b'],['e','f']]),
   Test "remove last of multiple character pairs in list" (removeList ['e','f'] [['a','b'],['c','d'],['e','f']] == [['a','b'],['c','d']]),
   Test "number pair not in list of length 1" (removeList [1,2] [[2,3]] == [[2,3]]),
   Test "number pair not in list longer than 1" (removeList [1,2] [[2,3],[4,5],[6,7]] == [[2,3],[4,5],[6,7]]),
   Test "character pair not in list of length 1" (removeList ['a','b'] [['b','c']] == [['b','c']]),
    Test "character pair not in list of length 1" (removeList ['a','b'] [['b','c'],['d','e'],['e','f']] == [['b','c'],['d','e'],['e','f']])
   ]

removeAllTestSuite = TestSuite "Test good removeAll examples"
   [Test "no elements in first list" (removeAll [] [1,2,3,4,5] == []),
   Test "no elements in second list" (removeAll [1,2,3,4,5] [] == [1,2,3,4,5]),
   Test "1 number in lists" (removeAll [1] [1] == []),
   Test "multiple numbers in lists" (removeAll [1,2,3,4,5,6] [5,6]== [1,2,3,4]),
   Test "one number in second list is in first list, one isn't" (removeAll [1,2,3,4,5,6,7,8] [1,9] == [2,3,4,5,6,7,8]),
   Test "remove first and last elements in list" (removeAll [1,8] [1,2,3,4,5,6,7,8] == [2,3,4,5,6,7]),
   Test "1 character in list" (removeAll ['a'] ['a'] == []),
   Test "remove first and last of multiple characters in list" (removeAll ['a','f'] ['a','b','c','d','e','f'] == ['b','c','d','e']),
   Test "multiple characters in list" (removeAll ['c','d'] ['a','b','c','d','e','f'] == ['a','b','e','f']),
   Test "number not in list of length 1" (removeAll [1] [2] == [2]),
   Test "number not in list longer than 1" (removeAll [1] [2,3,4,5,6,7] == [2,3,4,5,6,7]),
   Test "numbers not in list longer than 1" (removeAll [1,2] [3,4,5,6,7] == [3,4,5,6,7]),
   Test "character not in list of length 1" (removeAll ['a'] ['b'] == ['b']),
   Test "characters not in list of longer than 1" (removeAll ['a','b'] ['c','d','e','f'] == ['c','d','e','f'])
   ]

deleteLastTestSuite = TestSuite "Test good deleteLast examples"
   [--Test "no elements in list" (deleteLast [] == []),
   Test "1 number in list" (deleteLast [1] == []),
   Test "multiple numbers in list" (deleteLast [1,2,3,4,5,6,7,8,9] == [1,2,3,4,5,6,7,8]),
   Test "1 character in list" (deleteLast ['a'] == []),
   Test "multiple characters in list" (deleteLast ['a','b','c','d'] == ['a','b','c'])
   ]

myReverseTestSuite = TestSuite "Test good myReverse examples"
  [--Test "no elements in list" (myReverse [] == []),
   Test "1 number in list" (myReverse [1] == [1]),
   Test "multiple numbers in list" (myReverse [1,2,3,4,5,6,7,8,9] == [9,8,7,6,5,4,3,2,1]),
   Test "1 character in list" (myReverse ['a'] == ['a']),
   Test "multiple characters in list" (myReverse ['a','b','c','d'] == ['d','c','b','a'])
   ]

minAndMaxTestSuite = TestSuite "Test good minAndMax examples"
  [--Test "no elements in list" (minAndMax [] == []),
   Test "1 number in list" (minAndMax [1] == [1,1]),
   Test "multiple numbers in list" (minAndMax [1,2,3,4,5,6,7,8,9] == [1,9]),
   Test "1 character in list" (minAndMax ['a'] == ['a']),
   Test "multiple characters in list" (minAndMax ['a','b','c','d'] == ['a','d'])
   ]