--Danielle Sarafian
--Functional Exercises--4/20/2020
module Exercises where

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
