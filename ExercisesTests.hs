module ExercisesTests where

import Exercises
import TestSuiteSupportModule

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