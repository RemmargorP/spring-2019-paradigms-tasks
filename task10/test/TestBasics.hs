import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on infinite list" $
        head' [1..] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 5 elements from infinite list" $
        take' 5 [3..] @?= [3, 4, 5, 6, 7]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' drops 3 elements from infinite list" $
        head' (drop' 3 [1..]) @?= 4

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on infinite lists" $
        take' 10 (concat' [1,2,3,4] [5..]) @?= [1..10]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]
