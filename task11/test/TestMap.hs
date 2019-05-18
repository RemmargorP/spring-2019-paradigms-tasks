{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "fromList and toAscList" [
            testCase "fromList is capable to make null map" $
                let map = Map.fromList [] :: m Int Int in
                Map.null map @?= True
            ,
            testCase "fromList is capable to make valid non-empty map" $
                let map = Map.fromList [(1, 2), (3, 4), (5, 6), (7, 8)] :: m Int Int in
                ( Map.size map     == 4 && 
                  Map.lookup 1 map == Just 2 &&
                  Map.lookup 3 map == Just 4 &&
                  Map.lookup 5 map == Just 6 &&
                  Map.lookup 7 map == Just 8 ) @?= True
            ,
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ],
        testGroup "insert" [
            testCase "insert is capable to insert into an empty map" $
                let map  = Map.empty :: m Int Int in
                let map' = Map.insert 1 239 map in
                Map.lookup 1 map' @?= Just 239
            ,
            testCase "insert is capable to insert into a non-empty map" $
                let map  = Map.singleton 1 239 :: m Int Int in
                let map' = Map.insert 2 3 map in
                Map.lookup 2 map' @?= Just 3
            ,
            testCase "insert is capable to replace value" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.insert 1 239 map in
                Map.lookup 1 map' @?= Just 239
        ],
        testGroup "insertWith" [
            testCase "insertWith is capable to insert into an empty map" $
                let map  = Map.empty :: m Int Int in
                let map' = Map.insertWith (+) 1 239 map in
                Map.lookup 1 map' @?= Just 239
            ,
            testCase "insertWith alters value if key exists" $
                let map  = Map.singleton 1 1 :: m Int Int in
                let map' = Map.insertWith (+) 1 239 map in
                Map.lookup 1 map' @?= Just 240
        ],
        testGroup "insertWithKey" [
            testCase "insertWithKey is capable to insert into an empty map" $
                let map  = Map.empty :: m Int Int in
                let map' = Map.insertWithKey (\key new old -> key + new + old) 1 239 map in
                Map.lookup 1 map' @?= Just 239
            ,
            testCase "insertWithKey alters value if key exists" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.insertWithKey (\key new old -> key + new + old) 10 239 map in
                Map.lookup 10 map' @?= Just 250
        ],
        testGroup "delete" [
            testCase "delete doesn't change the map if key doesn't exist" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.delete 7 map in
                ( Map.size map'      == 1 && 
                  Map.lookup 10 map' == Just 1 ) @?= True
            ,
            testCase "delete deletes the key from map if it exists" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.delete 10 map in
                Map.null map' @?= True
        ],
        testGroup "adjust" [
            testCase "adjust doesn't change the map if key doesn't exist" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.adjust (7 +) 3 map in
                ( Map.size map'      == 1 && 
                  Map.lookup 10 map' == Just 1 ) @?= True
            ,
            testCase "adjust updates the value if key exists" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.adjust (7 +) 10 map in
                Map.lookup 10 map' @?= Just 8
        ],
        testGroup "adjustWithKey" [
            testCase "adjustWithKey doesn't change the map if key doesn't exist" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.adjust (7 +) 3 map in
                ( Map.size map'      == 1 && 
                  Map.lookup 10 map' == Just 1 ) @?= True
            ,
            testCase "adjustWithKey updates the value if key exists" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.adjust (7 +) 10 map in
                Map.lookup 10 map' @?= Just 8
        ],
        testGroup "update" [
            testCase "update doesn't change the map if key doesn't exist" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.update (\x -> Just (x + 1)) 3 map in
                ( Map.size map'      == 1 && 
                  Map.lookup 10 map' == Just 1 ) @?= True
            ,
            testCase "update updates the value if key exists" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.update (\x -> Just (x + 1)) 10 map in
                Map.lookup 10 map' @?= Just 2
            ,
            testCase "update deletes the key if function returns Nothing" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.update (\x -> Nothing) 10 map in
                Map.null map' @?= True
        ],
        testGroup "updateWithKey" [
            testCase "updateWithKey doesn't change the map if key doesn't exist" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.updateWithKey (\k x -> Just (k + x + 1)) 3 map in
                ( Map.size map'      == 1 && 
                  Map.lookup 10 map' == Just 1 ) @?= True
            ,
            testCase "updateWithKey updates the value if key exists" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.updateWithKey (\k x -> Just (k + x + 1)) 10 map in
                Map.lookup 10 map' @?= Just 12
            ,
            testCase "updateWithKey deletes the key if function returns Nothing" $
                let map  = Map.singleton 10 1 :: m Int Int in
                let map' = Map.updateWithKey (\k x -> Nothing) 10 map in
                Map.null map' @?= True
        ],
        testGroup "alter and lookup" [
            testCase "alter is capable to insert into an empty map" $
                let map  = Map.empty :: m Int Int in
                let map' = Map.alter (const $ Just 239) 1 map in
                Map.lookup 1 map' @?= Just 239
            ,
            testCase "alter is capable to insert into a non-empty map" $ 
                let map  = Map.singleton 1 239 :: m Int Int in
                let map' = Map.alter (const $ Just 37) 2 map in
                ( Map.lookup 1 map' == Just 239 &&
                  Map.lookup 2 map' == Just 37 ) @?= True
            ,
            testCase "alter is capable to alter the value if key exists" $
                let map  = Map.singleton 1 239 :: m Int Int in
                let map' = Map.alter (const $ Just 37) 1 map in
                Map.lookup 1 map' @?= Just 37
            ,
            testCase "alter is capable to delete the key if function returns Nothing" $
                let map  = Map.singleton 1 239 :: m Int Int in
                let map' = Map.alter (const $ Nothing) 1 map in
                Map.null map' @?= True
        ],
        testGroup "member" [
            testCase "member returns True if key exists" $
                let map = Map.singleton 1 239 :: m Int Int in
                Map.member 1 map @?= True
            ,
            testCase "member returns False if key doesn't exist" $
                let map = Map.singleton 1 239 :: m Int Int in
                Map.member 2 map @?= False
        ],
        testGroup "notMember" [
            testCase "notMember returns True if key exists" $
                let map = Map.singleton 1 239 :: m Int Int in
                Map.notMember 1 map @?= False
            ,
            testCase "notMember returns False if key doesn't exist" $
                let map = Map.singleton 1 239 :: m Int Int in
                Map.notMember 2 map @?= True
        ],
        testGroup "empty and singleton" [
            testCase "empty returns an empty map" $ 
                let map = Map.empty :: m Int Int in
                Map.null map @?= True
            ,
            testCase "singleton returns a singleton map" $
                let map = Map.singleton 1 1 :: m Int Int in
                Map.size map @?= 1
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList)
        -- mapTests "NaiveTree" (Proxy :: Proxy NaiveTree)
        -- testNaiveTree
    ]
