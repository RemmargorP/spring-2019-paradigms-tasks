import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        mywalter = robot "Walter" 50 123
        dummy = robot "Dummy" 1 0
        grandmaster = robot "Grand Master Kolbaster" 99 1337
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for getAttack" $
            getAttack walter @?= 50

        , testCase "Test for getHealth" $
            getHealth mywalter @?= 123

        , testCase "Test for setName" $
            setName "Roflan" mywalter @?= robot "Roflan" 50 123

        , testCase "Test for setAttack" $
            setAttack 30 mywalter @?= robot "Walter" 30 123

        , testCase "Test for setHealth" $
            setHealth 239 mywalter @?= robot "Walter" 50 239

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for damage" $
            damage mywalter 100 @?= robot "Walter" 50 23

        , testCase "Test for isAlive" $ do
            isAlive mywalter @?= True
            isAlive dummy @?= False

        , testCase "Test for fight" $ do
            fight mywalter dummy @?= robot "Dummy" 1 (-50)
            fight dummy mywalter @?= mywalter

        , testCase "Test for threeRoundFight" $ do
            threeRoundFight mywalter dummy @?= mywalter
            threeRoundFight mywalter grandmaster @?= grandmaster
            threeRoundFight grandmaster (robot "Ex-Grand Master" (337+99+99) 1000) @?= grandmaster

        , testCase "Test for survivors" $ -- Для остальных функций из шага 4 не имеет смысла делать тесты.
            survivors @?= [robot "Alpha" 123 456, robot "Gamma" 301 1337]
        ]
