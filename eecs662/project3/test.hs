{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads
import Control.Monad

-- Import p3 stuff
import P3

-- Test Cases

-- Tests for evalDynCFAE and evalStatCFAE.  test2 and test3 should demonstrate
-- the difference between static and dynamic scoping.  If you get the same
-- results with both interpreters, you've got problems.
test0 = (App (Lambda "inc" (Id "inc")) (Lambda "x" (Plus (Id "x") (Num 1))))
test1 = (App (Lambda "inc" (App (Id "inc") (Num 3))) (Lambda "x" (Plus (Id "x") (Num 1))))
test2 = (App (Lambda "n" (App (Lambda "inc" (App (Lambda "n" (App (Id "inc") (Num 3))) (Num 3))) (Lambda "x" (Plus (Id "x") (Id "n"))))) (Num 1))
test3 = (App (Lambda "Sum" (App (Id "Sum") (Num 3))) (Lambda "x" (If0 (Id "x") (Num 0) (Plus (Id "x") (App (Id "Sum") (Minus (Id "x") (Num 1)))))))

-- List of tests if you would like to use map for testing
tests = [test0,test1,test2,test3]

-- Tests for evalCFBAE and evalDynCFAE.  These are the same tests as above
-- using Bind.  You should get the same results from evalCFBAE that you
-- get from evalStatCFAE.
test0' = (Bind' "inc" (Lambda' "x" (Plus' (Id' "x") (Num' 1))) (Id' "inc"))
test1' = (Bind' "inc" (Lambda' "x" (Plus' (Id' "x") (Num' 1))) (App' (Id' "inc") (Num' 3)))
test2' = (Bind' "n" (Num' 1) (Bind' "inc" (Lambda' "x" (Plus' (Id' "x") (Id' "n"))) (Bind' "n" (Num' 3) (App' (Id' "inc") (Num' 3)))))
test3' = (Bind' "Sum" (Lambda' "x" (If0' (Id' "x") (Num' 0) (Plus' (Id' "x") (App' (Id' "Sum") (Minus' (Id' "x") (Num' 1)))))) (App' (Id' "Sum") (Num' 3)))

-- List of tests if you would like to use map for testing
tests' = [test0',test1',test2',test3']

runTests =
    do putStrLn $ ("evalDynCFAE  -- " ++ (show $ map (evalDynCFAE []) tests))
       putStrLn $ ("evalStatCFAE -- " ++ (show $ map (evalStatCFAE []) tests))
       putStrLn $ ("evalCFBAE    -- " ++ (show $ map (evalCFBAE []) tests'))
