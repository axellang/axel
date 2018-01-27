%%%MACRO_DEFINITION%%%

main :: IO ()
main = putStrLn $ unlines $ map toAxel $ %%%MACRO_NAME%%% %%%ARGUMENTS%%%
