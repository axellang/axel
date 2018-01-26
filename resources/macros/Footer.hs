%%%MACRO_DEFINITION%%%

main :: IO ()
main = putStrLn $ unlines $ map toLihsp $ %%%MACRO_NAME%%% %%%ARGUMENTS%%%
