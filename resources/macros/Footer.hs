%%%MACRO_DEFINITION%%%

main :: IO ()
main = do result <- %%%MACRO_NAME%%% %%%ARGUMENTS%%%
          putStrLn $ unlines $ map toAxel result
