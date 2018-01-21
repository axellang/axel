%%%MACRO_DEFINITION%%%

main :: IO ()
main = %%%MACRO_NAME%%% %%%ARGUMENTS%%% >>= putStrLn . unlines . map toLihsp
