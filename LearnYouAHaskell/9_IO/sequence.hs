-- sequence :: [IO a] -> IO [a]
{-  
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

is equal to this next main-}
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs