import Text.Regex.TDFA


main = do
    let p =  makeRegex "([a-z]+[ ]*)*" :: Regex
    print $ matchAllText p "ala ma kota"
    print "end"