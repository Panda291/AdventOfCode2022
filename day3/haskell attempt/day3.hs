main = do
    xss <- lines <$> readFile "../src/resources/test_input.txt"
    print xss