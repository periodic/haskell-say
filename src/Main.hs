module Main where

main :: IO ()
main = haskellSays "Hello, World!"

-- | Have the Haskell logo say something.
haskellSays :: String -> IO ()
haskellSays str = mapM_ putStrLn $ headerLines ++ content ++ haskellLogoSays
  where
    headerLines = [
      " " ++ replicate 58 '-',
      "/" ++ replicate 58 ' ' ++ "\\"
      ]
    content = map padLine (wrapLine 56 str)

-- Produces: 60 column wide "| <content> |"
padLine :: String -> String
padLine content = ("| " ++) content ++ rightPadding ++ "|"
  where
    rightPadding = replicate paddingWidth ' '
    paddingWidth = 60 - 2 - length content - 1


haskellLogoSays :: [String]
haskellLogoSays = [
    "\\                                                          /",
    " \\____       _____________________________________________/",
    "      \\    /",
    "       \\  /",
    "        \\/",
    "  _____   _____",
    "  \\    \\  \\    \\",
    "   \\    \\  \\    \\",
    "    \\    \\  \\    \\",
    "     \\    \\  \\    \\  \\-----------|",
    "      \\    \\  \\    \\  \\          |",
    "       \\    \\  \\    \\  \\---------|",
    "       /    /  /     \\",
    "      /    /  /       \\  \\-------|",
    "     /    /  /    ^    \\  \\      |",
    "    /    /  /    / \\    \\  \\ ----|",
    "   /    /  /    /   \\    \\",
    "  /____/  /____/     \\____\\"
              ]


-- | Wraps the given 'str' to lines of at most 'width' columns.
wrapLine :: Int -> String -> [String]
wrapLine width str = go (words str) 0 [] []
  where
    go :: [String] -> Int -> [String] -> [String] -> [String]
    go [] _ currentLine wrappedLines = wrappedLines ++ [unwords currentLine]
    go (w:ws) n currentLine wrappedLines
      -- If the next word does fits on the currentLine, add the current line to
      -- the wrappedLines and start a new line.
      | n + (length w) + length currentLine > width =
          go (w:ws) 0 [] (wrappedLines ++ [unwords currentLine])
      -- If the next word does fit on the currentLine, add it to the end.
      | otherwise =
          go ws (n + length w) (currentLine ++ [w]) wrappedLines
