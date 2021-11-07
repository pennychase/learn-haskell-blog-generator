
-- Exercises and examples from Section 5

-- IO Action example

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n" *>
            confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()

main1 :: IO ()
main1 =
  putStrLn "This program will tell you a secret" *>
    whenIO confirm (putStrLn "IO is actually pretty awesome") *> 
      putStrLn "Bye"


greeting :: IO ()
greeting =
  putStrLn "Tell me your name." *>
    let
      greet name = "Hello, " ++ name ++ "!"
    in
      getLine >>= putStrLn . greet
      -- getLine >>= \name ->
      --   putStrLn (greet name)
