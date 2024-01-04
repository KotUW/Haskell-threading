import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.Random (randomRIO)

-- A list of texts to post
texts :: [String]
texts = ["Hello, world!", "How are you?", "Nice weather today.", "I'm a Haskell bot."]

-- A list of user names
users :: [String]
users = ["User1", "User2", "User3", "User3", "User4", "User5", "User6", "User7", "User8"]
postRandomText :: String -> IO ()
postRandomText user = Control.Monad.forever $ do
    -- Choose a random text
    textIndex <- randomRIO (0, length texts - 1)
    let text = texts !! textIndex

    -- Print the text
    putStrLn (user ++ ": " ++ text)

    -- Wait for a random amount of time (between 1 and 5 seconds)
    delay <- randomRIO (1, 5)
    threadDelay (delay * 1000000)  -- threadDelay takes microseconds

main :: IO ()
main = do
    -- Create a new thread for each user
    mapM_ (forkIO . postRandomText) users

    -- Keep the main thread alive indefinitely
    Control.Monad.forever $ threadDelay maxBound
