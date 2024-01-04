import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void,  replicateM_)
import System.Random (randomRIO)

-- A list of texts to post
texts :: [String]
texts = ["Hello, world!", "How are you?", "Nice weather today.", "I'm a Haskell bot."]

-- A list of user names
users :: [String]
users = ["User1", "User2", "User3", "User3", "User4", "User5", "User6", "User7", "User8"]
postRandomText :: String -> IO ()
postRandomText user = do
    -- Choose a random text
    textIndex <- randomRIO (0, length texts - 1)
    let text = texts !! textIndex

    -- Print the text
    putStrLn (user ++ ": " ++ text)

userThread 0 = do 
    putStrLn "Terminating Thread."
    
userThread numOfMsgs = do 
    userIndex <- randomRIO(0, length users -1)
    let userId = users !! userIndex

    postRandomText userId
    -- Wait for a random amount of time (between 1 and 5 seconds)
    -- delay <- randomRIO (1, 5)
    -- threadDelay (delay * 1000000)  -- threadDelay takes microseconds

    userThread (numOfMsgs - 1)

-- The thread function
threadFunction :: Int -> IO ()
threadFunction n = replicateM_ n $ userThread 10

main :: IO ()
main = do
    putStr "Enter the number of threads to spawn:"
    n <- readLn :: IO Int

    -- Create the thread
    void $ forkIO $ threadFunction n

    -- Wait for the user to press enter before exiting the main thread
    putStrLn "Press enter to exit."
    _ <- getLine
    return ()
