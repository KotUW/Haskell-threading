import Control.Concurrent
import Control.Monad
import System.Environment
import System.Exit
import System.Random

type MessageBox = MVar (Int, String)
type User = Int

-- Helper function to generate a receiver ID that is different from the sender's ID
getReceiver :: Int -> Int -> IO Int
getReceiver numUsers senderId = do
    receiverId <- randomRIO (0, numUsers - 1)
    if receiverId == senderId
        then getReceiver numUsers senderId
        else return receiverId

-- Simulate a user's activity
userActivity :: User -> Int -> [MessageBox] -> IO ()
userActivity user numMessages messageBoxes = do
    putStrLn $ "User " ++ show user ++ " is active."
    forM_ [1..numMessages] $ \msgNum -> do
        let message = "Hello from user " ++ show user ++ ", message " ++ show msgNum
        receiver <- getReceiver (length messageBoxes) user
        putMVar (messageBoxes !! receiver) (user, message)
        -- threadDelay 100  -- Simulate time delay for user activity
    --     maybeMessage <- tryReadMVar (messageBoxes !! user)
    --     case maybeMessage of
    --         Nothing -> putStrLn "Skipping Message not meant for me."
    --         Just (sender, receivedMessage) -> putStrLn $ "User " ++ show user ++ " received a message from user " ++ show sender ++ ": " ++ receivedMessage
    putStrLn $ "User " ++ show user ++ " has finished their activity."

main :: IO ()
main = do
    args <- getArgs
    if length args<2 then do
        putStrLn "Invalid number of Args."
        putStrLn "Usage: stack run <num of users> <num of messages>"
        exitWith (ExitFailure 1)
    else do
        let numUsers = read (args !! 0) :: Int
        let numMessages = read (args !! 1) :: Int
        putStrLn $ "Simulating " ++ show numUsers ++ " users, each sending " ++ show numMessages ++ " messages."
        messageBoxes <- replicateM numUsers newEmptyMVar
        let users = [0..numUsers-1]
        forM_ users $ \user -> do
            forkIO (userActivity user numMessages messageBoxes)
        -- Wait for user input to end the program
        -- _ <- getLine
        -- putStrLn "Simulation ended."
