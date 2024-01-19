{-# LANGUAGE GADTs #-}
import Control.Concurrent ( forkIO, MVar, takeMVar )
import Control.Monad (replicateM_, replicateM )
import Control.Concurrent.MVar
    ( newEmptyMVar, newMVar, modifyMVar_, readMVar, tryPutMVar )
import GHC.Conc (threadDelay)
import System.Random (randomRIO)
import Data.Foldable (forM_)
import System.Timeout (timeout)

data Message where
  Msg :: String -> Message

data User where
  User :: {userName :: String,
             messagesReceived :: Control.Concurrent.MVar Int,
             inbox :: Control.Concurrent.MVar Message} ->
            User

-- | Function to generate a random user name
generateRandomUserName :: IO String
generateRandomUserName = replicateM 3 (randomRIO ('a', 'z'))

createUser :: IO User
createUser = do
  name <- generateRandomUserName
  messagesCounter <- newMVar 0
  User name messagesCounter <$> newEmptyMVar

getRandomRecipient :: [User] -> IO User
getRandomRecipient recipients = do
  index <- randomRIO (0, 9)
  return $ recipients !! index

userThread :: User -> [User] -> IO ()
userThread user allUsers = do
  let userName' = userName user
  -- totalMessages <- readMVar (messagesReceived user)

  -- Send messages to random users
  replicateM_ 13 $ do
    recipient <- getRandomRecipient allUsers
    let content = userName' ++ ": Hello, " ++ userName recipient ++ "!"
-- Release the lock on the sender's inbox before sending the message
    _ <- sendMessage recipient content
    putStrLn $ userName' ++ " is Sending message"
-- Wait for a short delay to simulate processing time
    -- threadDelay (500 * 1000)  -- 0.5 seconds

    receiveMode user

receiveMode :: User -> IO ()
receiveMode user = do
  putStrLn $ userName user ++ " is entering receive mode..."
  replicateM_ 5 $ do
    putStrLn $ userName user ++ " is waiting for a message..."
    timedReceive user

    modifyMVar_ (messagesReceived user) (\count -> return $ count + 1)
    currentCount <- readMVar (messagesReceived user)
    putStrLn $ userName user ++ " has received " ++ show currentCount ++ " messages so far."

timedReceive :: User -> IO ()
timedReceive user = do
  result <- timeout (3 * 10^6) $ takeMVar (inbox user)  -- Wait for 3 seconds
  case result of
    Just _  -> putStrLn $ userName user ++ " received a message!"
    Nothing -> putStrLn $ userName user ++ " timed out waiting for a message. Moving to the next replicate step."

  modifyMVar_ (messagesReceived user) (\count -> return $ count + 1)
  currentCount <- readMVar (messagesReceived user)
  putStrLn $ userName user  ++ " has received " ++ show currentCount ++ " messages so far."

sendMessage :: User -> String -> IO Bool
sendMessage recipient content = do
  tryPutMVar (inbox recipient) (Msg content)


printMessageCounts :: [User] -> IO ()
printMessageCounts users = do
  putStrLn "\n\n\n---------------------"
  putStrLn "Message counts:"
  forM_ users $ \user -> do
    count <- readMVar (messagesReceived user)
    putStrLn $ userName user ++ ": " ++ show count

main :: IO ()
main = do
  user1 <- createUser
  user2 <- createUser
  user3 <- createUser
  user4 <- createUser
  user5 <- createUser
  user6 <- createUser
  user7 <- createUser
  user8 <- createUser
  user9 <- createUser
  user10 <- createUser

  let users = [user1, user2, user3, user4, user5, user6, user7, user8, user9, user10]
  _ <- Control.Concurrent.forkIO (userThread user1 users)
  _ <- Control.Concurrent.forkIO (userThread user2 users)
  _ <- Control.Concurrent.forkIO (userThread user3 users)
  _ <- Control.Concurrent.forkIO (userThread user4 users)
  _ <- Control.Concurrent.forkIO (userThread user5 users)
  _ <- Control.Concurrent.forkIO (userThread user6 users)
  _ <- Control.Concurrent.forkIO (userThread user7 users)
  _ <- Control.Concurrent.forkIO (userThread user8 users)
  _ <- Control.Concurrent.forkIO (userThread user9 users)
  _ <- Control.Concurrent.forkIO (userThread user10 users)

  -- Wait for threads to finish processing
  threadDelay (5 *1000000 )  -- 5 seconds delay to allow threads to finish

  -- Print message counts
  printMessageCounts users
