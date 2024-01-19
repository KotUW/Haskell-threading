# prosoc

## Requirements (Learnouts)

- Your project should define an appropriate Haskell data type User, which should include their username, and a Message type to keep track of messages between users.
- Your project should also contain a main thread which creates ten “users” (ten values of type User), and spawns ten threads, one for each of these users.
- Each user thread should behave as follows: at random time intervals, the thread should select one of the other users at random, and send a random message to that user.
- Your system should simulate 100 messages, and then terminate and output the final count of how many messages each user received.
- Make sure each definition includes haddock style comments.
- You should also write a one-page report, detailing any issues you have faced, and justifying any design decisions you’ve had to make (e.g. how you chose to model the User and Message types, and how did you choose which MVars to use).

## Requirments 

Have a data type User that contains it's username and type Message.

In Message type, It records the messagte and which user it is send to.

## Program flow

Main, asks for number of threads. defaults to 10 (when given non-intractive flag).
Main, creates **n** no. of users. Where **n** depend upon the answer of above.
Main, forks the function `userThread` depending upon the number of users.

userThread, It randomly selects a user.
userThread, It randomly selects a message (or generates it).
userThread, The message is saved into Message typedef and,
userThread, gets printed with format  `{ThreadUser} -> {RandomUser} : {Message}`  

