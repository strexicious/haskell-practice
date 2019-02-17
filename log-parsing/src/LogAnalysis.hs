{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = validateMessage . getMessageType

getMessageType :: String -> (Maybe MessageType, String)
getMessageType ('I':' ':rest) = (Just Info, rest)
getMessageType ('W':' ':rest) = (Just Warning, rest)
getMessageType ('E':' ':rest) = (Just (Error (read prio)), afterPrio)
    where (prio, afterPrio) = getWord (rest, "")
getMessageType rest           = (Nothing, rest)

validateMessage :: (Maybe MessageType, String) -> LogMessage
validateMessage (Just msgType, rest) = LogMessage msgType (read timestamp) msg
    where (timestamp, msg) = getWord (rest, "")
validateMessage (Nothing, rest)         = Unknown rest

getWord :: (String, String) -> (String, String)
getWord (      "", ret) = (ret, "")
getWord (' ':rest, ret) = (ret, rest)
getWord (l  :rest, ret) = getWord (rest, ret ++ [l])

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert msg Leaf    = Node Leaf msg Leaf
insert msg @ (LogMessage _ stamp _) (Node left old @ (LogMessage _ ostamp _) right)
    | stamp < ostamp = Node (insert msg left) old right
    | stamp > ostamp = Node left old (insert msg right)

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (m:ms) = insert m $ build ms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

severError :: LogMessage -> Bool
severError (Unknown _) = False
severError (LogMessage (Error prio) _ _)
    | prio < 50 = False
    | otherwise = True
severError (LogMessage _ _ _) = False

msgString :: LogMessage -> String
msgString (Unknown str) = str
msgString (LogMessage _ _ str) = str

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map msgString . filter severError . inOrder . build
