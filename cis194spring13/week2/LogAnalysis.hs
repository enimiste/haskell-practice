module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage log = 
    case words log of
        "I":(t:xs) -> LogMessage Info (read t) (unwords xs)
        "W":(t:xs) -> LogMessage Warning (read t) (unwords xs)
        "E":(r:(t:xs)) -> LogMessage (Error (read r)) (read t) (unwords xs)
        _ -> Unknown log


parse :: String -> [LogMessage]
parse content = (map parseMessage . lines) content 

insert :: LogMessage -> LogMessageTree -> LogMessageTree
insert msg tree =
    case (msg, tree) of
        (Unknown _, _) -> tree
        (msg, Leaf) -> Node Leaf msg Leaf
        (LogMessage _ t1 _, Node l mg@(LogMessage _ t _) r) | t1 <= t -> Node (insert msg l) mg r
                                                            | t1 > t -> Node l mg (insert msg r)
build :: [LogMessage] -> LogMessageTree
build = build' Leaf
    where build' tree [] = tree
          build' tree (x:xs) = build' (insert x tree) xs 

inOrder :: LogMessageTree -> [LogMessage]
inOrder tree =
    case tree  of
        Leaf -> []
        Node l e r ->  (inOrder l) ++ [e] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . filter isRelevent . inOrder . build
-- whatWentWrong logs = map extractMessage (filter isRelevent (inOrder (build logs)))

isRelevent :: LogMessage -> Bool
isRelevent (LogMessage (Error r) _ _ ) | r > 50 = True
isRelevent _ = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ s ) = s

extractTimestamps :: LogMessage -> TimeStamp
extractTimestamps (LogMessage _ t _ ) = t
