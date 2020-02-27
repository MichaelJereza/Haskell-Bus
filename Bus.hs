module Bus where

type Miles = Float
type Capacity = Int
type Name = [Char]
type Bus = (Capacity, Capacity)

data Stop = Gain Capacity 
          | Loss Capacity
          deriving(Eq,Show)
data Route = Halt Stop
           | Go Miles
           deriving(Eq,Show)

--data Task = Run Bus
--          | Def Bus
--          | Map [Route]        
--handleTasks :: [Task]->Bus


performStop :: Stop->Bus->Bus
performStop stop (passengers, max) = case stop of
                       Gain cap -> if passengers+cap<max then (passengers+cap, max) else (max, max)
                       Loss cap -> if passengers-cap>0 then (passengers-cap, max) else (0, max)

schedule :: Bus -> [Route] -> Bus 
schedule bus [] = bus
schedule bus (r:oute) = case r of
                        Halt stop   -> schedule (performStop stop bus) oute
                        Go distance -> schedule bus oute

exampleRoute :: [Route]
exampleRoute = [Go 15, Halt (Gain 1), Go 5, Halt (Gain 10), Go 20, Halt (Loss 5)]

exampleRun1 :: Bus
exampleRun1 = schedule (0, 30) exampleRoute
