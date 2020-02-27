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

-- | Exchange the passengers from stop and bus capacity.
performStop :: Stop->Bus->Bus
performStop stop (passengers, max) = case stop of
                       Gain cap -> if (busHandle (passengers, max) cap) then (passengers+cap, max) else (max, max)
                       Loss cap -> if (busHandle (passengers, max) (-cap)) then (passengers-cap, max) else (0, max)



-- | Bus handles each stop in a [Route].
schedule :: Bus -> [Route] -> Bus 
schedule bus [] = bus
schedule bus (r:oute) = case r of
                        Halt stop   -> schedule (performStop stop bus) oute
                        Go distance -> schedule bus oute



-- | Functions about Bus state
--
-- Is the bus full?
busFull :: Bus -> Bool
busFull (passengers, max) = if passengers == max then True else False

-- Is the bus empty?
busEmpty :: Bus -> Bool
busEmpty (passengers, max) = if passengers == 0 then True else False

-- Do passenger exchanges make sense?
busHandle :: Bus -> Capacity -> Bool
busHandle (passengers, max) netchange = if ( (passengers + netchange <= max) && (passengers + netchange >= 0) ) then True else False



-- | Testing examples
exampleRoute :: [Route]
exampleRoute = [Go 15, Halt (Gain 1), Go 5, Halt (Gain 10), Go 20, Halt (Loss 5)]

exampleRun1 :: Bus
exampleRun1 = schedule (0, 30) exampleRoute
