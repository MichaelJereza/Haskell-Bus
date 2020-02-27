module Bus where

type Status = (Bus->Bool)
type Miles = Float
type Capacity = Int
type Name = [Char]
type Bus = (Capacity, Capacity)

type Stop = (Name, Exchange)
type Route = [Command]

data Exchange  = Gain Capacity 
               | Loss Capacity
               deriving(Eq,Show)
data Command = Halt Stop
             | Go Miles
             deriving(Eq,Show)
data Task = Run Route
          | While Status Route
          | If Status Route Route
--handleTasks :: Bus->[Task]->Bus
--handleTasks bus [] = bus
--handleTasks bus (task:s) = case task of
--                         Run route -> handleTasks (schedule bus route) s
--                         While condition route -> case (condition bus) of 
--                                                  True -> handleTasks (schedule bus route) (task:s)
 --                                                 False -> handleTasks (schedule bus route) (task:s)
   --                      If status t e-> if status then handleTasks (schedule bus t) s else handleTasks (schedule bus e) s

-- | Exchange the passengers from stop and bus capacity.
performStop :: Stop->Bus->Bus
performStop (name, exchange) (passengers, max) = case exchange of
                       Gain cap -> if (busHandle (passengers, max) cap) then (passengers+cap, max) else (max, max)
                       Loss cap -> if (busHandle (passengers, max) (-cap)) then (passengers-cap, max) else (0, max)



-- | Bus handles each stop in a Route.
schedule :: Bus -> Route -> Bus 
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
exampleRoute :: Route
exampleRoute = [Go 15, Halt ("Street A", (Gain 1)), Go 5, Halt ("Street B", (Gain 1)), Go 20, Halt ("Street C", (Loss 1))]

exampleRoute2 :: Route
exampleRoute2 = [Halt ("Street D", (Loss 1))]

exampleRun1 :: Bus
exampleRun1 = schedule (0, 30) exampleRoute
