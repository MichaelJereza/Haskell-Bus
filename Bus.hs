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
          | While Status Task
          | If Status Task Task


-- | This is the main wrapper for handling tasks and running the program.
displayTasks :: Bus->[Task]->IO ()
displayTasks bus tasks = putStrLn ("\nStarting: " ++ show bus ++ ".\n" ++ handleTasks bus tasks)



-- | Iterates through Task list and returns strings of operations.
handleTasks :: Bus->[Task]->String
handleTasks bus [] = "\n\nFinished: " ++ show bus ++ ".\n"
handleTasks bus ((Run route):s) = case schedule bus route of 
                                  bus' -> "\nRunning route: " ++ rootInfo route ++ "\nBus: " ++ show bus' ++ "." ++ handleTasks (schedule bus route) s
handleTasks bus ((While condition task):s) = case (condition bus) of
                                             True  -> "\nWhile: True..." ++ handleTasks bus (task:(While condition task):s)
                                             False -> "\nWhile: False..." ++ handleTasks bus s
handleTasks bus ((If status t e):s) = if (status bus) then "\nIf: True..." ++ handleTasks bus (t:s) 
                                                      else "\nIf: False..." ++ handleTasks bus (e:s)


-- | Return a string representing a route.
rootInfo :: Route->String
rootInfo [] = ""
rootInfo (r:oute) = case r of
                    Halt (name, exchange) -> name ++ ": " ++ show exchange ++ ", " ++ rootInfo oute
                    Go miles              -> "drove " ++ show miles ++ " miles, " ++ rootInfo oute




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

-- Are there passengers on the bus?
busHasPassengers :: Bus -> Bool
busHasPassengers (passengers, max) = if passengers > 0 then True else False

busHasRoom :: Bus -> Bool
busHasRoom (passengers, max) = if passengers < max then True else False

-- Do passenger exchanges make sense?
busHandle :: Bus -> Capacity -> Bool
busHandle (passengers, max) netchange = if ( (passengers + netchange <= max) && (passengers + netchange >= 0) ) then True else False



-- | Testing examples
-- Gain 5
exampleRoute :: Route
exampleRoute = [Go 15, Halt ("Street A", (Gain 1)), Go 5, Halt ("Street B", (Loss 1)), Go 20, Halt ("Street C", (Gain 5))]
-- Lose 1
exampleRoute2 :: Route
exampleRoute2 = [Go 0.5, Halt ("Street D", (Loss 1))]

exampleRun1 :: Bus
exampleRun1 = schedule (0, 30) exampleRoute

-- Fills 5 passengers, loops emptying 1 passenger at a time
exampleTask1 :: IO ()
exampleTask1 = displayTasks (0, 20) [Run exampleRoute, While busHasPassengers (Run exampleRoute2) ]

-- Fills bus, then loops emptying 1 passenger at a time
exampleTask2 :: IO ()
exampleTask2 = displayTasks (1, 20) [While busHasPassengers (If busFull (While busHasPassengers (Run exampleRoute2)) (Run exampleRoute))] 

-- An example of a failure, looping forever.
infiniteTask :: IO ()
infiniteTask = displayTasks (20, 20) [While busHasPassengers (If busFull (Run exampleRoute2)(Run exampleRoute))]


exampleFail1 :: Bus
exampleFail1 = schedule (5, 10) [Go 5, Halt ("Street A", (Gain 10))]

exampleFail2 :: Bus
exampleFail2 = schedule (0, 5) [Go 5, Halt ("Street B", (Loss 1))]
