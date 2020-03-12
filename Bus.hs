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
          | Create Name Route
          | Use Name
          | While Status Task
          | If Status Task Task

type Var = (Name, Route)

-- | This is the main wrapper for handling tasks and running the program.
displayTasks :: Bus->[Task]->IO ()
displayTasks bus tasks = putStrLn ("\nStarting: " ++ show bus ++ ".\n" ++ handleTasks bus tasks empty)



-- | Initial Variable environment called by displayTasks
empty :: [Var]
empty = []



-- | Iterates through Task list and returns strings of operations.
handleTasks :: Bus->[Task]->[Var]->String
handleTasks bus [] env = "\n\nFinished: " ++ show bus ++ ".\n"
handleTasks bus ((Run route):s) env = case schedule bus route of 
                                      bus' -> "\nRunning route: " ++ rootInfo route ++ "\nBus: " ++ show bus' ++ "." ++ handleTasks (schedule bus route) s env
handleTasks bus ((While condition task):s) env = case (condition bus) of
                                             True  -> "\nWhile: True..." ++ handleTasks bus (task:(While condition task):s) env
                                             False -> "\nWhile: False..." ++ handleTasks bus s env
handleTasks bus ((If status t e):s) env = if (status bus) then "\nIf: True..." ++ handleTasks bus (t:s) env
                                                      else "\nIf: False..." ++ handleTasks bus (e:s) env
handleTasks bus ((Create name route):s) env = case getVar name env of
                                              Just _ -> "\nFailed creating " ++ name ++ ", variables already exists!"
                                              Nothing -> handleTasks bus s (env++[createVar name route])
handleTasks bus ((Use name):s) env = case getVar name env of
                                     Just route -> "\nRunning variable: " ++ name ++ "." ++ handleTasks bus ((Run route):s) env
                                     Nothing -> "\nVariable " ++ name ++ " does not exist!"



-- | Get Var from name return route
getVar :: Name->[Var]->Maybe Route
getVar name [] = Nothing
getVar name ((var, route):variables) = if name == var then Just route else getVar name variables



-- | Create Var from Name and Route
createVar :: Name->Route->Var
createVar name route = (name, route)



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
exampleRoute = [Go 15, Halt ("Stop A", (Gain 1)), Go 5, Halt ("Stop B", (Loss 1)), Go 20, Halt ("Stop C", (Gain 5))]
-- Lose 1
exampleRoute2 :: Route
exampleRoute2 = [Go 0.5, Halt ("Stop D", (Loss 1))]

exampleRun1 :: Bus
exampleRun1 = schedule (0, 30) exampleRoute

-- Fills 5 passengers, loops emptying 1 passenger at a time
exampleTask1 :: IO ()
exampleTask1 = displayTasks (0, 20) [Run exampleRoute, While busHasPassengers (Run exampleRoute2) ]

-- Fills bus, then loops emptying 1 passenger at a time
exampleTask2 :: IO ()
exampleTask2 = displayTasks (1, 20) [While busHasPassengers (If busFull (While busHasPassengers (Run exampleRoute2)) (Run exampleRoute))] 

-- Defines "myroute" route
exampleVariable1 :: IO ()
exampleVariable1 = displayTasks (1, 20) [Create "myroute" [Go 15, Halt ("Stop Z", (Gain 1)), Go 3.5], While busHasRoom (Use "myroute")] 

-- Defines "myroute" and "youroute"
exampleVariable2 :: IO ()
exampleVariable2 = displayTasks (1, 20) [Create "myroute" [Go 15, Halt ("Stop Z", (Gain 1)), Go 3.5], Create "youroute" [Go 1.2, Halt ("Stop X", (Loss 1)), Go 0.2], While busHasRoom (Use "myroute"), While busHasPassengers (Use "youroute")] 

-- An example of a failure, looping forever.
infiniteTask :: IO ()
infiniteTask = displayTasks (20, 20) [While busHasPassengers (If busFull (Run exampleRoute2)(Run exampleRoute))]

exampleFail1 :: Bus
exampleFail1 = schedule (5, 10) [Go 5, Halt ("Stop A", (Gain 10))]

exampleFail2 :: Bus
exampleFail2 = schedule (0, 5) [Go 5, Halt ("Stop B", (Loss 1))]
