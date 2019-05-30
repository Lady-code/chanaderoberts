import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String

-- available directions for the player in the game
type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("room1", "n"), "room2"),
    (("room2", "e"), "room3"),
    (("room2", "s"), "room1"),
    (("room3", "n"), "room4"),
    (("room3", "w"), "room2"),
    (("room4", "n"), "room5"),
    (("room4", "e"), "room6Locked"),
    (("room4", "s"), "room3"),
    (("room5", "s"), "room4"),
    (("room6", "e"), "room7"),
    (("room6", "w"), "room4"),
    (("room7", "w"), "room6")
    ]

-- locations of all item in the game
type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("myself", "room1"),
    ("ruby", "room7"),
    ("key", "room5"),
    -- This is a hack, so I don't have to add more lists to the "World" state
    ("room1", "alive")
    ]

type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")

main :: IO (String)
main = do
    putStrLn "\nWelcome to the my game!\n"
    putStrLn gameRoomsMap
    putStrLn instructions
    putStr room1
    play_game ( return (paths, locations, ""))
    return "Goodbye!"

gameRoomsMap =
  "Map\n" ++
  "    _____\n" ++
  "    | 5 |________\n" ++
  "____| 4 ! 6 ! 7 |\n" ++
  "| 2 ! 3 |--------\n" ++
  "| 1 |----\n" ++
  "-----\n"

instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w         -- to go in that direction.\n" ++
    "pick               -- to pick up the item.\n" ++
    "quit               -- to end the game and quit.\n"

play_game :: IO (World) -> IO (World)
play_game world = do
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    if response == win
       then return (paths, locations, "Quitting.")
    else if response == lost
       then return (paths, locations, "Quitting.")
    else do
      putStr "command> "
      command <- getLine
      if command == "quit"
         then return (paths, locations, "Quitting.")
      else play_game ( return (do_command command paths locations))

-- changes the location of the player
move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "n" paths locations = go "n" paths locations
do_command "e" paths locations = go "e" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "w" paths locations = go "w" paths locations
do_command "u" paths locations = go "u" paths locations
do_command "pick" paths locations = pickItem paths locations
do_command _ paths locations = (paths, locations, "Invalid Input!")

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
            let my_location = get "myself" locations
            let new_location = move my_location direction paths
            let key_location = get "key" locations
            if new_location == "room6Locked" && key_location == "myself" then do -- if the room6 is locked and player has a key, then room6Locked will be changed into room6, the paths map will update
              let new_paths = delete (("room4", "e"), "room6Locked") paths
              let final_paths = (("room4", "e"), "room6") : new_paths
              let new_locations = put "myself" "room6" locations
              let response = describe "room6" new_locations
              (final_paths, new_locations, response)
            else if new_location == "room6Locked" then -- if the room6 is locked, the player left in room4
              (paths, locations, "Room 6 is locked. You need to find a key to open the door! You are still in the room4.")
            else do
              let new_locations = put "myself" new_location locations
              let response = describe new_location new_locations
              (paths, new_locations, response)

pickItem :: PathMap -> LocationMap -> World
pickItem paths locations = do
  let my_location = get "myself" locations
  let ruby_location = get "ruby" locations
  let key_location = get "key" locations
  if my_location == ruby_location then do
    let new_locations = delete ("ruby", ruby_location) locations
    let final_locations = put "ruby" "myself" new_locations
    (paths, final_locations, "You pick up the ruby!")
  else if my_location == key_location then do
    let new_locations = delete ("key", ruby_location) locations
    let final_locations = put "key" "myself" new_locations
    (paths, final_locations, "You pick up the key!")
  else (paths, locations, "Nothing to pick up!")

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> ""

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "myself" locations
        ruby_location = get "ruby" locations
        key_location = get "key" locations
    in describe_helper here ruby_location key_location locations

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "room5" "myself" "myself" locations = description "win"
describe_helper "room2" "room7" "myself" locations = description "lose"
describe_helper here _ _ locations = description here

description :: Location -> String

win = "Congratulations!! You have recovered the ruby and won the game."
lost = "You found the key, but you don't have the ruby. You Lost!"

room1 = "You are in a giant room1, standing in a rough\n" ++
    "mat of coarse hair. The smell is awful. You can go north or east.\n"

description "win" = win

description "lose" = lost

description "room1" = room1

description "room2" =
    "There is a giant room2 here!  One hairy leg, about the\n" ++
    "size of a telephone pole, is directly in front of you!\n" ++
    "I would advise you to leave promptly and quietly....\n" ++
    "You can go east only."

description "room3" =
    "You are in a room3.  To the north is the dark mouth\n" ++
    "of a room4; Your assignment, should you decide to accept it, is to\n" ++
    "recover the famed Bar-Abzad ruby. To the west is room2."

description "room4" =
    "You are in the mouth of a dank room4.  The exit is to\n" ++
    "the south; there is a large, dark, round passage to\n" ++
    "the north."

description "room5" =
    "There is a giant room5 here! You can see a key on the floor! It may open the room6!"

description "room6" =
    "This is a big room6. Scary place to be. To the east is room7, to the west room4. Ruby can be there!"

description "room7" =
    "This is a big room7. You can see a ruby on the floor! Enter the room5 to win the game!"

description someplace = someplace ++ ", and you can't see anything."
