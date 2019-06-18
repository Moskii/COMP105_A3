import System.Environment
import System.IO

maze_path = "F:\\University Work\\Computer Science Lectures\\Programming Language Paradigm\\Assignment 3\\haskellPaths\\maze-big-4.txt"

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

get_maze :: String -> IO [String]
get_maze file = do
    -- unboxing
    x <- readFile file
    let
      formatted = lines x
    -- returns the string as IO [String]
    return formatted

print_maze :: [String] -> IO ()
-- putStrLn ensures the return type is IO()
print_maze str = putStrLn (unlines str)

is_wall :: [String] -> (Int, Int) -> Bool
-- 'Str' is the maze.
is_wall str (int1,int2) =
    let
      -- From the input maze, it checks whether there is a '#' on that coordinate.
      getting = get str int1 int2
    in
      if getting == '#' then True else False

place_player :: [String] -> (Int, Int) -> [String]
-- PLaces a '@' symbol at the designated coordinate
place_player str (int1,int2) = set str int1 int2 '@'

move :: (Int, Int) -> Char -> (Int, Int)
--Depeding on the 'chr' input will determine whether the coordinates will need to change
move (int1,int2) chr
  | chr == 'w' = (int1, int2 - 1)
  | chr == 's' = (int1, int2 + 1)
  | chr == 'a' = (int1 - 1, int2)
  | chr == 'd' = (int1 + 1, int2)
  | otherwise = (int1,int2) --The 'catch all' statement

can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move str (int1,int2) chr =
    let
      moving = move (int1,int2) chr
      checks = is_wall str moving --This checks if there is a wall.
    in
      if checks == False then True else False --If there is no wall then the '@' is allowed to move

game_loop :: [String] -> (Int, Int) -> IO ()
game_loop str (int1,int2) = 
  do
    print_maze (place_player str (int1,int2))
    x <- getLine
    let
      --gets the head of string i.e the character.
      conv = head x
      moves = can_move str (int1,int2) conv
      new_coords = if moves == True then (move (int1,int2) conv) else (int1,int2)
    game_loop str new_coords

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path = error "Not implemented"
-- TBC

main :: IO ()
main = error "Not implemented"
--TBC
