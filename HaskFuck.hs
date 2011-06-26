{-
Experimental (i.e. not working properly) brainfuck interpreter in Haskell
The test program below works but others may not
The , (comma) command has not been implemented yet

I mainly wrote this to try to wrap my head around monad transformers and improve
my general Haskell skills.  Please feel free to steal from/modify but bear in
mind I am no Haskell expert (yet) so it may not be a good example to learn from!

-}

import Control.Monad.State
import Data.Maybe
import Control.Monad.Reader
import Data.Array.IO
import Data.List
import Data.Char
import System.Environment

-- Brainfuck program for testing purposes
test = unlines [
  "+++++ +++++             initialize counter (cell #0) to 10",
  "[                       use loop to set the next four cells to 70/100/30/10",
  "    > +++++ ++              add  7 to cell #1",
  "    > +++++ +++++           add 10 to cell #2 ",
  "    > +++                   add  3 to cell #3",
  "    > +                     add  1 to cell #4",
  "    <<<< -                  decrement counter (cell #0)",
  "]                   ",
  "> ++ .                  print 'H'",
  "> + .                   print 'e'",
  "+++++ ++ .              print 'l'",
  ".                       print 'l'",
  "+++ .                   print 'o'",
  "> ++ .                  print ' '",
  "<< +++++ +++++ +++++ .  print 'W'",
  "> .                     print 'o'",
  "+++ .                   print 'r'",
  "----- - .               print 'l'",
  "----- --- .             print 'd'",
  "> + .                   print '!'",
  "> .                     print '\n'"
  ]

-- Brainfuck command type
data Cmd = Next | Prev | Inc | Dec | Out | In | Start | End deriving Show

-- State of execution
-- (instruction index, data pointer)
type BFState = (Int, Int)

-- Shared environment
-- cmdStream : List of brainfuck commands in program
-- memArray  : Mutable array used for brainfuck memory
data BFRecord = BFRecord {
  cmdStream :: [Cmd],
  memArray :: IOArray Int Char
  }

-- Monad layers
-- First layer  : IO monad for I/O and access to mutable arrays
-- Second layer : State monad storing BFState above
-- Third layer  : Reader monad for storing command list and reference to mutable memory array
type StateIO = StateT BFState IO
type ReaderStateIO = ReaderT BFRecord StateIO

-- Lift us out of the Reader/State monad into IO
io = lift.lift

-- Parse a single character into a BrainFuck command
-- Valid BF commands are wrapped with Just
-- All other characters return Nothing
parse :: Char -> Maybe Cmd
parse c = case c of
  '>' -> Just Next
  '<' -> Just Prev
  '+' -> Just Inc
  '-' -> Just Dec
  '.' -> Just Out
  ',' -> Just In
  '[' -> Just Start
  ']' -> Just End
  otherwise -> Nothing

-- Parse string into brainfuck commands discarding invalid chars
parseCmds :: String -> [Cmd]
parseCmds = mapMaybe parse

-- Use to accumulate bracket nesting depth
-- If we encounter a [, the depth is increased
-- If we encounter a ], decreased
-- Other commands have no effect
depth :: Int -> Cmd -> Int
depth x c = case c of
  Start -> x + 1
  End -> x - 1
  otherwise -> x

-- Return nesting depth as a list of ints
-- Depending on starting command, the initial depth will be +/- 1
-- Scan through instruction list and build list of nesting depths
depths :: [Cmd] -> [Int]
depths (Start:cmds) = scanl depth 1 cmds
depths (End:cmds) = scanl depth (-1) cmds
depths (_:cmds) = error "Should be Start or End"

-- Return matching brace by walking through list of depths until zero is found
findMatching :: [Cmd] -> Int
findMatching cmds = fromJust $ findIndex (==0) (depths cmds)

-- Takes a reference to a mutable array, index into the array and a function f
-- Modifies element at given position by applying function f
transArray ref idx f = do
  val <- io $ readArray ref idx
  io $ writeArray ref idx (f val)

-- Execute a given command
execute :: Cmd -> ReaderStateIO ()

-- Increment data pointer
execute Next = modify $ \(pc, ptr) -> (pc+1, ptr+1)

-- Decrement data pointer
execute Prev = modify $ \(pc, ptr) -> (pc+1, ptr-1)

-- Increment data value
execute Inc = do
  env <- ask
  (pc, ptr) <- get
  transArray (memArray env) ptr (\x -> chr $ ord x + 1)
  put (pc+1, ptr)

-- Decrement data value
execute Dec = do
  env <- ask
  (pc, ptr) <- get
  transArray (memArray env) ptr (\x -> chr $ ord x - 1)
  put (pc+1, ptr)

-- If data pointer is 0 then jump past matching closing bracket
-- Else just move to next instruction
execute Start = do
  env <- ask
  (pc, ptr) <- get
  let match = findMatching (drop pc $ cmdStream env)
  val <- io $ readArray (memArray env) ptr
  modify $ \(pc, ptr) -> if val == (chr 0) then (pc+match+1, ptr) else (pc+1, ptr)

-- If data pointer is non-zero jump to instruction after matching opening bracket
-- Otherwise just move to next instruction
execute End = do
  env <- ask
  (pc, ptr) <- get
  let match = findMatching (reverse $ take (pc +1) $ cmdStream env)
  val <- io $ readArray (memArray env) ptr
  modify $ \(pc, ptr) -> if val /= (chr 0) then (pc-match+1, ptr) else (pc+1, ptr)

-- Print a character
execute Out = do
  env <- ask
  (pc, ptr) <- get
  val <- io $ readArray (memArray env) ptr
  io $ putChar $ val
  put (pc+1, ptr)

execute cmd = error $ "Not implemented: " ++ (show cmd)

-- Execute program until we're at the end
step ::ReaderStateIO ()
step = do
  env <- ask
  (pc, _) <- get
  let cmd = (cmdStream env) !! pc
 -- io $ putStrLn $ (show cmd) ++ (show pc)
  execute cmd
  (pc, _) <- get
  if pc == length (cmdStream env) then
    return ()
    else step

-- Run from input file (if supplied)
-- or test program (defined above) otherwise
main = do
  args <- getArgs
  input <- if length args == 0 then do
    return test
           else do
             s <- readFile $ args!!0
             return s

  mem <- newArray (0, 30000) (chr 0) :: IO (IOArray Int Char)
  runStateT (runReaderT step (BFRecord (parseCmds input) mem)) (0,0)
  return ()