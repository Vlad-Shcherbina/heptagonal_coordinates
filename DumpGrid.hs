import Control.Monad
import Control.Monad.Identity
import Control.Monad.Loops
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, sortBy)
import System.IO
import System.Environment
import Data.Function
import Data.String

import Heptagrid


type IndexState = Map.Map Heptagon Int


try_add :: Heptagon -> State IndexState Bool
try_add h = do
    s <- get
    case Map.lookup h s of
        Nothing -> do
            put $ Map.insert h (Map.size s) s
            return True
        Just index ->
            return False


expand_frontier_element :: Heptagon -> State IndexState [Heptagon]
expand_frontier_element h =
    filterM try_add $ adjacent h


dfa_step :: [Heptagon] -> State IndexState [Heptagon]
dfa_step frontier = do
    liftM concat $ mapM (expand_frontier_element) frontier


monad_iterateN :: (Monad m) => Int -> (a -> m a) -> a -> m a
monad_iterateN n f = foldl (<=<) return $ replicate n f


dump_adjacent :: Map.Map Heptagon Int -> IO ()
dump_adjacent h_map = do
    let show_h h = do
        let xs = [Map.findWithDefault (-1) hh h_map | hh <- h : adjacent h]
        putStrLn $ intercalate " " $ map show xs
    sequence $ map show_h $ sortBy (compare `on` (h_map Map.!)) $ Map.keys h_map
    return ()


main = do
    args <- getArgs
    let num_steps :: Int
        num_steps = if args == [] then 1 else read (args!!0)

    let start_state = Map.singleton origin 0
    let start_frontier = [origin]

    let dfa_steps = monad_iterateN num_steps dfa_step
    let (frontier, state) = runState ((dfa_steps) start_frontier) start_state

    dump_adjacent state

    hPutStr stderr "frontier size: "
    hPrint stderr $ length frontier
    hPutStr stderr "     map size: "
    hPrint stderr $ Map.size state
