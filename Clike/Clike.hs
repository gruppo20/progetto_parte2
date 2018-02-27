import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Data.Map as M
import AbsClike
import LexClike
import ParClike
import ErrM
import PrintClike

import TypeChecker


-- driver

check :: String -> IO () 
check s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "\n\n--------------------------------\n\n"
                           putStrLn "SYNTAX ERROR"
                           putStrLn err
                           putStrLn "\n\n--------------------------------\n\n"
                           exitFailure 
            Ok  tree -> do 
                        putStrLn "\n\n--------------------------------\n\n"
                        putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
                        putStrLn $ "\n\n[Linearized Tree]\n\n" ++ PrintClike.printTree tree
                        putStrLn $ "\n\n[Typecheck]\n" 
                        case typecheck tree of
                          Bad err -> do putStrLn err
                                        --exitFailure 
                          Ok env   -> do putStrLn $ "Types are ok.\n\n"
                        putStrLn "\n\n--------------------------------\n\n"



main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: <SourceFile>"
                         exitFailure

