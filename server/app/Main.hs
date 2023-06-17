module Main
    ( main
    ) where

import           CLI                 (Mode (..), userInputLoop)
import           Control.Monad.State (evalStateT)
import           WarmDrinkF          (ComponentList (..))

import           Routes              (routes)
import           Web.Scotty          (scotty)

--- Main function (entry point) ---

-- main :: IO ()
-- main = do
--     putStrLn "\ESC[1;34mWelcome to ScaldishDrink!\ESC[0m"
--     putStrLn "Type 'help' for a list of commands"
--     evalStateT (userInputLoop Normal) (ComponentList [] [])
--     putStrLn "Goodbye"

main :: IO ()
main = scotty 8000 routes
