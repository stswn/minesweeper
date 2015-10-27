module App.Params ( Params (..)
                  , BoardType (..)
                  , getParams
                  ) where

import Options.Applicative

data Params = Params { boardType :: BoardType
                     , useAscii :: Bool
                     }

data BoardType = Beginner
               | Intermediate
               | Expert
               | Custom Int Int Int

params :: Parser Params
params = Params
    <$> subparser ( command "beginner" (info (pure Beginner)
                      (progDesc "Beginner board - 9x9, 10 mines"))
                 <> command "intermediate" (info (pure Intermediate)
                      (progDesc "Intermediate board - 16x16, 40 mines"))
                 <> command "expert" (info (pure Expert)
                      (progDesc "Expert board - 30x16, 99 mines"))
                 <> command "custom" (info (Custom
                      <$> argument auto (metavar "WIDTH")
                      <*> argument auto (metavar "HEIGHT")
                      <*> argument auto (metavar "MINES")
                        ) (progDesc "Custom board"))
        )
    <*> switch ( long "use-ascii"
              <> help "Use ASCII symbols for marked and exploded fields" )

getParams :: IO Params
getParams = execParser $ info (helper <*> params) (header "Minesweeper")
