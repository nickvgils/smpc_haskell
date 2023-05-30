module Parser where


import Options.Applicative


options :: IO Sample
options = execParser opts
    where
      opts = info (sample <**> helper)
        ( fullDesc
        <> progDesc "Print a greeting for TRGET"
        <> header "hello - a test for optparse-applicative" )

data Sample = Sample
  { enthusiasm :: Int,
     index :: Int }

sample :: Parser Sample
sample = Sample
      <$> option auto
          ( long "m"
         <> help "use m local parties (and run all m, if i is not set)"
         <> showDefault
         <> value 1
         <> metavar "INT" )
      <*> option auto
          ( long "i"
         <> help "set index of this local party to i, 0<=i<m"
         <> showDefault
         <> value 0
         <> metavar "INT" )