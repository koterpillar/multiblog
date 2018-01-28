{-|
Default option, suggested in
https://github.com/Gabriel439/Haskell-Optparse-Generic-Library/issues/30
-}
module Options.Generic.Default
    ( DefaultArg(..)
    , getRecordDefault
    , getRecordPureDefault
    ) where

import Control.Applicative

import Options.Generic

data DefaultArg a
    = JustArg a
    | DefaultArg

instance ParseRecord a => ParseRecord (DefaultArg a) where
    parseRecord =
        fmap JustArg parseRecord <|> fmap (\() -> DefaultArg) parseRecord

getRecordDefault :: ParseRecord a => a -> Text -> IO a
getRecordDefault def msg = fmap (defaultArg def) (getRecord msg)

getRecordPureDefault :: ParseRecord a => a -> [Text] -> Maybe a
getRecordPureDefault def args = fmap (defaultArg def) (getRecordPure args)

defaultArg :: a -> DefaultArg a -> a
defaultArg _ (JustArg a) = a
defaultArg def DefaultArg = def
