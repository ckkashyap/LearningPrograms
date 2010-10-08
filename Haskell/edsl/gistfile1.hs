{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

import Control.Applicative (Applicative,(<*>),(<$>),pure)
import Control.Monad (ap,join)
import "mtl" Control.Monad.Trans (liftIO)
import "mtl" Control.Monad.Reader (ReaderT,runReaderT,ask)
import System (system)

data MakeInfo = MakeInfo
    { target_  :: String
    , sources_ :: [String]
    }
  deriving(Show)

type MakeMonad = ReaderT MakeInfo IO

instance Applicative MakeMonad where
  pure = return
  (<*>)  = ap

runMake :: MakeMonad () -> MakeInfo -> IO ()
runMake fn makeInfo = runReaderT fn makeInfo

class Combinable a b where
  combine :: a -> b -> MakeMonad String

instance (Show a) => Combinable String a where
  a `combine` b = return (a ++ ' ':(show b))

instance Combinable String [String] where
  a `combine` b = return $ foldl (++) a $ map (' ':) b

instance (Combinable a b) => Combinable (MakeMonad a) b where
  ma `combine` b = ma >>= (`combine` b)

instance (Combinable a b) => Combinable a (MakeMonad b) where
  a `combine` mb = mb >>= combine a

instance (Combinable a b) => Combinable (MakeMonad a) (MakeMonad b) where
  ma `combine` mb = join $ combine <$> ma <*> mb

infixl &
(&) :: (Combinable a b) => a -> b -> MakeMonad String
(&) = combine

sh :: MakeMonad String -> MakeMonad ()
sh m = do 
    cmd <- m
    liftIO $ system cmd
    return ()

target :: MakeMonad String
target = target_ <$> ask 

sources :: MakeMonad [String]
sources = sources_ <$> ask

-- simple test printing some values using the system "echo" command via your
-- shell
test = do
    sh $ "echo" & "just a test from echo shell command:" & target & sources
    sh $ "echo" & "another line: " & ["abc", "def"] & (1 :: Int)

testInfo = MakeInfo "mytarget" ["source1", "source2"]

runTest m = runMake m testInfo
