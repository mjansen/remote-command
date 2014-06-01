module OutputParsers
  ( MachineInfo(..)
  , MachineName
  , parseMachineInfo
  ) where

import Prelude hiding (takeWhile)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import Data.Attoparsec.ByteString.Char8

type MachineName = ByteString

data MachineInfo = MachineInfo
  { mi_name        :: MachineName
  , mi_memory      :: Integer
  , mi_ncpus       :: Int
  , mi_productName :: ByteString
  } deriving (Eq, Ord, Show, Read)

machineInfoParser :: MachineName -> Parser MachineInfo
machineInfoParser mName = do
  string "MemTotal:"
  skipSpace
  mem <- decimal
  string " kB\n"
  ncpus <- decimal
  char '\n'
  productName <- takeWhile (/= '\n')
  char '\n'
  return $ MachineInfo mName (mem * 1024) ncpus productName

parseMachineInfo :: MachineName -> ByteString -> Either String MachineInfo
parseMachineInfo mName = parseOnly (machineInfoParser mName)
