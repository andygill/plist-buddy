{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Database.PlistBuddy.Types
        ( -- * Remote Monad
          PlistBuddy(..)
        , PlistPrimitive(..)
        , help
        , exit
        , save
        , revert
        , clear
        , get
        , set
        , add
        , delete
  --      , throwPlistError
  --      , catchPlistError
        -- * Remote monad handle
        , Plist(..)
        -- * Other types
        , Value(..)
         -- * Exception
        , PlistBuddyException(..)
        , PlistError(..)
        -- * Audit
        , Audit(..)
        , Trail(..)
        , AuditTrail(..)
        ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Remote.Monad (KnownResult (..), RemoteMonad(..), primitive)


import           Data.ByteString      (ByteString)
import           Data.String          (IsString(..))
import           Data.IORef
import           Data.Text            (Text)

import           System.Posix.Pty
import           System.Process

import           Data.Time

import           GHC.Generics

------------------------------------------------------------------------------

-- | The Remote Monad
type PlistBuddy a = RemoteMonad PlistPrimitive a

newtype PlistError = PlistError String
 deriving (Show, Eq)

instance IsString PlistError where
  fromString = PlistError

-- | A version of 'catchError' with the type specialized to 'PlistBuddy'. Using
--   this will cause a static error if used on a non-'PlistBuddy' monad.
{-
catchPlistError :: PlistBuddy a -> (PlistError -> PlistBuddy a) -> PlistBuddy a
catchPlistError = catchError

-- | Throw a 'PlistError'. Uncaught 'PlistError' exceptions will
--   be thrown by 'send' as IO Exceptions.

throwPlistError :: PlistError -> PlistBuddy a
throwPlistError = throwError
-}
-- | The Remote Plist
data Plist = Plist
  { plist_pty     :: Pty
  , plist_lock    :: MVar ()
  , plist_proc    :: ProcessHandle
  , plist_debug   :: Bool
  , plist_file    :: FilePath
  , plist_trail   :: Trail -> IO ()      -- audit information
  , plist_launder :: IO ()             -- close audit without issuing an exit; for testing
  , plist_dirty   :: IORef (Maybe Bool) -- if the database has been changed; Nothing == closed
  }

------------------------------------------------------------------------------

data Value  = String Text
            | Array [Value]
            | Dict [(Text,Value)]
            | Bool Bool
            | Real Double
            | Integer Integer
            | Date UTCTime
            | Data ByteString
        deriving (Show, Read, Generic)

------------------------------------------------------------------------------

-- | 'PlistBuddyException' is for fatal things,
--  like the sub-process blocks, for some reason.
newtype PlistBuddyException = PlistBuddyException String
    deriving (Show, Generic)

instance Exception PlistBuddyException

------------------------------------------------------------------------------

data AuditTrail = AuditTrail ByteString [Trail] (Maybe ByteString)
   deriving (Show,Read,Generic)

data Audit
 = UTCTime :! Trail
   deriving (Show,Read,Generic)

data Trail
  = SaveTrail ByteString       -- ^ hash code of saved file
  | RevertTrail
  | ExitTrail
  | ClearTrail Value
  | SetTrail [Text] Value
  | AddTrail [Text] Value
  | DeleteTrail [Text]
  | StartTrail ByteString      -- ^ hash code at start of audit capture
    deriving (Show,Read,Generic)


data PlistPrimitive a where
  Help   :: PlistPrimitive Text
  Exit   :: PlistPrimitive ()
  Save   :: PlistPrimitive ()
  Revert :: PlistPrimitive ()
  Clear  :: Value -> PlistPrimitive ()
  Get    :: [Text] -> PlistPrimitive Value
  Set    :: [Text] -> Value -> PlistPrimitive ()
  Add    :: [Text] -> Value -> PlistPrimitive ()
  Delete :: [Text] -> PlistPrimitive ()
  ImportData :: [Text] -> ByteString -> Trail -> PlistPrimitive ()
  MergeDate :: [Text] -> UTCTime -> Trail -> PlistPrimitive ()


instance KnownResult PlistPrimitive where
  knownResult x = case x of
                    Help          -> Nothing
                    Get {}        -> Nothing
                    Exit          -> Just ()
                    Save          -> Just ()
                    Revert        -> Just ()
                    Clear{}       -> Just ()
                    Set {}        -> Just ()
                    Add {}        -> Just ()
                    Delete {}     -> Just ()
                    ImportData {} -> Just ()
                    MergeDate {}  -> Just ()







help :: PlistBuddy Text
help = primitive Help

exit   :: PlistBuddy ()
exit = primitive Exit

save   :: PlistBuddy ()
save = primitive Save

revert :: PlistBuddy ()
revert = primitive Revert

clear  :: Value -> PlistBuddy ()
clear v = primitive $ Clear v

get    :: [Text] -> PlistBuddy Value
get entry = primitive $ Get entry

set    :: [Text] -> Value -> PlistBuddy ()
set entry val= primitive $ Set entry val

add    :: [Text] -> Value -> PlistBuddy ()
add entry val = primitive $ Add entry val

delete :: [Text] -> PlistBuddy ()
delete entry = primitive $ Delete entry

importData :: [Text] -> ByteString -> Trail -> PlistBuddy ()
importData entry d trail  = primitive $ ImportData entry d trail

mergeDate :: [Text] -> UTCTime -> Trail -> PlistBuddy ()
mergeDate entry date trail = primitive $ MergeDate entry date trail
