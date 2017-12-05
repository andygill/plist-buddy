{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Database.PlistBuddy.Audit ( auditOn, auditOff, hashcode, recover, findTrail ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base16    as B16
import qualified Data.ByteString.Lazy      as LB
import           Data.Char                 (isSpace)
import           Data.Text                 (Text)
import           Database.PlistBuddy.Types

import           System.IO
import           System.Process

import           Data.Time

import           GHC.Generics

import qualified Crypto.Hash.MD5           as MD5

import           Debug.Trace

auditOn :: FilePath -> Plist -> IO Plist
auditOn auditFile plist = do
  -- if there is no file, then this creates an empty file first
  au <- openFile auditFile AppendMode
  -- state what we are auditing, with blank line(s)
  -- in case the previous line was incomplete
  hPutStr au $ "\n\n" ++ take 72 (cycle "-") ++ "\n"
  t <- getCurrentTime
  h <- hashcode (plist_file plist)
  issue au $ t :! StartTrail h
  -- and append to the audit file
  let up u = do
        o <- hIsOpen au
        if o then do
          t <- getCurrentTime
          issue au $ t :! u
          case u of
            ExitTrail -> hClose au
            _    -> return ()
        else return () -- putStrLn $ "audit log failure: " ++ show u ++ "\n"
  return $ plist { plist_trail = up, plist_launder = hClose au }

-- | Turn off audit.
auditOff :: Plist -> IO ()
auditOff = plist_launder

hashcode :: FilePath -> IO ByteString
hashcode fileName = do
  bs <- LB.readFile fileName
  return $! B16.encode $! MD5.hashlazy bs

issue :: Show a => Handle -> a -> IO ()
issue h u = do
  hPutStr h $ show u ++ "\n"
  hFlush h

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                  [(r,rest)] | all isSpace rest -> return r
                  _ -> Nothing

-- | Find the list of 'PlistBuddy' commands to recover the plist.
--   Be careful when running recover with the audit capability turned on; it can duplicate
--   the audit trail, because recovery is also write. (This should not break anything)
recover :: FilePath -> IO [AuditTrail]
recover auditFile = do
  txt <- readFile auditFile
  let trails  = [ v | Just (_ :! v) <- map maybeRead $ lines $ txt ]
  return $ runTrails trails

runTrails :: [Trail] -> [AuditTrail]
runTrails [] = []
runTrails (inst :rest) = case inst of
  SaveTrail bs  -> runTrails' bs [] rest
  StartTrail bs -> runTrails' bs [] rest
  _        -> runTrails rest            -- find a 'Save/Start' checkpoint
  where
    runTrails' :: ByteString -> RList Trail -> [Trail] -> [AuditTrail]
    runTrails' bs [] [] = []
    runTrails' bs insts [] = [AuditTrail bs (reverse insts) Nothing]
    runTrails' bs insts (inst : rest) = case inst of
          SaveTrail bs'  -> mkTrail (Just bs') $ runTrails' bs' [] rest -- save is start of next trail
          StartTrail bs' -> mkTrail Nothing    $ runTrails' bs' [] rest -- start ignores trail before, because of no save
          RevertTrail    -> runTrails' bs [] rest                -- revert wipes all unsaved instructions
          ExitTrail      -> runTrails rest                       -- abandon the changes; start trail again
          ClearTrail _   -> runTrails' bs [inst] rest            -- anything *before* clear is now lost
          _         -> runTrails' bs (inst : insts) rest    -- Set / Add / Delete
     where
          mkTrail done k =
                  if null insts
                  then k
                  else (AuditTrail bs (reverse insts) done) : k

type RList a = [a] -- a reversed list, often a stack

-- Find the last trail with this hashcode
findTrail :: ByteString -> [AuditTrail] -> [Trail]
findTrail bs trails = combine $ dropMe trails
  where
    combine [] = []
    combine ( AuditTrail bs ts (Just bs')
            : AuditTrail bs'' ts2 done
            : more) | bs' == bs'' =
      combine (AuditTrail bs (ts ++ ts2) done : more)
    combine (AuditTrail bs ts _ : _) = ts

    takeMe [] acc = reverse acc
    takeMe (x@(AuditTrail bs' _ _) : xs) acc
     | bs == bs' = takeMe xs [x] --- restart
     | otherwise = takeMe xs (x : acc)

    dropMe [] = []
    dropMe (x@(AuditTrail bs' _ _) : xs)
     | bs == bs' = takeMe xs [x]
     | otherwise = dropMe xs
