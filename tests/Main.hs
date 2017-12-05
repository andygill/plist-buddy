{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
import           Database.PlistBuddy
import           Control.Monad             (when)
import           Control.Monad.Except

import qualified Data.ByteString           as BS
import           Data.Monoid
import           Data.Text                 (Text, pack, unpack)
import qualified Data.Text                 as T
import           Data.Text.IO              as TIO
import           Data.Time

import           Control.Exception         (bracket, catch, evaluate)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck           hiding (replay)
import           Test.QuickCheck.Exception

import           Control.Concurrent        (threadDelay)
import           System.Directory          (doesFileExist, removeFile)
import qualified System.IO                 as IO
import           System.Mem
import           System.Timeout

import           Data.List                 (lookup, nub, sort, sortBy,
                                            transpose)

import           Control.Monad.Reader
import           GHC.Generics

import           Data.Char                 (isDigit)
import           System.Environment


clearAudit :: IO ()
clearAudit = TIO.writeFile "test.audit" ""

clearDB :: IO ()
clearDB = do
  TIO.writeFile "test.plist" "{}"
  TIO.writeFile "test.audit" ""

rmDB :: IO ()
rmDB = doesFileExist "test.plist" >>= \ b -> when b (removeFile "test.plist")

openConnection :: Bool -> IO Plist
openConnection audit = do
    d <- openPlist "test.plist"
    send d $ clear (Dict [])
    if audit
    then auditOn "test.audit" d
    else return d

closeConnection :: Plist -> IO ()
closeConnection d = send d $ exit


-- only for tests that write then read
withPlistConnection :: Bool -> (Plist -> IO ()) -> IO ()
withPlistConnection audit
                    = --guardPlistBuddyException .
                      bracket (openConnection audit)
                              closeConnection

guardPlistBuddyException :: IO a -> IO a
guardPlistBuddyException m = m `catch` \ (PlistBuddyException msg) -> do
       IO.putStrLn $ "\ndiscarded: " ++ show msg
       discard

main :: IO ()
main = hspec $ do

  beforeAll clearDB $ do
    describe "initial plist" $ modifyMaxSuccess (\ x -> 100) $ do

      it "check initial dict is an dictionary" $ withPlistConnection False $ \ d -> do
        r0 <- send d $ get []
        r0 `shouldBe` Dict []

      it "check reset to array" $ withPlistConnection False $ \ d -> do
        _ <- send d $ clear (Array [])
        r0 <- send d $ get []
        r0 `shouldBe` Array []

      it "check reset to back to an dict" $ withPlistConnection False $ \ d -> do
        _ <- send d $ clear (Array [])
        _ <- send d $ clear (Dict [])
        r0 <- send d $ get []
        r0 `shouldBe` Dict []

      it "check adding a value at top level" $
        property $ \ (Label lbl) (OneValue v) audit -> withPlistConnection audit $ \ d -> do
                debug $ ("add val top",lbl,v)
                _ <- send d $ add [lbl] v
                r0 <- send d $ get []
                r0 `shouldBe` Dict [(lbl,v)]

      it "check adding then setting a value at top level" $
        property $ \ (Label lbl) (PrimValue v1) audit ->
          forAll (arbitrarySameType v1) $ \ v2 ->
            withPlistConnection audit $ \ d -> do
              debug $ ("add then set top",lbl,v1,v2)
              _ <- send d $ add [lbl] v1
              _ <- send d $ set [lbl] v2
              r0 <- send d $ get []
              r0 `shouldBe` Dict [(lbl,v2)]

      it "populate a DB" $
        property $ \ (DictValue v) audit -> withPlistConnection audit $ \ d -> do
          debug $ ("populate",v)
          r0 <- send d $ do
            populateDict v
            get []
          r0 `shouldBe` v

      it "test deeper get" $
        property $ \ (DictValue v) audit ->
          forAll (arbitraryReadPath 0.8 v) $ \ (Path ps,v') -> do
            withPlistConnection audit $ \ d -> do
              send d $ populateDict v
              r0 <- send d $ get ps
              r0 `shouldBe` v'

      it "test deepest get" $
        property $ \ (DictValue v) audit ->
          forAll (arbitraryReadPath 1.0 v) $ \ (Path ps,v') -> do
            withPlistConnection audit $ \ d -> do
              send d $ populateDict v
              r0 <- send d $ get ps
              r0 `shouldBe` v'


      it "test deepest set then get" $
        property $ \ (DictValue v) audit ->
          forAll (arbitraryReadPath 1.0 v) $ \ (Path ps,v1) ->
            not (null ps) && (case v1 of { Dict {} -> False; Array {} -> False ; _-> True}) ==>
            forAll (arbitrarySameType v1) $ \ v2 ->
              withPlistConnection audit $ \ d -> do
                debug (v1,v2,ps)
                send d $ populateDict v
                send d $ set ps v2
                r0 <- send d $ get ps
                r0 `shouldBe` v2

      it "test delete" $
        property $ \ (DictValue v) audit ->
          forAll (arbitraryReadPath 1.0 v) $ \ (Path ps,v1) ->
            not (null ps) ==>
              withPlistConnection audit $ \ d -> do
--                print (v1,ps)
                (r1,parent) <- send d $ do
                  populateDict v
                  r1 <- get ps
                  parent <- get (init ps)
                  delete ps
                  return (r1,parent)
                case parent of
                    Dict {} -> do
                        r2 <- (Just <$> send d (get ps)) `catch` \ (PlistBuddyException _) -> return Nothing
                        (r1,r2) `shouldBe` (v1,Nothing)
                    Array xs -> do
                        xs' <- send d $ do
                          Array xs' <- get (init ps)
                          return xs'
                        (r1,length xs) `shouldBe` (v1,length xs' + 1)

      it "check for bad path error handling" $
        property $ \ (DictValue v) (Path p) audit -> p `notIn` v ==>  withPlistConnection audit $ \ d -> do
          debug $ ("bad path",v,p)
          r <- (send d $ (do
                  populateDict v
                  get p
                  return False)) `catch` \ (PlistBuddyException _) -> do
                    return True

          r `shouldBe` True

      it "test get/set/delete sequences" $
        property $ \ audit ->
          forAll (modSized 8 return) $ \ n ->
          forAll (Blind <$> arbitraryUpdates n (Dict [])) $ \ (Blind updates) ->
              withPlistConnection audit $ \ d -> do
                send d $ clear (Dict [])
                let xs = []
                xs <- sequence [ send d $ do
                            u -- the update
                            r <- get []
                            return (v,r)
                  | (u,v) <- updates
                  ]
                map fst xs `shouldBe` map snd xs

  beforeAll clearDB $ do
    describe "plist modification" $ do
      it "test save of DB" $
        property $ \ (DictValue v) -> guardPlistBuddyException $ do
          d <- openPlist "test.plist"
          send d $ clear (Dict [])  -- clear dict
          send d $ do
            populateDict v
            save
            exit
          d <- openPlist "test.plist"
          r0 <- send d $ get []
          send d $ exit
          r0 `shouldBe` v

      it "test save of DB, with changes in between" $
        property $ \ (DictValue v) (DictValue v') -> guardPlistBuddyException $ do
          d <- openPlist "test.plist"
          send d $ clear (Dict [])  -- clear dict
          send d $ do
            populateDict v
            save
            clear $ Dict []
            populateDict v'
            exit
          d <- openPlist "test.plist"
          r0 <- send d $ get []
          send d $ exit
          r0 `shouldBe` v

      it "test save and revert of DB" $
        property $ \ (DictValue v) (DictValue v') -> guardPlistBuddyException $ do
          d <- openPlist "test.plist"
          send d $ clear (Dict [])  -- clear dict
          r0 <- send d $ do
            populateDict v
            save
            clear $ Dict []
            populateDict v'
            revert
            r <- get []
            exit
            return r
          r0 `shouldBe` v

      it "test double exit" $
        property $ \ (DictValue v) (DictValue v') -> guardPlistBuddyException $ do
          d <- openPlist "test.plist"
          send d $ clear (Dict [])  -- clear dict
          send d $ do
            populateDict v
            save
            exit
          d <- openPlist "test.plist"
          r0 <- send d $ get []
          send d $ exit
          res <- (send d $ do { clear $ Dict [] ; populateDict v' ; return True})
                     `catch` \ (e :: PlistBuddyException) -> do { return False }
          (r0,res) `shouldBe` (v,False)

  beforeAll clearDB $ do
    describe "plist audit test" $ do
      it "test get/set/delete sequences, with basic audit trail usage" $
          forAll (modSized 8 return) $ \ n1 ->
          forAll (modSized 8 return) $ \ n2 ->
          forAll (modSized 8 return) $ \ n3 ->
          let s = Dict [] in
          forAll (Blind <$> arbitraryUpdates n1 (s)                ) $ \ (Blind updates1) ->
          forAll (Blind <$> arbitraryUpdates n2 (lastOf s $ updates1)) $ \ (Blind updates2) ->
          forAll (Blind <$> arbitraryUpdates n3 (lastOf s $ updates1 ++ updates2)) $ \ (Blind updates3) ->
             do d <- openPlist "test.plist"

                -- Populate dictionary randomly
                send d $ clear (Dict [])  -- clear dict
                sequence_ [ send d u | (u,_) <- updates1 ]
                send d $ save
                send d $ exit

                -- Reload, with (new) audit
                clearAudit
                d <- openPlist "test.plist"
                d <- auditOn "test.audit" d
                sequence_ [ send d u | (u,_) <- updates2 ]
                send d $ save

                -- And do more stuff, without saving, but with audit
                sequence_ [ send d u | (u,_) <- updates3 ]
                auditOff d -- turn off audit, before turning exiting the plist
                send d $ exit

                -- now need to restore
                h <- hashcode "test.plist"
--                print ("hashcode",h)
                auditTrails <- recover "test.audit"
--                print ("audit trails",auditTrails)
                let trail = findTrail h auditTrails
                d <- openPlist "test.plist"
                send d $ sequence_ $ map replay trail
                let target = lastOf s $ updates1 ++ updates2 ++ updates3
                r0 <- send d $ get []
                send d $ exit
                r0 `shouldBe` target

  beforeAll rmDB $ do
    describe "create plists" $ do
      it "open non-existent plist" $ do
       property $ do
         d <- openPlist "test.plist"
         r0 <- send d $ get []
         send d $ exit
         r0 `shouldBe` Dict []

  beforeAll clearDB $ do
    describe "auto-save plists" $ modifyMaxSuccess (\ x -> 3) $ do
      it "test bg save of DB, with changes in between" $
        property $ noShrinking $ \ (DictValue v) (DictValue v') -> guardPlistBuddyException $ do
          bg <- backgroundPlist 1 $ openPlist "test.plist"
          bgSend bg $ clear (Dict [])  -- clear dict
          bgSend bg $ do
            populateDict v
            save
            clear $ Dict []
            populateDict v'
          threadDelay $ 2 * 1000 * 1000
          -- the save should have automatically happened
          d <- openPlist "test.plist"
          r0 <- send d $ get []
          send d $ exit
          r0 `shouldBe` v'

      it "auto-restart" $
        property $ noShrinking $ \ (DictValue v) (DictValue v') -> guardPlistBuddyException $ do
          bg <- backgroundPlist 1 $ openPlist "test.plist"
          bgSend bg $ clear (Dict [])  -- clear dict
          bgSend bg $ do
            populateDict v
            save
            clear $ Dict []
            populateDict v'
          threadDelay $ 2 * 1000 * 1000
          -- the save should have automatically happened
          r0 <- bgSend bg $ get []
          bgSend bg $ exit
          r0 `shouldBe` v'

lastOf :: Value -> [(a,Value)] -> Value
lastOf v xs = last (v : map snd xs)

populateDict :: Value -> PlistBuddy ()
populateDict (Dict xs) =
   do sequence_ [ populate (Path $ [i]) v | (i,v) <- xs ]
populateDict _ = error "expecting a Dict"

populate :: Path -> Value -> PlistBuddy ()
populate (Path ps) val =
  case val of
    Dict xs -> do
      add ps (Dict [])
      sequence_ [ populate (Path $ ps ++ [i]) v | (i,v) <- xs ]
    Array vs -> do
      add ps (Array [])
      sequence_ [ populate (Path $ ps ++ [pack (show i)]) v | (i,v) <- [0..] `zip` vs ]
    _ -> do
      add ps val


[] `notIn` _        = False
(p:ps) `notIn` (Dict xs) = case lookup p xs of
                        Nothing -> True
                        Just v -> ps `notIn` v
(p:ps) `notIn` (Array vs)
                     | T.all isDigit p && not (T.null p)
                     = case drop (read (unpack p)) vs of
                         [] -> False
                         (v:_) -> ps `notIn` v
_ `notIn` _          = True


arbitraryUpdates :: Int -> Value -> Gen [(PlistBuddy (),Value)]
arbitraryUpdates 0 v = return []
arbitraryUpdates n v = do
  (u,v') <- arbitraryUpdate v
  rest <- arbitraryUpdates (n-1) v'
  return $ (u,v') : rest

arbitraryUpdate :: Value -> Gen (PlistBuddy (),Value)
arbitraryUpdate v = frequency
  [ (200, arbitraryAdd v)
  , (200, arbitrarySet v)
  , (100, arbitraryDelete v)
  , (1, return (clear $ Dict [],Dict []))  -- Infrequently!
  ]

-- for now, does not go deep
-- TODO: make this go deeper (randomly)
arbitraryAdd :: Value -> Gen (PlistBuddy (),Value)
arbitraryAdd = addMe  []
  where
    addMe p (Dict xs) = do
      Label lbl <- arbitrary
      OneValue val <- arbitrary
      if (lbl `elem` map fst xs)
      then addMe p (Dict xs)  -- try again
      else return (add (p ++ [lbl]) val,Dict $ xs ++ [(lbl,val)])
    addMe p (Array vs) = do
      let lbl = T.pack (show $ length vs) -- for now, append at end
      OneValue val <- arbitrary
      return (add (p ++ [lbl]) val,Array $ vs ++ [val])
    addMe p other = return (return (), other)

arbitrarySet :: Value -> Gen (PlistBuddy (),Value)
arbitrarySet = setMe []
  where
    setMe p (Dict []) = return (return (), Dict [])
    setMe p (Dict xs) = do
      Label lbl <- Label <$> elements (map fst xs)
      case lookup lbl xs of
        Just v -> do
          if valueType v `elem` ["array","dict"]
          then return (return (), Dict xs)
          else do
              v' <- arbitrarySameType v
              return ( set (p ++ [lbl]) v'
                     , Dict [ if l == lbl then (l,v') else (l,x) | (l,x) <-  xs ]
                     )
        Nothing -> error $ "should never happen" ++ show (lbl,map fst xs,xs)
-- for now
--    setMe p (Array vs) =
    setMe p other = return (return (), other)

arbitraryDelete :: Value -> Gen (PlistBuddy (),Value)
arbitraryDelete = delMe []
  where
    delMe p (Dict []) = return (return (), Dict [])
    delMe p (Dict xs) = do
      Label lbl <- Label <$> elements (map fst xs)
      return ( delete (p ++ [lbl])
             , Dict [ (l,x) | (l,x) <-  xs, l /= lbl ]
             )
    delMe p other = return (return (), other)

{-
    addMe p (Array vs) = do
      let lbl = T.pack (show $ length vs) -- for now, append at end
      OneValue val <- arbitrary
      vs <- sequence $
          [ return [ addMe (p ++ [T.pack $ show $ i]) v | (v,i) <- xs `zip` [(0::Int)..] ]
      return $ concat xs
    addMe p (Dict xs) = do
      Label lbl <- arbitrary
      OneValue val <- arbitrary
      xs <- sequence $
          [ return [ (add (p ++ [lbl]) val, Dict (xs ++ [(lbl,val)]))]
          |  not (lbl `elem` map fst xs) ] ++
          [ addMe (p ++ [l]) v | (l,v) <- xs ]
      return $ concat xs
    addMe p other = return []
-}

{-
arbitraryAdd :: Value -> Gen [(PlistBuddy (),Value)]
addInValue = addMe []
  where
    addMe p (Array vs) = do
      let lbl = T.pack (show $ length vs) -- for now, append at end
      OneValue val <- arbitrary
      vs <- sequence $
          [ return [ addMe (p ++ [T.pack $ show $ i]) v | (v,i) <- xs `zip` [(0::Int)..] ]
      return $ concat xs

    addMe p (Dict xs) = do
      Label lbl <- arbitrary
      OneValue val <- arbitrary
      xs <- sequence $
          [ return [ (add (p ++ [lbl]) val, Dict (xs ++ [(lbl,val)]))]
          |  not (lbl `elem` map fst xs) ] ++
          [ addMe (p ++ [l]) v | (l,v) <- xs ]
      return $ concat xs
    addMe p other = return []
-}
{-
-- Set of commands that change the value somehow
deleteInValue :: Value -> Gen [PlistBuddy ()]
deleteInValue = del []
 where
   del (Dict xs)
   del (Array vs)  = [ n <- [0..(length n - 1)]
   del p other     = delete p
-}

{-
    -- TO ADD
        -- try get type error

        r <- send d $ ((set ["I1"] (String "foo")>>return "no failed") `catchPlistError` \ msg -> return msg)
        check "check for type error" r $ "set failed: \"Unrecognized Integer Format\""

        _ <- send d $ exit

        d <- openPlist "test.plist"

        now <- getCurrentTime

	send d $ add ["S5"] (Date now)

        Date r0 <- send d $ get ["S5"]

        check "check for date storage" (abs (diffUTCTime now r0) < 1) $ True
        _ <- send d $ exit

        d <- debugOn <$> openPlist "test.plist"
-}


check :: (Eq a, Show a) => Text -> a -> a -> IO ()
check msg t1 t2 = if t1 /= t2 then fail ("check failed: " ++ show (msg,t1,t2)) else TIO.putStrLn msg

arbitraryValue :: Int -> Gen Value
arbitraryValue n = frequency
  [(7,(\ (PrimValue v) -> v) <$> arbitrary),
   (2,mysized $ \ n' -> mkDict <$> sequence [ arbitraryDict (n-1) | _ <- [1..n]]),
   (1,mysized $ \ n' -> Array <$> sequence [ arbitraryValue (n-1) | _ <- [1..n]])
  ]
  where mysized k | n == 0    = k 0
                  | otherwise = modSized 8 k


-- removes dup labels
mkDict :: [(Text,Value)] -> Value
mkDict xs = Dict [ (lbl,v) | (lbl,v) <- nub (map fst xs) `zip` map snd xs ]

arbitraryDict :: Int -> Gen (Text,Value)
arbitraryDict n = do
  Label nm <- arbitrary
  v <- arbitraryValue n
  return (nm,v)

arbitraryDate :: Gen UTCTime
arbitraryDate =
  UTCTime <$> ((\ d -> addDays d (fromGregorian 1950 1 1)) -- 2003 10 25
                  <$> choose (0, 85 * 365)  -- dates after 2038 have issues (wordsize?)
                                -- +2
              )
          <*> (fromInteger <$> choose (0,60 * 60 * 24 - 1))

arbitraryText :: Gen Text
arbitraryText = modSized 10 $ \ n -> pack <$> (vectorOf n $ elements ('\n':[' '..'~']))

-- 28
arbitraryData :: Gen BS.ByteString
arbitraryData = modSized 32 $ \ n -> BS.pack <$> (vectorOf n $ elements ([0..255]))

instance Eq Value where
  (==) = eqValue

eqValue :: Value -> Value -> Bool
eqValue (String s1) (String s2) = s1 == s2
eqValue (Array a1)  (Array a2)  = a1 == a2
eqValue (Dict d1)   (Dict d2)   = sortBy f d1 == sortBy f d2  -- order should not matter
  where f (a,_) (b,_) = a `compare` b
eqValue (Bool a1)    (Bool a2)   = a1 == a2
eqValue (Real a1)    (Real a2)   = abs (a1 - a2) <= abs ((a1 + a2) / 1e6)
eqValue (Integer a1) (Integer a2) = a1 == a2
eqValue (Date d1)    (Date d2)    = d1 == d2
eqValue (Data d1)    (Data d2)    = d1 == d2
eqValue _ _ = False

---------------------------------------

---------------------------------------
valueShrink :: Value -> [Value]
valueShrink (Dict []) = []
valueShrink (Dict [(lbl,x)]) = [x]
valueShrink (Dict xs) =
    [ Dict (take i xs ++ drop (i + 1) xs)
    | i <- [0..length xs - 1]
    ] ++
    [ Dict (map fst xs `zip` vs)
    | vs <- transpose $ fmap valueShrink (map snd xs)
    ]
valueShrink (Array []) = []
valueShrink (Array [x]) = [x]
valueShrink (Array vs) =
    [ Array (take i vs ++ drop (i + 1) vs)
    | i <- [0..length vs - 1]
    ] ++
    [ Array vs
    | vs <- transpose $ map valueShrink vs
    ]
--valueShrink (Date d) = [Date $ addUTCTime (60*60) d,Date $ addUTCTime (60) d,Date $ addUTCTime (1) d]

valueShrink other = []


---------------------------------------

arbitrarySameType :: Value -> Gen Value
arbitrarySameType v0 = do
  (OneValue v) <- arbitrary
  if valueType v0 == valueType v
  then return v
  else arbitrarySameType v0

newtype PrimValue = PrimValue Value -- any primitive
  deriving (Show,Generic)

instance Arbitrary PrimValue where
  arbitrary = PrimValue <$> oneof
    [ Integer <$> arbitrary
    , String  <$> arbitraryText
    , Bool    <$> arbitrary
    , Real    <$> arbitrary
    , Date    <$> arbitraryDate -- for now
    , Data    <$> arbitraryData
    ]
  shrink (PrimValue v) = [ PrimValue v' | v' <- valueShrink v, valueType v == valueType v']

newtype OneValue = OneValue Value -- primitive + empty dict or empty array
  deriving (Show,Generic)

instance Arbitrary OneValue where
  arbitrary = OneValue <$> arbitraryValue 0
  shrink (OneValue v) = [ OneValue v' | v' <- valueShrink v, valueType v == valueType v']

newtype DeepValue = DeepValue Value -- any value, to any depth
  deriving (Show,Generic)

instance Arbitrary DeepValue where
  arbitrary = DeepValue <$> modSized 8 arbitraryValue
  shrink (DeepValue v) = [ DeepValue v' | v' <- valueShrink v, valueType v == valueType v']

newtype DictValue = DictValue Value -- any value, to any depth
  deriving (Show,Generic)

instance Arbitrary DictValue where
  arbitrary = (DictValue . mkDict) <$> (modSized 8 $ \ n -> vectorOf n (modSized 8 arbitraryDict))
  shrink (DictValue v) = [ DictValue v' | v' <- valueShrink v, valueType v == valueType v']

newtype Label = Label Text
  deriving (Show,Generic)

instance Arbitrary Label where
  arbitrary = modSized 32 $ \ n -> (Label . pack) <$> sequence
                [ elements (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])
                | _ <- [0..n]
                ]

newtype Path = Path [Text] -- non-empty
  deriving (Show,Generic)

instance Arbitrary Path where
  arbitrary = Path <$> (modSized 8 $ \ n -> vectorOf (n+1) ((\ (Label t) -> t) <$> arbitrary))

modSized :: Int -> (Int -> Gen a) -> Gen a
modSized n k = choose (0,n-1) >>= k

newtype ReadPath = ReadPath [Text] -- can be empty, must be valid
  deriving (Show,Generic)

instance Arbitrary ReadPath where
  arbitrary = ReadPath <$> (modSized 8 $ \ n -> vectorOf n ((\ (Label t) -> t) <$> arbitrary))

arbitraryReadPath :: Double -> Value -> Gen (Path,Value)
arbitraryReadPath n v@(Dict xs) = do
    stop <- choose (0,1)
    if (stop > n) || null xs
    then return (Path [],v)
    else do nm <- elements (map fst xs)
            case lookup nm xs of
              Just v' -> do
                (Path ps,vr) <- arbitraryReadPath n v'
                return (Path (nm:ps),vr)
              Nothing -> error "arbitraryReadPath internal error"
arbitraryReadPath n v@(Array vs) = do
    stop <- choose (0,1)
    if (stop > n) || null vs
    then return (Path [],v)
    else do i <- elements [0..(length vs - 1)]
            (Path ps,vr) <- arbitraryReadPath n (vs !! i)
            return (Path (pack (show i):ps),vr)
arbitraryReadPath _ v = return (Path [],v)


compareValue :: Path -> Value -> Value -> String
compareValue (Path ps) v1 v2 | valueType v1 /= valueType v2 = "different types : " ++ show (ps,v1,v2)
compareValue (Path ps) (Dict ds1) (Dict ds2)
  | nm1 /= nm2 = "different names of fields in dict : " ++ show (ps,nm1,nm2)
  | otherwise = concat [ case (lookup nm ds1,lookup nm ds2) of
                          (Just v1,Just v2) -> compareValue (Path (ps ++ [nm])) v1 v2
                          _ -> "internal error in dict compare " ++ show ps
                       | nm <- nm1 ]
 where
   nm1 = sort (nub (map fst ds1))
   nm2 = sort (nub (map fst ds2))

compareValue (Path ps) (Array ds1) (Array ds2)
  | length ds1 /= length ds2 = "different lengths of array : " ++ show (ps,length ds1,length ds2)
  | otherwise = concat [ compareValue (Path (ps ++ [pack (show i)])) d1 d2 | (i,d1,d2) <- zip3 [0..] ds1 ds2 ]
compareValue (Path ps) v1 v2
  | v1 /= v2 = "different values : " ++ show (ps,v1,v2)
  | otherwise = ""


debug :: Show a => a -> IO ()
debug = const $ return ()
--debug = print


-- TODO: make all labels length 1, and test for testing
