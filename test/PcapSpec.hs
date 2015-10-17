{--
For some the context on creating the tests see the example at:

https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md

To run the tests, do
    cabal configure --enable-tests
    cabal test
--}

import System.IO.Error
import Test.Hspec

import Network.Pcap

spec :: Spec
spec = do
  describe "openOffline" $ do
    it "works on empty pcap file" $ do
        (openOffline empty_pcap >> return True) `shouldSatisfyM` id

  describe "statistics" $ do

    it "succeeds on online streams" $ do
        (ifSucceeds (openLive "any" 64 False 0) statistics)
            `satisfiesIfSucceeds` sane_statistics

    it "throws when being gathered from offline streams" $ do
        (openOffline empty_pcap >>= statistics) `shouldThrow` anyIOException

    it "throws when being gathered from dead streams" $ do
        (openDead DLT_NULL 64 >>= statistics) `shouldThrow` anyIOException


  where
    empty_pcap = "test/empty.pcap"
    shouldSatisfyM m cond = m >>= (`shouldSatisfy` cond)
    -- If the condition does not throw, check the Maybe value
    satisfiesIfSucceeds cond check = do
        maybe_v <- cond
        case maybe_v of
            Nothing -> do
                putStrLn ("  (Use sudo to properly check "
                            ++ "the following single condition:)")
                maybe_v `shouldBe` Nothing
            Just value -> (value `shouldSatisfy` check)
    -- Only test the second argument if the first computation does not throw
    ifSucceeds :: IO a -> (a -> IO b) -> IO (Maybe b)
    ifSucceeds comp1 comp2 = do
        either_err_f <- tryIOError comp1
        case either_err_f of
            Left _ -> return Nothing
            Right v -> fmap Just (comp2 v)

-- Checks that the statistics values are within a sane range, which is
-- relatively small for each constituent part of the Statistics type.
-- Otherwise it might just be some interpreted memory garbage.
sane_statistics :: Statistics -> Bool
sane_statistics (Statistics v1 v2 v3) =
    (v1 + v2 + v3) < 1000000 && v1 >= 0 && v2 >= 0 && v3 >= 0

main :: IO ()
main = hspec spec
