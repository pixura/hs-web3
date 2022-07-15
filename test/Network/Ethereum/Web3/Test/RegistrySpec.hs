{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Network.Ethereum.Web3.Test.RegistrySpec where

import           Data.Default                 (def)
import           Test.Hspec                   (Spec)

import           Network.Ethereum.Api.Types   (Filter)
import           Network.Ethereum.Contract.TH (abiFrom)
import           Network.Ethereum.Web3        (EventAction (TerminateEvent),
                                               Web3, event)

[abiFrom|test/contracts/Registry.json|]

-- this spec is just to test compilation
spec :: Spec
spec = return ()

monitor :: Web3 ()
monitor = do
  let fltr1 = def :: Filter A
      fltr2 = def :: Filter B
  event fltr1 $ \_ -> pure TerminateEvent
  event fltr2 $ \_ -> pure TerminateEvent
  pure ()
