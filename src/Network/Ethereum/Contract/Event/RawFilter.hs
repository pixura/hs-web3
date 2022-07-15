{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      :  Network.Ethereum.Contract.Event.RawFilter
-- Copyright   :  Charles Crain 2019
-- License     :  BSD3
--
-- Maintainer  :  craincharles@gmail.com
-- Stability   :  experimental
-- Portability :  unportable
--
-- Raw contract event filter support.
--

module Network.Ethereum.Contract.Event.RawFilter
  ( rawFilter
  , FilterMiddleware(..)
  )
where

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Async               (Async)
import           Control.Monad                          (forM, void, when)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Reader             (ReaderT (..))
import           Data.Machine                           (MachineT, asParts,
                                                         autoM, await,
                                                         construct, final,
                                                         repeatedly, runT,
                                                         unfoldPlan, (~>))
import           Data.Machine.Plan                      (PlanT, stop, yield)
import           Data.Maybe                             (catMaybes, listToMaybe, mapMaybe)

import           Data.Solidity.Event                    (DecodeEvent (..))
import qualified Network.Ethereum.Api.Eth               as Eth
import           Network.Ethereum.Api.Provider          (Web3, forkWeb3)
import           Network.Ethereum.Api.Types             (Change (..),
                                                         DefaultBlock (..),
                                                         Filter (..), Quantity)
import           Network.Ethereum.Contract.Event.Common



-- | Middleware for intercepting Filters before they are used to get logs.
type FilterMiddleware e = Filter e -> Web3 (Filter e)

-- | Effectively a mapM_ over the machine using the given handler.
reduceEventStream
  :: forall k. MachineT Web3 k [Change]
  -> ([Change] -> Web3 EventAction)
  -> Web3 (Maybe (EventAction, Quantity))
reduceEventStream changes handler = fmap listToMaybe . runT $
     changes
  ~> autoM processChanges
  ~> asParts
  ~> runWhile (\(act, _) -> act /= TerminateEvent)
  ~> final
  where
    runWhile p = repeatedly $ do
      v <- await
      if p v
        then yield v
        else yield v >> stop
    processChanges  changes' = do
      act <- handler changes'
      if act == TerminateEvent
        then pure [(act, 0)]
        else case mapMaybe changeBlockNumber changes' of
          []  -> pure []
          bns -> pure [(act, maximum bns)]

-- | Queries a window of blocks at a time for logs matching the Filter. Maintains a lag behind the head of chain.
--   FilterMiddleware allows for logging/modifying the current filter.
rawFilter
  :: forall e. Filter e
  -> Integer -- window
  -> Integer -- lag
  -> FilterMiddleware e
  -> ([Change] -> Web3 EventAction)
  -> Web3 ()
rawFilter fltr window lag middleware handler = do
  start <- mkBlockNumber $ filterFromBlock fltr
  let initState = FilterStreamState { fssCurrentBlock  = start
                                    , fssInitialFilter = fltr
                                    , fssWindowSize    = window
                                    , fssLag           = lag
                                    }
  mLastProcessedFilterState <- reduceEventStream (playOldLogs middleware initState) handler
  case mLastProcessedFilterState of
    Nothing ->
      let pollingFilterState = FilterStreamState { fssCurrentBlock  = start
                                                 , fssInitialFilter = fltr
                                                 , fssWindowSize    = 1
                                                 , fssLag           = lag
                                                 }
      in  void $ reduceEventStream (playNewLogs middleware pollingFilterState) handler
    Just (act, lastBlock) -> do
      when (act /= TerminateEvent && (BlockWithNumber lastBlock < filterToBlock fltr))
        $ let pollingFilterState = FilterStreamState
                { fssCurrentBlock  = lastBlock + 1
                , fssInitialFilter = fltr
                , fssWindowSize    = 1
                , fssLag           = lag
                }
          in  void $ reduceEventStream
                (playNewLogs middleware pollingFilterState)
                handler

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playNewLogs :: forall e k. FilterMiddleware e -> FilterStreamState e -> MachineT Web3 k [Change]
playNewLogs middleware s = playLogs middleware $ newFilterStream s

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playOldLogs :: forall e k. FilterMiddleware e -> FilterStreamState e -> MachineT Web3 k [Change]
playOldLogs middleware s = playLogs middleware $ filterStream s

playLogs :: forall e k. FilterMiddleware e -> MachineT Web3 k (Filter e) -> MachineT Web3 k [Change]
playLogs middleware s =
     s
  ~> autoM middleware
  ~> autoM Eth.getLogs

-- | 'filterStream' is a machine which represents taking an initial filter
-- over a range of blocks b1, ... bn (where bn is possibly `Latest` or `Pending`,
-- but b1 is an actual block number), and making a stream of filter objects
-- which cover this filter in intervals of size `windowSize`. The machine
-- halts whenever the `fromBlock` of a spanning filter either (1) excedes then
-- initial filter's `toBlock` or (2) is greater than the chain head's block number.
filterStream
  :: forall e k. FilterStreamState e -> MachineT Web3 k (Filter e)
filterStream initialPlan = unfoldPlan initialPlan filterPlan
 where
  filterPlan
    :: FilterStreamState e
    -> PlanT k (Filter e) Web3 (FilterStreamState e)
  filterPlan initialState@FilterStreamState {..} = do
    filterEnd <- lift . mkBlockNumber $ filterToBlock fssInitialFilter
    let end = filterEnd - fromIntegral fssLag
    if fssCurrentBlock > end
      then stop
      else do
        let to'     = min end (fssCurrentBlock + fromInteger fssWindowSize)
            filter' = fssInitialFilter
              { filterFromBlock = BlockWithNumber fssCurrentBlock
              , filterToBlock   = BlockWithNumber to'
              }
        yield filter'
        filterPlan $ initialState { fssCurrentBlock = to' + 1 }


newFilterStream :: forall e k. FilterStreamState e -> MachineT Web3 k (Filter e)
newFilterStream initialState = unfoldPlan initialState filterPlan
 where
  filterPlan
    :: FilterStreamState e -> PlanT k (Filter e) Web3 (FilterStreamState e)
  filterPlan s@FilterStreamState {..} = do
    if BlockWithNumber fssCurrentBlock > filterToBlock fssInitialFilter
      then stop
      else do
        newestBlockNumber <- lift $ pollTillBlockProgress fssCurrentBlock fssLag
        -- we need newestBlockNumber > mfssCurrentBlock && newestBlockNumber <= chainHead - lag
        let filter' = fssInitialFilter
              { filterFromBlock = BlockWithNumber fssCurrentBlock
              , filterToBlock   = BlockWithNumber newestBlockNumber
              }
        yield filter'
        filterPlan $ s { fssCurrentBlock = newestBlockNumber + 1 }