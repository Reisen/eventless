module Eventless.Types.BackendStore
  ( BackendStore(..)
  )
where

import           Protolude
import           Data.Aeson                     ( FromJSON )
import           Data.UUID                      ( UUID )
import           Eventless.Types.Aggregate
import           Eventless.Types.Event


data BackendStore = Backend
  { loadLatest            :: LoadLatest
  , loadVersion           :: LoadVersion
  , writeEventTransaction :: WriteEventTransaction
  , writeSingleEvent      :: WriteSingleEvent
  }


type LoadLatest
  =  forall a m. MonadIO m
  => FromJSON a
  => UUID
  -> m (Maybe (Aggregate a))

type LoadVersion
  =  forall a m. MonadIO m
  => FromJSON a
  => UUID
  -> Int
  -> m (Maybe (Aggregate a))

type WriteEventTransaction
  =  forall m t. MonadIO m
  => Traversable t
  => UUID
  -> t Event
  -> m ()

type WriteSingleEvent
  =  forall m. MonadIO m
  => UUID
  -> Event
  -> m ()
