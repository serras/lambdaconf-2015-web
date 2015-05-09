{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses,
             EmptyDataDecls, FlexibleContexts, FlexibleInstances,
             GADTs, GeneralizedNewtypeDeriving #-}
module Db where

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  firstName  String
  lastName   String
  UniqueName firstName lastName
  deriving Show
Task json
  title     String
  completed Bool
  user      UserId
  deriving Show
|]
