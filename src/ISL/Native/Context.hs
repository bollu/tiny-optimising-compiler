{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.Native.Context (islCtx) where

import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Language.Haskell.TH as TH

import ISL.Native.Types

islCtx :: C.Context
islCtx = baseCtx <> bsCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = islTypesTable
      }

islTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
islTypesTable = Map.fromList
  [ (C.TypeName "isl_ctx",         [t| Ctx        |])
  , (C.TypeName "isl_map",         [t| Map        |])
  , (C.TypeName "isl_basic_map",   [t| BasicMap   |])
  , (C.TypeName "isl_set",         [t| Set        |])
  , (C.TypeName "isl_basic_set",   [t| BasicSet   |])
  , (C.TypeName "isl_local_space", [t| LocalSpace |])
  , (C.TypeName "isl_space",       [t| Space      |])
  , (C.TypeName "isl_constraint",  [t| Constraint |])
  , (C.TypeName "isl_id",          [t| Id         |])
  , (C.TypeName "isl_dim_type",    [t| DimType    |])
  ]
