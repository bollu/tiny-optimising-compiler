{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An inline-c based low-level interface to isl.
module ISL.Native
  ( IslCopy(copy)
  , IslFree(free)

  , ctxFree

  , basicSetCopy
  , basicSetFree

  , unsafeSetIntersect
  , setIntersect
  , unsafeSetUnion
  , setUnion
  , unsafeSetSubtract
  , setSubtract
  , setCopy
  , setEmpty
  , unsafeSetUniverse
  , setUniverse
  , setGetSpace
  , setFree
  , setNBasicSet
  , unsafeSetCoalesce
  , setCoalesce
  , unsafeSetParams
  , setParams
  , setComplement
  , setGetDimId
  , unsafeSetProjectOut
  , setProjectOut

  , basicMapCopy
  , basicMapFree

  , mapCopy
  , mapFree

  , localSpaceCopy
  , localSpaceFree

  , spaceCopy
  , spaceFree

  , constraintCopy
  , constraintFree

  , idCopy
  , idFree
  ) where

import Control.Monad (void)
import Foreign.Ptr
import Foreign.C
import qualified Language.C.Inline as C

import ISL.Native.Context (islCtx)
import ISL.Native.Types

C.context islCtx

C.include "<math.h>"
C.include "<isl/ctx.h>"
C.include "<isl/constraint.h>"
C.include "<isl/id.h>"
C.include "<isl/map.h>"
C.include "<isl/set.h>"
C.include "<isl/space.h>"

class IslCopy a where
  copy :: Ptr a -> Ptr a

class IslFree a where
  free :: Ptr a -> IO ()

-- __isl_take: can no longer be used
-- __isl_keep: only used temporarily

-- * Ctx

instance IslFree Ctx where free = ctxFree

ctxFree :: Ptr Ctx -> IO ()
ctxFree ctx = [C.block| void { isl_ctx_free($(isl_ctx* ctx)); } |]

-- * BasicSet

instance IslCopy BasicSet where copy = basicSetCopy
instance IslFree BasicSet where free = basicSetFree

basicSetCopy :: Ptr BasicSet -> Ptr BasicSet
basicSetCopy bset =
  [C.pure| isl_basic_set* { isl_basic_set_copy($(isl_basic_set* bset)) } |]

basicSetFree :: Ptr BasicSet -> IO ()
basicSetFree bset = void
  [C.block| isl_basic_set* { isl_basic_set_free($(isl_basic_set* bset)); } |]

-- * Set

instance IslCopy Set where copy = setCopy
instance IslFree Set where free = setFree

setCopy :: Ptr Set -> Ptr Set
setCopy set = [C.pure| isl_set* { isl_set_copy($(isl_set* set)) } |]

setFree :: Ptr Set -> IO ()
setFree set = void [C.block| isl_set* { isl_set_free($(isl_set* set)); } |]

unsafeSetIntersect :: Ptr Set -> Ptr Set -> Ptr Set
unsafeSetIntersect set1 set2 = [C.pure| isl_set* {
  isl_set_intersect($(isl_set* set1), $(isl_set* set2))
  } |]

setIntersect :: Ptr Set -> Ptr Set -> Ptr Set
setIntersect set1 set2 = unsafeSetIntersect (setCopy set1) (setCopy set2)

unsafeSetUnion :: Ptr Set -> Ptr Set -> Ptr Set
unsafeSetUnion set1 set2 = [C.pure| isl_set* {
  isl_set_union($(isl_set* set1), $(isl_set* set2))
  } |]

setUnion :: Ptr Set -> Ptr Set -> Ptr Set
setUnion set1 set2 = unsafeSetUnion (setCopy set1) (setCopy set2)

unsafeSetSubtract :: Ptr Set -> Ptr Set -> Ptr Set
unsafeSetSubtract set1 set2 = [C.pure| isl_set* {
  isl_set_subtract($(isl_set* set1), $(isl_set* set2))
  } |]

setSubtract :: Ptr Set -> Ptr Set -> Ptr Set
setSubtract set1 set2 = unsafeSetSubtract (setCopy set1) (setCopy set2)

-- | Create an empty set
setEmpty :: Ptr Space -> Ptr Set
setEmpty space = [C.pure| isl_set* { isl_set_empty($(isl_space* space)) } |]

-- | Create a universe set
unsafeSetUniverse :: Ptr Space -> Ptr Set
unsafeSetUniverse space = [C.pure| isl_set* {
  isl_set_universe($(isl_space* space))
  } |]

setUniverse :: Ptr Space -> Ptr Set
setUniverse = unsafeSetUniverse . spaceCopy

-- | It is often useful to create objects that live in the same space as some
-- other object. This can be accomplished by creating the new objects based on
-- the space of the original object.
setGetSpace :: Ptr Set -> Ptr Space
setGetSpace set = [C.pure| isl_space* {
  isl_set_get_space($(isl_set* set))
  } |]

-- | The number of basic sets in a set can be obtained
setNBasicSet :: Ptr Set -> CInt
setNBasicSet set = [C.pure| int { isl_set_n_basic_set($(isl_set* set)) } |]

unsafeSetCoalesce :: Ptr Set -> Ptr Set
unsafeSetCoalesce set =
  [C.pure| isl_set* { isl_set_coalesce($(isl_set* set)) } |]

-- | Simplify the representation of a set by trying to combine pairs of basic
-- sets into a single basic set.
setCoalesce :: Ptr Set -> Ptr Set
setCoalesce = unsafeSetCoalesce . setCopy

-- | Projection
unsafeSetParams :: Ptr Set -> Ptr Set
unsafeSetParams set = [C.pure| isl_set* { isl_set_params($(isl_set* set)) } |]

setParams :: Ptr Set -> Ptr Set
setParams = unsafeSetParams . setCopy

unsafeSetComplement :: Ptr Set -> Ptr Set
unsafeSetComplement set =
  [C.pure| isl_set* { isl_set_complement($(isl_set* set)) } |]

-- | Projection
setComplement :: Ptr Set -> Ptr Set
setComplement = unsafeSetComplement . setCopy

setGetDimId :: Ptr Set -> DimType -> CUInt -> Ptr Id
setGetDimId set ty pos =
  let ty' :: CInt
      ty' = fromDimType ty
  in [C.pure| isl_id* {
     isl_set_get_dim_id(
       $(isl_set* set),
       $(int ty'),
       $(unsigned int pos)
     )
     } |]

unsafeSetProjectOut :: Ptr Set -> DimType -> CUInt -> CUInt -> Ptr Set
unsafeSetProjectOut set ty first n =
  let ty' :: CInt
      ty' = fromDimType ty
  in [C.pure| isl_set* {
     isl_set_project_out(
       $(isl_set* set),
       $(int ty'),
       $(unsigned int first),
       $(unsigned int n)
     )
     } |]

setProjectOut :: Ptr Set -> DimType -> CUInt -> CUInt -> Ptr Set
setProjectOut set ty first n = unsafeSetProjectOut (setCopy set) ty first n

-- * BasicMap

instance IslCopy BasicMap where copy = basicMapCopy
instance IslFree BasicMap where free = basicMapFree

basicMapCopy :: Ptr BasicMap -> Ptr BasicMap
basicMapCopy bmap =
  [C.pure| isl_basic_map* { isl_basic_map_copy($(isl_basic_map* bmap)) } |]

basicMapFree :: Ptr BasicMap -> IO ()
basicMapFree bmap = void
  [C.block| isl_basic_map* { isl_basic_map_free($(isl_basic_map* bmap)); } |]

-- * Map

instance IslCopy Map where copy = mapCopy
instance IslFree Map where free = mapFree

mapCopy :: Ptr Map -> Ptr Map
mapCopy map = [C.pure| isl_map* { isl_map_copy($(isl_map* map)) } |]

mapFree :: Ptr Map -> IO ()
mapFree map = void [C.block| isl_map* { isl_map_free($(isl_map* map)); } |]

-- * LocalSpace

instance IslCopy LocalSpace where copy = localSpaceCopy
instance IslFree LocalSpace where free = localSpaceFree

localSpaceCopy :: Ptr LocalSpace -> Ptr LocalSpace
localSpaceCopy ls =
  [C.pure| isl_local_space* { isl_local_space_copy($(isl_local_space* ls)) } |]

localSpaceFree :: Ptr LocalSpace -> IO ()
localSpaceFree ls = void
  [C.block| isl_local_space* { isl_local_space_free($(isl_local_space* ls)); } |]

-- * Space

instance IslCopy Space where copy = spaceCopy
instance IslFree Space where free = spaceFree

spaceCopy :: Ptr Space -> Ptr Space
spaceCopy space =
  [C.pure| isl_space* { isl_space_copy($(isl_space* space)) } |]

spaceFree :: Ptr Space -> IO ()
spaceFree space = void
  [C.block| isl_space* { isl_space_free($(isl_space* space)); } |]

-- * Constraint

instance IslCopy Constraint where copy = constraintCopy
instance IslFree Constraint where free = constraintFree

constraintCopy :: Ptr Constraint -> Ptr Constraint
constraintCopy c =
  [C.pure| isl_constraint* { isl_constraint_copy($(isl_constraint* c)) } |]

constraintFree :: Ptr Constraint -> IO ()
constraintFree c = void
  [C.block| isl_constraint* { isl_constraint_free($(isl_constraint* c)); } |]

-- * Id

instance IslCopy Id where copy = idCopy
instance IslFree Id where free = idFree

idCopy :: Ptr Id -> Ptr Id
idCopy i = [C.pure| isl_id* { isl_id_copy($(isl_id* i)) } |]

idFree :: Ptr Id -> IO ()
idFree i = void [C.block| isl_id* { isl_id_free($(isl_id* i)); } |]
