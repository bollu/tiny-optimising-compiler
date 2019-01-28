{-# LANGUAGE ForeignFunctionInterface #-}

-- | Prototype of a c2hs-based low-level interface. We will prefer a ISL.Native
-- for now, which uses inline-c instead.
module ISL.Native.C2Hs where

#include <isl/aff.h>
#include <isl/ctx.h>
#include <isl/constraint.h>
#include <isl/id.h>
#include <isl/map.h>
#include <isl/set.h>
#include <isl/space.h>
#include <isl/list.h>
#include <isl/multi.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C

import ISL.Native.Types
import Control.Monad (foldM)

type PtrCtx = Ptr Ctx
{#pointer *isl_ctx as PtrCtx -> Ctx nocode #}

{#fun isl_ctx_alloc as ctxAlloc {} -> `Ptr Ctx' id #}
{#fun isl_ctx_free  as ctxFree { id `Ptr Ctx' } -> `()' #}

type PtrMap = Ptr Map
{#pointer *isl_map as PtrMap -> Map nocode #}

type PtrSet = Ptr Set
{#pointer *isl_set as PtrSet -> Set nocode #}

type PtrBasicSet = Ptr BasicSet
{#pointer *isl_basic_set as PtrBasicSet -> BasicSet nocode #}

type PtrLocalSpace = Ptr LocalSpace
{#pointer *isl_local_space as PtrLocalSpace -> LocalSpace nocode #}

type PtrAff = Ptr Aff
{#pointer *isl_aff as PtrAff -> Aff nocode #}

type PtrPwAff = Ptr Pwaff
{#pointer *isl_pw_aff as PtrPwAff -> Pwaff nocode #}


type PtrPwmultiaff = Ptr Pwmultiaff
{#pointer *isl_pw_multi_aff as PtrPwmultiaff -> Pwmultiaff nocode #}

type PtrMultipwaff = Ptr Multipwaff
{#pointer *isl_multi_pw_aff as PtrMultipwaff -> Multipwaff nocode #}


type PtrVal = Ptr Val
{#pointer *isl_val as PtrVal -> Val nocode #}

type PtrSpace = Ptr Space
{#pointer *isl_space as PtrSpace -> Space nocode #}

type PtrConstraint = Ptr Constraint
{#pointer *isl_constraint as PtrConstraint -> Constraint nocode #}

type PtrId = Ptr Id
{#pointer *isl_id as PtrId -> Id nocode #}


-- type PtrListPwaff = Ptr (ListPwaff)
-- {#pointer *isl_pw_aff_list as PtrListPwaff -> ListPwaff nocode #}
type PtrListPwaff = Ptr (List Pwaff)
{#pointer *isl_pw_aff_list as PtrListPwaff -> List Pwaff nocode #}

-- =================
-- ID

{#fun isl_id_alloc as idAlloc_ { id `Ptr Ctx', `String', `Ptr ()' } -> `Ptr Id'  id #}
{#fun isl_id_copy as idCopy { id `Ptr Id' } -> `Ptr Id'  id #}

idAlloc :: Ptr Ctx -> String -> IO (Ptr Id)
idAlloc ctx s = idAlloc_ ctx s nullPtr

{#fun isl_map_free as mapFree { id `Ptr Map' } -> `()' #}
{#fun isl_set_free as setFree { id `Ptr Set' } -> `()' #}
{#fun isl_set_read_from_str as setReadFromStr
  { id `Ptr Ctx'
  , `String'
  } -> `Ptr Set' id #}
-- =================
-- map
{#fun isl_map_gist as mapGist
  { id `Ptr Map', id `Ptr Map'} -> `Ptr Map' id #}

{#fun isl_map_read_from_str as mapReadFromStr
  { id `Ptr Ctx'
  , `String'
  } -> `Ptr Map' id #}

{#fun isl_map_is_equal as mapIsEqual
  { id `Ptr Map'
  , id `Ptr Map'
  } -> `Bool' #}

{#fun isl_map_power as mapPower
  { id `Ptr Map'
  , alloca- `CInt' peek*
  } -> `Ptr Map' id #}


{#fun isl_map_add_dims as mapAddDims
  { id `Ptr Map'
  , fromDimType `DimType'
  , id `CUInt'
  } -> `Ptr Map' id #}

{#fun isl_map_project_out as mapProjectOut
  { id `Ptr Map'
  , fromDimType `DimType'
  , id `CUInt'
  , id `CUInt'
  } -> `Ptr Map' id #}

{#fun isl_map_transitive_closure as mapTransitiveClosure
  { id `Ptr Map'
  , alloca- `CInt' peek*
  } -> `Ptr Map' id #}


{#fun isl_map_copy as mapCopy
  { id `Ptr Map'
  } -> `Ptr Map' id #}

{#fun isl_map_domain as mapDomain
  { id `Ptr Map'
  } -> `Ptr Set' id #}


{#fun isl_map_range as mapRange
  { id `Ptr Map'
  } -> `Ptr Set' id #}

{#fun isl_map_intersect_domain as mapIntersectDomain
  { id `Ptr Map'
  , id `Ptr Set'
  } -> `Ptr Map' id #}

{#fun isl_map_intersect_range as mapIntersectRange
  { id `Ptr Map'
  , id `Ptr Set'
  } -> `Ptr Map' id #}

{#fun isl_map_union as mapUnion
  { id `Ptr Map'
  , id `Ptr Map'
  } -> `Ptr Map' id #}


{#fun isl_map_to_str as mapToStr
  { id `Ptr Map'
  } -> `String' #}


{#fun isl_map_from_pw_aff as mapFromPwaff
  { id `Ptr Pwaff'
  } -> `Ptr Map' id #}


{#fun isl_map_set_tuple_name as mapSetTupleName
  { id `Ptr Map'
  , fromDimType `DimType'
  , `String'
  } -> `Ptr Map' id #}


{#fun isl_map_set_dim_name as mapSetDimName
  { id `Ptr Map'
  , fromDimType `DimType'
  , id `CUInt'
  , `String'
  } -> `Ptr Map' id #}



{#fun isl_map_set_dim_id as mapSetDimId
  { id `Ptr Map'
  , fromDimType `DimType'
  , id `CUInt'
  , id `Ptr Id'
  } -> `Ptr Map' id #}


{#fun isl_map_get_space as mapGetSpace
  { id `Ptr Map' } -> `Ptr Space' id #}

{#fun isl_map_dim as mapDim
  { id `Ptr Map', fromDimType `DimType' } -> `CUInt' id #}


{#fun isl_map_get_dim_id as mapGetDimId
  { id `Ptr Map', fromDimType `DimType', id `CUInt'} -> `Ptr Id' id #}

{#fun isl_map_domain_product as mapDomainProduct
  { id `Ptr Map', id `Ptr Map'} -> `Ptr Map' id #}


{#fun isl_map_range_product as mapRangeProduct
  { id `Ptr Map', id `Ptr Map'} -> `Ptr Map' id #}


{#fun isl_map_domain_map as mapDomainMap
  { id `Ptr Map' } -> `Ptr Map' id #}


{#fun isl_map_range_map as mapRangeMap
  { id `Ptr Map' } -> `Ptr Map' id #}


{#fun isl_map_range_map as mapReverse
  { id `Ptr Map' } -> `Ptr Map' id #}


{#fun isl_map_wrap as mapWrap 
  { id `Ptr Map'  } -> `Ptr Set' id #}

{#fun isl_map_from_domain_and_range as mapFromDomainAndRange
  { id `Ptr Set', id `Ptr Set' } -> `Ptr Map' id #}

{#fun isl_map_apply_range as mapApplyRange
  { id `Ptr Map', 
    id `Ptr Map' } -> `Ptr Map' id #}

-- __isl_give isl_map *isl_map_move_dims(__isl_take isl_map *map,
-- 	enum isl_dim_type dst_type, unsigned dst_pos,
-- 	enum isl_dim_type src_type, unsigned src_pos, unsigned n);

{#fun isl_map_move_dims as mapMoveDims
  { id `Ptr Map', 
  fromDimType `DimType', id `CUInt',
  fromDimType `DimType', id `CUInt',
  id `CUInt' } -> `Ptr Map' id #}

{#fun isl_map_add_constraint as mapAddConstraint
  { id `Ptr Map'
  , id `Ptr Constraint'
  } -> `Ptr Map' id #}


-- =================
-- set

{#fun isl_set_project_out as setProjectOut
  { id `Ptr Set'
  , fromDimType `DimType'
  , id `CUInt'
  , id `CUInt'
  } -> `Ptr Set' id #}

{#fun isl_set_intersect as setIntersect
  { id `Ptr Set'
  , id `Ptr Set'
  } -> `Ptr Set' id #}

{#fun isl_set_union as setUnion
  { id `Ptr Set'
  , id `Ptr Set'
  } -> `Ptr Set' id #}


{#fun isl_set_get_space as setGetSpace
  { id `Ptr Set'
  } -> `Ptr Space' id #}


{#fun isl_set_to_str as setToStr
  { id `Ptr Set'
  } -> `String' #}

{#fun isl_set_unwrap as setUnwrap 
  { id `Ptr Set' } -> `Ptr Map' id #}


{#fun isl_set_add_dims as setAddDims
  { id `Ptr Set'
  , fromDimType `DimType'
  , id `CUInt'
  } -> `Ptr Set' id #}

{#fun isl_set_set_dim_id as setSetDimId
  { id `Ptr Set'
  , fromDimType `DimType'
  , id `CUInt'
  , id `Ptr Id'
  } -> `Ptr Set' id #}

  
{#fun isl_set_coalesce as setCoalesce
  { id `Ptr Set'
  } -> `Ptr Set' id #}

-- =================
-- constraint

{#fun isl_constraint_alloc_equality as constraintAllocEquality
  { id `Ptr LocalSpace'
  } -> `Ptr Constraint' id #}



{#fun isl_constraint_alloc_inequality as constraintAllocInequality
  { id `Ptr LocalSpace'
  } -> `Ptr Constraint' id #}

{#fun isl_constraint_set_coefficient_si as constraintSetCoefficientSi
  { id `Ptr Constraint'
  , fromDimType `DimType'
  , id `CInt'
  , id `CInt'
  } -> `Ptr Constraint' id #}

{#fun isl_constraint_set_constant_si as constraintSetConstantSi
  { id `Ptr Constraint'
  , id `CInt'
  } -> `Ptr Constraint' id #}

{#fun isl_basic_set_add_constraint as basicSetAddConstraint
  { id `Ptr BasicSet'
  , id `Ptr Constraint'
  } -> `Ptr BasicSet' id #}


{#fun isl_set_add_constraint as setAddConstraint
  { id `Ptr Set'
  , id `Ptr Constraint'
  } -> `Ptr Set' id #}

{#fun isl_space_set_alloc as spaceSetAlloc_
  { id `Ptr Ctx'
  , id `CUInt'
  , id `CUInt'
  } -> `Ptr Space' id #}

{#fun isl_basic_set_universe as basicSetUniverse
  { id `Ptr Space'
  } -> `Ptr BasicSet' id #}


-- =================
-- space
{#fun isl_space_copy as spaceCopy
  { id `Ptr Space'
  } -> `Ptr Space' id #}

{#fun isl_space_alloc as spaceAlloc_
  { id `Ptr Ctx', `CUInt', `CUInt', `CUInt'
  } -> `Ptr Space' id #}


{#fun isl_space_set_dim_id as spaceSetDimId
  { id `Ptr Space'
  , fromDimType `DimType'
  , id `CUInt'
  , id `Ptr Id'
  } -> `Ptr Space' id #}


{#fun isl_space_dim as spaceDim_
  { id `Ptr Space', fromDimType `DimType' } -> `CUInt' id #}

spaceDim :: Ptr Space -> DimType -> IO Int
spaceDim sp dt = fromIntegral <$> spaceDim_ sp dt 



{#fun isl_space_get_dim_id as spaceGetDimId_
  { id `Ptr Space', fromDimType `DimType', fromIntegral `CUInt'} -> `Ptr Id' id #}

spaceGetDimId :: Ptr Space -> DimType -> Int -> IO (Ptr Id)
spaceGetDimId space dt ix = spaceGetDimId_ space dt (fromIntegral ix)


{#fun isl_space_find_dim_by_id as spaceFindDimById_
  { id `Ptr Space', fromDimType `DimType', id `Ptr Id'} -> `CInt' id #}

spaceFindDimById :: Ptr Space -> DimType -> Ptr Id -> IO Int
spaceFindDimById sp dt id = fromIntegral <$> spaceFindDimById_ sp dt id


{#fun isl_space_add_dims as spaceAddDims
  { id `Ptr Space'
  , fromDimType `DimType'
  , id `CUInt'
  } -> `Ptr Space' id #}

{#fun isl_space_map_from_domain_and_range as spaceMapFromDomainAndRange
  { id `Ptr Space'
  , id `Ptr Space'
  } -> `Ptr Space' id #}


{#fun isl_space_align_params as spaceAlignParams
  { id `Ptr Space', id `Ptr Space' } -> `Ptr Space' id #}


-- =================
-- local space
{#fun isl_local_space_add_dims as localSpaceAddDims
  { id `Ptr LocalSpace'
  , fromDimType `DimType'
  , id `CUInt'
  } -> `Ptr LocalSpace' id #}

{#fun isl_local_space_get_space as localSpaceGetSpace
  { id `Ptr LocalSpace'
  } -> `Ptr Space' id #}

{#fun isl_local_space_dump as localSpaceDump
  { id `Ptr LocalSpace'
  } -> `()' #}

-- {#fun isl_local_space_to_str as localSpaceToStr
--   { id `Ptr LocalSpace'
--   } -> `String' #}

{#fun isl_local_space_from_space as localSpaceFromSpace
  { id `Ptr Space'
  } -> `Ptr LocalSpace' id #}

{#fun isl_local_space_copy as localSpaceCopy
  { id `Ptr LocalSpace'
  } -> `Ptr LocalSpace' id #}

{#fun isl_local_space_set_dim_name as localSpaceSetDimName
  { id `Ptr LocalSpace'
  , fromDimType `DimType'
  , id `CUInt'
  , `String'
  } -> `Ptr LocalSpace' id #}


{#fun isl_local_space_set_dim_id as localSpaceSetDimId
  { id `Ptr LocalSpace'
  , fromDimType `DimType'
  , fromIntegral `Int'
  , id `Ptr Id'
  } -> `Ptr LocalSpace' id #}

-- =================
-- basic set
{#fun isl_basic_set_project_out as basicSetProjectOut
  { id `Ptr BasicSet'
  , fromDimType `DimType'
  , id `CUInt'
  , id `CUInt'
  } -> `Ptr BasicSet' id #}

{#fun isl_basic_set_read_from_str as basicSetReadFromStr
  { id `Ptr Ctx'
  , `String'
  } -> `Ptr BasicSet' id #}

{#fun isl_basic_set_to_str as basicSetToStr
  { id `Ptr BasicSet'
  } -> `String' #}

-- =================
-- set

{# fun isl_set_universe as setUniverse
    {id `Ptr Space'} -> `Ptr Set' id #}
{# fun isl_set_indicator_function as setIndicatorFunction
    {id `Ptr Set'} -> `Ptr Pwaff' id #}


{# fun isl_set_apply as setApply
    {id `Ptr Set', id `Ptr Map'} -> `Ptr Set' id #}
-- =================
-- aff

{#fun isl_aff_copy as affCopy
  { id `Ptr Aff'
  } -> `Ptr Aff' id #}

{#fun isl_aff_val_on_domain as affValOnDomain
    {id `Ptr LocalSpace', id `Ptr Val'} -> `Ptr Aff' id #}

{#fun isl_aff_mul as affMul
    {id `Ptr Aff', id `Ptr Aff'} -> `Ptr Aff' id #}

{#fun isl_aff_var_on_domain as affVarOnDomain
    {id `Ptr LocalSpace',  fromDimType `DimType', fromIntegral `Int' } -> `Ptr Aff' id #}

{# fun isl_aff_to_str as affToStr
    {id `Ptr Aff' } -> `String'  #}
-- =================
-- Pwaff
{#fun isl_pw_aff_copy as pwaffCopy
  { id `Ptr Pwaff'
  } -> `Ptr Pwaff' id #}

{# fun isl_pw_aff_from_aff as pwaffFromAff
    {id `Ptr Aff' } -> `Ptr Pwaff' id #}

{# fun isl_pw_aff_to_str as pwaffToStr
    {id `Ptr Pwaff' } -> `String'  #}


{# fun isl_pw_aff_add as pwaffAdd
    {id `Ptr Pwaff', id `Ptr Pwaff'} -> `Ptr Pwaff' id #}

{# fun isl_pw_aff_lt_set as pwaffLtSet
    {id `Ptr Pwaff', id `Ptr Pwaff'} -> `Ptr Set' id #}

{# fun isl_pw_aff_align_params as pwaffAlignParams
    {id `Ptr Pwaff', id `Ptr Space'} -> `Ptr Pwaff' id #}

{# fun isl_pw_aff_get_space as pwaffGetSpace
    {id `Ptr Pwaff' } -> `Ptr Space' id #}


{# fun isl_pw_aff_get_domain_space as pwaffGetDomainSpace
    {id `Ptr Pwaff' } -> `Ptr Space' id #}

{#fun isl_pw_aff_add_dims as pwaffAddDims
  { id `Ptr Pwaff'
  , fromDimType `DimType'
  , id `CUInt'
  } -> `Ptr Pwaff' id #}

{#fun isl_pw_aff_set_dim_id as pwaffSetDimId
  { id `Ptr Pwaff'
  , fromDimType `DimType'
  , id `CUInt'
  , id `Ptr Id'
  } -> `Ptr Pwaff' id #}


{#fun isl_pw_aff_coalesce as pwaffCoalesce
  { id `Ptr Pwaff'
  } -> `Ptr Pwaff' id #}


{#fun isl_pw_aff_intersect_domain as pwaffIntersectDomain
  { id `Ptr Pwaff'
  , id `Ptr Set'
  } -> `Ptr Pwaff' id #}


{#fun isl_pw_aff_pullback_multi_pw_aff as pwaffPullbackMultipwaff
  { id `Ptr Pwaff'
  , id `Ptr Multipwaff'
  } -> `Ptr Pwaff' id #}


{#fun isl_pw_aff_is_equal as pwaffIsEqual
  { id `Ptr Pwaff'
  , id `Ptr Pwaff'
  } -> `Bool' #}

-- =================
-- pwaff list 


{#fun isl_pw_aff_list_alloc as pwaffListAlloc
  { id `Ptr Ctx', fromIntegral `Int' } -> `Ptr (List Pwaff)' id #}

{#fun isl_pw_aff_list_insert as pwaffListInsert
    {id `Ptr (List Pwaff)', fromIntegral `Int', id `Ptr Pwaff' } -> `Ptr (List Pwaff)' id #}

{#fun isl_pw_aff_list_add as pwaffListAdd
    {id `Ptr (List Pwaff)', id `Ptr Pwaff' } -> `Ptr (List Pwaff)' id #}

toListPwaff :: Ptr Ctx -> [Ptr Pwaff] -> IO (Ptr (List Pwaff))
toListPwaff ctx pws = do
    l <- pwaffListAlloc ctx 0
    foldM (\l pw -> pwaffListAdd l pw) l pws

{#fun isl_multi_pw_aff_from_pw_aff_list as multipwaffFromPwaffList 
{ id `Ptr Space', id `Ptr (List Pwaff)' } -> `Ptr Multipwaff' id #}


{# fun isl_multi_pw_aff_to_str as multipwaffToStr
    {id `Ptr Multipwaff' } -> `String' #}

-- =================
-- pw multi aff
{# fun isl_pw_multi_aff_from_map as pwmultiaffFromMap
    {id `Ptr Map' } -> `Ptr Pwmultiaff' id #}


{# fun isl_pw_multi_aff_to_str as pwmultiaffToStr
    {id `Ptr Pwmultiaff' } -> `String' #}

{# fun isl_pw_multi_aff_get_pw_aff as pwmultiaffGetPwaff
    {id `Ptr Pwmultiaff', id `CInt' } -> `Ptr Pwaff' id #}

-- val
-- =================
{#fun isl_val_int_from_si as valIntFromSI
    {id  `Ptr Ctx', id `CLong'} -> `Ptr Val' id #}

affInt :: Ptr Ctx -> Ptr LocalSpace -> Int -> IO (Ptr Aff)
affInt ctx ls i = do
  v <- valIntFromSI ctx (fromIntegral i)
  affValOnDomain ls v

pwaffInt :: Ptr Ctx -> Ptr LocalSpace -> Int -> IO (Ptr Pwaff)
pwaffInt ctx ls i = do
    aff <- affInt ctx ls i
    pwaffFromAff aff
    
spaceSetAlloc :: Ptr Ctx -> Int -> Int -> IO (Ptr Space)
spaceSetAlloc ctx nparam ndim = 
    spaceSetAlloc_ ctx (fromIntegral nparam) (fromIntegral ndim)

localSpaceSetAlloc :: Ptr Ctx -> Int -> Int -> IO (Ptr LocalSpace)
localSpaceSetAlloc ctx nparam ndim = 
    spaceSetAlloc ctx nparam ndim >>= localSpaceFromSpace


spaceAlloc :: Ptr Ctx -> Int -> Int -> Int -> IO (Ptr Space)
spaceAlloc ctx nparam nin nout = 
    spaceAlloc_ ctx (fromIntegral nparam) (fromIntegral nin) (fromIntegral nout)


localSpaceAlloc :: Ptr Ctx -> Int -> Int -> Int -> IO (Ptr LocalSpace)
localSpaceAlloc ctx np nin nout =
    spaceAlloc ctx np nin nout >>= localSpaceFromSpace

