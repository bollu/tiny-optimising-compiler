{-# LANGUAGE ForeignFunctionInterface #-}

-- | Types for the low-level interface to isl.
module ISL.Native.Types where

#include <isl/space.h>

import Foreign.C (CInt)

-- | A given context can only be used within a single thread, and all arguments
-- to a function must be allocated within the same context. All objects
-- allocated within a context should be freed before the context is freed.
data Ctx

-- | A single-space set of tuples that can be described as a conjunction of
-- affine constraints.
data BasicSet

-- | A union of 'BasicSet's
data Set

-- | A single-space relation mapping tuples to tuples that can be described as
-- a conjunction of affine constraints.
data BasicMap

-- | A union of 'BasicMap's
data Map

-- | A local space is essentially a space with zero or more existentially
-- quantified variables. The local space of various objects can be obtained
-- using the following functions.
data LocalSpace

-- | Whenever a new set, relation or similar object is created from scratch,
-- the space in which it lives needs to be specified using an isl_space. Each
-- space involves zero or more parameters and zero, one or two tuples of set or
-- input/output dimensions. The parameters and dimensions are identified by an
-- isl_dim_type and a position. The type isl_dim_param refers to parameters,
-- the type isl_dim_set refers to set dimensions (for spaces with a single
-- tuple of dimensions) and the types isl_dim_in and isl_dim_out refer to input
-- and output dimensions (for spaces with two tuples of dimensions).  Local
-- spaces (see §1.4.9) also contain dimensions of type isl_dim_div.  Note that
-- parameters are only identified by their position within a given object.
-- Across different objects, parameters are (usually) identified by their names
-- or identifiers. Only unnamed parameters are identified by their positions
-- across objects. The use of unnamed parameters is discouraged.
data Space

-- | An affine constraint.
data Constraint

-- | Identifiers are used to identify both individual dimensions and tuples of
-- dimensions.  They consist of an optional name and an optional user pointer.
-- The name and the user pointer cannot both be NULL, however. Identifiers with
-- the same name but different pointer values are considered to be distinct.
-- Similarly, identifiers with different names but the same pointer value are
-- also considered to be distinct. Equal identifiers are represented using the
-- same object. Pairs of identifiers can therefore be tested for equality using
-- the == operator. Identifiers can be constructed, copied, freed, inspected
-- and printed using the following functions.
data Id

-- an ISL List of a's
data List a

data Val
data Aff
data Pwaff
data Pwmultiaff
data Multipwaff

{#enum isl_dim_type as DimType {underscoreToCase} deriving(Eq, Show) #}
{#enum isl_bool as IslBool {underscoreToCase} deriving(Eq, Show) #}

fromDimType :: DimType -> CInt
fromDimType = fromIntegral . fromEnum

fromRawIslBool :: CInt -> Maybe Bool
fromRawIslBool i = 
    case (fromIntegral i) of
        -1 -> Nothing
        0 -> Just False
        1 -> Just True


