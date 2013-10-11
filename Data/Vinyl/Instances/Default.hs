{-# LANGUAGE DataKinds #-}          -- required by vinyl
{-# LANGUAGE TypeOperators #-}      -- required by vinyl
{-# LANGUAGE FlexibleContexts #-}   -- needed by vinyl record instance definition Default
{-# LANGUAGE FlexibleInstances #-}  -- needed by vinyl record instance definition Default
{-# LANGUAGE TypeFamilies #-}       -- needed by TplGLFW
{-# LANGUAGE OverlappingInstances #-} 

module Data.Vinyl.Instances.Default ( Default
                                    , WrapDefault(..)
                                    )where 

import Data.Vinyl
import Data.Default

{--- Wrap vinyl PlainRec type for default instance-}
{-newtype DefaultPlainRec rs = DefaultPlainRec {getDefPlainRec :: PlainRec rs}-}

{--- Default for empty rec is nil-}
{-instance Default (DefaultPlainRec '[]) where-}
    {-def = DefaultPlainRec RNil-}

{--- Defualt for PlainRec is a PlainRec for which Default is defined for the head and the tial of the type list-}
{-instance (Default t, Default (DefaultPlainRec rs)) => Default (DefaultPlainRec ((sy ::: t) ': rs)) where-}
    {-def = DefaultPlainRec (Field  =: def <+> getDefPlainRec def)-}

{--- Default for empty rec is nil-}
{-instance Default (PlainRec '[]) where-}
    {-def = RNil-}

{--- Defualt for PlainRec is a PlainRec for which Default is defined for the head and the tial of the type list-}
{-instance (Default t, Default (PlainRec rs)) => Default (PlainRec ((sy ::: t) ': rs)) where-}
    {-def = (Field  =: def <+>  def)-}

-- Wrap vinyl PlainRec type for default instance

newtype WrapDefault a = WrapDefault {unwrapDefault :: a}

instance Default (WrapDefault (PlainRec '[])) where
    def = WrapDefault RNil

instance (Default (WrapDefault t), Default (WrapDefault (PlainRec rs))) => Default (WrapDefault (PlainRec ((sy ::: t) ': rs))) where
    def = WrapDefault (Field  =: unwrapDefault def <+>  unwrapDefault def)

instance (Default a) => Default (WrapDefault a) where 
    def = WrapDefault def

{-type BlahL = PlainRec ["_blah" ::: Int, "_blop" ::: Float]-}
{-type BlahF = "blahs" ::: BlahL-}
{-type Blah = PlainRec ["blah" ::: Int, "blop" ::: Float, BlahF]-}

{-defBlah :: Blah-}
{-defBlah = unwrapDefault def-}
