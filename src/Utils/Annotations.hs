{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Annotations (Annotations, AnnGettable(..), singleA, ShowAll) where

import GHC.Exts (Constraint)

data Annotations (k::[*]) where
  AEmpty :: Annotations '[]
  ACons  :: t -> Annotations k -> Annotations (t:k)

class AnnGettable t k where
  getAnn :: k -> t
  
instance AnnGettable t (Annotations xs) => AnnGettable t (Annotations (x:xs)) where
  getAnn :: Annotations (x:xs) -> t
  getAnn (ACons _ xs) = getAnn xs

instance AnnGettable t (Annotations (t:xs)) where
  getAnn :: Annotations (t:xs) -> t
  getAnn (ACons x _) = x

singleA:: a -> Annotations '[a]
singleA i = ACons i AEmpty

type family ShowAll x :: Constraint where
  ShowAll '[] = ()
  ShowAll (x:xs) = (Show x, ShowAll xs)

annDoShow :: ShowAll x => Annotations x -> String
annDoShow AEmpty = ""
annDoShow (ACons x AEmpty) = show x
annDoShow (ACons a r) = show a ++ ", " ++ annDoShow r

instance ShowAll x => Show (Annotations x) where
  show a = "{" ++ annDoShow a ++ "}"
