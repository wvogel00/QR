module Basic where

import Type

ionR :: Ion -> Radius
ionR (Ion _ _ _ r) = r

poleOf :: Ion -> Pole
poleOf (Ion Plus _ _ _) = Plus
