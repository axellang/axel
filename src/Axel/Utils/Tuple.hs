module Axel.Utils.Tuple where

import Axel.Prelude

import Control.Lens.Operators ((^.))
import Control.Lens.Tuple (_1, _2)
import Control.Lens.Type (Lens)

type Annotated ann a = (a, ann)

annotate :: ann -> a -> Annotated ann a
annotate = flip (,)

annotateWith :: (a -> ann) -> a -> Annotated ann a
annotateWith f x = annotate (f x) x

unannotated :: Lens (Annotated ann a) (Annotated ann a') a a'
unannotated = _1

annotation :: Lens (Annotated ann a) (Annotated ann' a) ann ann'
annotation = _2

unannotate :: Annotated ann a -> a
unannotate = (^. unannotated)

flattenAnnotations ::
     Annotated ann (Annotated ann' a) -> Annotated (ann', ann) a
flattenAnnotations x =
  let outerAnnotation = x ^. annotation
      innerAnnotation = unannotate x ^. annotation
      innerValue = unannotate $ unannotate x
   in annotate (innerAnnotation, outerAnnotation) innerValue
