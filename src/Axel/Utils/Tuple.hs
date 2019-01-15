module Axel.Utils.Tuple where

import Control.Lens.Operators ((^.))
import Control.Lens.Tuple (_1, _2)
import Control.Lens.Type (Lens)

type Annotated a b = (a, b)

annotate :: b -> a -> Annotated a b
annotate = flip (,)

annotateWith :: (a -> b) -> a -> Annotated a b
annotateWith f x = annotate (f x) x

unannotated :: Lens (Annotated a b) (Annotated a' b) a a'
unannotated = _1

annotation :: Lens (Annotated a b) (Annotated a b') b b'
annotation = _2

unannotate :: Annotated a b -> a
unannotate = (^. unannotated)

flattenAnnotations :: Annotated (Annotated a b) c -> Annotated a (b, c)
flattenAnnotations x =
  let outerAnnotation = x ^. annotation
      innerAnnotation = unannotate x ^. annotation
      innerValue = unannotate $ unannotate x
   in annotate (innerAnnotation, outerAnnotation) innerValue
