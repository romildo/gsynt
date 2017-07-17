module Doc where

import Data.List (partition)

import Text.LaTeX (Texy(texy), render)
import Text.LaTeX.Base.Class (LaTeXC, liftL)
import Text.LaTeX.Base.Syntax (LaTeX(TeXRaw, TeXComm), TeXArg(FixArg, MSymArg))
import Text.LaTeX.Packages.Beamer (OverlaySpec(FromSlide, ToSlide, FromToSlide, OneSlide), only, onslide, uncover, visible, invisible, alert)
import Text.LaTeX.Packages.Color (ColSpec)

data EffectName
  = Vis     -- ^ visible
  | Invis   -- ^ invisible
  | Alert   -- ^ alert
  | Only    -- ^ only
  | OnSlide -- ^ on slide
  | Uncover -- ^ uncover
  | Shade   -- ^ shade
  | Shade2   -- ^ shade 2
  deriving (Eq,Show)

-- Range end points
data RangeIndicator
  = B                 -- ^ begin
  | E                 -- ^ end
  | BE                -- ^ begin end
  deriving (Eq,Show)

type Slide = Int -- ^ slide number

type Effect = (EffectName, RangeIndicator, Slide)

type Effects = [Effect]

data Doc a =
  Doc
  { overlayEffects :: Effects
  , info           :: a
  }
  deriving (Eq,Show)

findEnd :: EffectName -> [Effect] -> Maybe (Slide, [Effect])
findEnd x lst = findEndAux x lst 0 []

findEndAux :: EffectName -> [Effect] -> Slide -> [Effect] -> Maybe (Slide, [Effect])
findEndAux x (k@(y,B,_):ks)   n acc | x == y = findEndAux x ks (n+1) (k:acc)
findEndAux x ((y,E,slide):ks) 0 acc | x == y = Just (slide,reverse acc ++ ks)
findEndAux x (k@(y,E,_):ks)   n acc | x == y = findEndAux x ks (n-1) (k:acc)
findEndAux x (k:ks)           n acc          = findEndAux x ks n (k:acc)
findEndAux _ []               _ _            = Nothing

applyEffects :: LaTeXC l => [Effect] -> l -> l
applyEffects lst =
  applyEffects'' (compileEffects lst)
-- applyEffects lst =
--   id

applyEffects'' :: LaTeXC l => [(EffectName, OverlaySpec)] -> l -> l
applyEffects'' [] = id
applyEffects'' lst@((action, _) : _) =
  applyEffectName action (map snd lst1) . applyEffects'' lst2
  where
    (lst1, lst2) = partition ((== action) . fst) lst

applyEffectName :: LaTeXC l => EffectName -> [OverlaySpec] -> l -> l
applyEffectName effect = case effect of
                           Only    -> only
                           OnSlide -> onslide
                           Uncover -> uncover
                           Vis     -> visible
                           Invis   -> invisible
                           Alert   -> alert
                           Shade   -> bgyellow
                           Shade2  -> bggreen

compileEffects :: [Effect] -> [(EffectName, OverlaySpec)]
compileEffects [] = []
compileEffects ((effectName,pos,slide) : others) =
  (effectName, overlaySpec) : compileEffects rest
  where
    (overlaySpec,rest) = case pos of
                          B  -> case findEnd effectName others of
                                 Just (slide',others') ->
                                   (FromToSlide slide slide', others')
                                 Nothing ->
                                   (FromSlide slide, others)
                          E  -> (ToSlide slide, others)
                          BE -> (OneSlide slide, others)


bgyellow :: LaTeXC l => [OverlaySpec] -> l -> l
bgyellow os =
  liftL $ \l -> TeXComm "bgyellow" [ MSymArg $ fmap (TeXRaw . render) os , FixArg l ]

bggreen :: LaTeXC l => [OverlaySpec] -> l -> l
bggreen os =
  liftL $ \l -> TeXComm "bggreen" [ MSymArg $ fmap (TeXRaw . render) os , FixArg l ]


instance Texy a => Texy (Doc a) where
  texy (Doc overlays x) = applyEffects overlays (texy x)

instance Functor Doc where
  fmap f (Doc overlays x) = Doc overlays (f x)


addDoc :: [Effect] -> Doc a -> Doc a
addDoc ans1 (Doc ans2 x) =
  Doc (ans2 ++ ans1) x

clearDoc :: Doc a -> Doc a
clearDoc (Doc _ x) = Doc [] x


groupAssoc :: Eq a => [(a, b)] -> [(a, [b])]
groupAssoc [] = []
groupAssoc list@((firstKey,_):_) = (firstKey, map snd list1) : groupAssoc list2
  where
    (list1, list2) = partition ((firstKey ==) . fst) list
