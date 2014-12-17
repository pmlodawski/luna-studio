---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.Merge where

import qualified Data.Array.Accelerate as A

import           Flowbox.Graphics.Composition.Generators.Structures
import qualified Flowbox.Graphics.Utils                             as U
import           Flowbox.Prelude                                    hiding (min, max)
import qualified Flowbox.Prelude                                    as P



type Overlay a         = A.Exp a
type OverlayAlpha a    = A.Exp a
type Background a      = A.Exp a
type BackgroundAlpha a = A.Exp a

type BlendMode a = Overlay a -> Background a -> A.Exp a

type ComplicatedBlendMode a = Overlay a -> OverlayAlpha a -> Background a -> BackgroundAlpha a -> A.Exp a

data AlphaBlend = Adobe
                | Custom

union :: Num a => a -> a -> a
union a b = a + b - (a * b)

-- FIXME [KL]: Bounding box now is taken from the overlay generator
basicColorCompositingFormula :: (A.Elt a, A.IsFloating a)
                             => Generator x (A.Exp a) -- ^ Overlay / Source / Foreground / A
                             -> Generator x (A.Exp a) -- ^ Overlay alpha
                             -> Generator x (A.Exp a) -- ^ Background / Destination / B
                             -> Generator x (A.Exp a) -- ^ Background alpha
                             -> AlphaBlend            -- ^ Specifies if the same blending method is used on alpha channels
                             -> BlendMode a           -- ^ Function used for blending
                             -> Generator x (A.Exp a) -- ^ Merge result
basicColorCompositingFormula (Generator cnv overlay) (Generator _ alphaOverlay) (Generator _ background) (Generator _ alphaBackground) alphaBlend blend =
    Generator cnv $ \p ->
    let alphaResult p' = case alphaBlend of
            Adobe  -> alphaOverlay p' `union` alphaBackground p'
            Custom -> alphaOverlay p' `blend` alphaBackground p'
    in (1 - (alphaOverlay p / alphaResult p)) * background p + (alphaOverlay p / alphaResult p) *
        (U.invert (alphaBackground p) * overlay p + alphaBackground p * blend (overlay p) (background p))

-- FIXME [KL]: Bounding box now is taken from the overlay generator
threeWayMerge :: (A.Elt a, A.IsFloating a)
              => ComplicatedBlendMode a
              -> Generator x (A.Exp a) -- ^ A.R
              -> Generator x (A.Exp a) -- ^ A.G
              -> Generator x (A.Exp a) -- ^ A.B
              -> Generator x (A.Exp a) -- ^ B.R
              -> Generator x (A.Exp a) -- ^ B.G
              -> Generator x (A.Exp a) -- ^ B.B
              -> Generator x (A.Exp a) -- ^ A.A
              -> Generator x (A.Exp a) -- ^ B.A
              -> (Generator x (A.Exp a), Generator x (A.Exp a), Generator x (A.Exp a), Generator x (A.Exp a))
threeWayMerge blend ar ag ab br bg bb aa ba =
    (merge ar aa br ba, merge ag aa bg ba, merge ab aa bb ba, ba)
    where merge ov aov bgnd abgnd = complicatedColorCompositingFormula ov aov bgnd abgnd blend

-- FIXME [KL]: Bounding box now is taken from the aa' generator
threeWayMerge' :: (A.Elt a, A.IsFloating a)
              => AlphaBlend
              -> BlendMode a
              -> Generator x (A.Exp a) -- ^ A.R
              -> Generator x (A.Exp a) -- ^ A.G
              -> Generator x (A.Exp a) -- ^ A.B
              -> Generator x (A.Exp a) -- ^ B.R
              -> Generator x (A.Exp a) -- ^ B.G
              -> Generator x (A.Exp a) -- ^ B.B
              -> Generator x (A.Exp a) -- ^ A.A
              -> Generator x (A.Exp a) -- ^ B.A
              -> (Generator x (A.Exp a), Generator x (A.Exp a), Generator x (A.Exp a), Generator x (A.Exp a))
threeWayMerge' alphaBlend blend ar ag ab br bg bb aa ba =
  (merge ar aa br ba, merge ag aa bg ba, merge ab aa bb ba, mergeAlpha aa ba)
  where merge ov aov bgnd abgnd = basicColorCompositingFormula ov aov bgnd abgnd alphaBlend blend
        mergeAlpha (Generator cnv aa') (Generator _ ba') = Generator cnv $ \p -> case alphaBlend of
            Adobe  -> aa' p `union` ba' p
            Custom -> aa' p `blend` ba' p

-- FIXME [KL]: Bounding box now is taken from the overlay generator
complicatedColorCompositingFormula :: (A.Elt a, A.IsFloating a)
                                   => Generator x (A.Exp a)
                                   -> Generator x (A.Exp a)
                                   -> Generator x (A.Exp a)
                                   -> Generator x (A.Exp a)
                                   -> ComplicatedBlendMode a
                                   -> Generator x (A.Exp a)
complicatedColorCompositingFormula (Generator cnv overlay) (Generator _ alphaOverlay) (Generator _ background) (Generator _ alphaBackground) blend =
    Generator cnv $ \p -> blend (overlay p) (alphaOverlay p) (background p) (alphaBackground p)

liftBlend :: (A.Elt a, A.IsFloating a) => BlendMode a -> ComplicatedBlendMode a
liftBlend blend overlay _ background _ = blend overlay background


-- | A*b + B*(1-a)
atop :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
atop overlay alphaOverlay background alphaBackground = overlay * alphaBackground + background * U.invert alphaOverlay
--atop overlay alphaOverlay background alphaBackground = background * alphaOverlay + overlay * U.invert alphaBackground

-- | (A+B)/2
average :: (A.Elt a, A.IsFloating a) => BlendMode a
average overlay background = (overlay + background) / 2

-- | 1 - min (1, (1-B)/A), 0 if A = 0
-- Imagemagick says: 1 - ((1-B) / A)
colorBurn :: (A.Elt a, A.IsFloating a) => BlendMode a
colorBurn overlay background = (overlay A.==* 0.0) A.? (0, U.invert $ min 1 $ U.invert background / overlay)

-- | min(1, B / (1-A)), 1 if A = 1
-- Imagemagick says: B / (1-A)
colorDodge :: (A.Elt a, A.IsFloating a) => BlendMode a
colorDodge overlay background = (overlay A.==* 1.0) A.? (1, min 1 (background / U.invert overlay))

-- | A + B(1-a)/b, A if a > b
-- Dividing by zero avoided due to implementing a >= b condition. That way, if a = b = 0,
-- it takes a pixel from A. Nuke does the same thing.
conjointOver :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
conjointOver overlay alphaOverlay background alphaBackground =
    (alphaOverlay A.>=* alphaBackground) A.? 
        (overlay,
        overlay + background * U.invert alphaOverlay / alphaBackground)

-- | A
copy :: (A.Elt a, A.IsFloating a) => BlendMode a
copy overlay _ = overlay

-- | |A-B|
difference :: (A.Elt a, A.IsFloating a) => BlendMode a
difference overlay background = abs (overlay - background)

-- | A + B(1-a)/b, A + B if a + b < 1
disjointOver :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
disjointOver overlay alphaOverlay background alphaBackground =
    (alphaOverlay + alphaBackground A.<* 1) A.?
        (overlay + background,
        overlay + background * U.invert alphaOverlay / alphaBackground)

-- | A / B, 0 if B = 0 or (A < 0 and B < 0)
--  In Nuke docs, there is an explanation for both cs and cb being negative.
--  Case for cs being equal to 0.0 was tested with Nuke. For a channel with
--  all zeroed values it produced a zero valued channel, while for channel
--  having a value of 0.00001, it divided as expected, producing huge values
--  in the result.
divideBySrc :: (A.Elt a, A.IsFloating a) => BlendMode a
divideBySrc overlay background = (background A.==* 0.0 A.||* (overlay A.<* 0.0 A.&&* background A.<* 0.0)) A.? (0, overlay / background)

-- | B / A
-- See docs for @divideBySrc
divideByDst :: (A.Elt a, A.IsFloating a) => BlendMode a
divideByDst = flip divideBySrc

-- | A + B - 2AB
exclusion :: (A.Elt a, A.IsFloating a) => BlendMode a
exclusion overlay background = overlay + background - 2 * overlay * background

-- | B - A
from :: (A.Elt a, A.IsFloating a) => BlendMode a
from overlay background = background - overlay

-- | 2AB / (A+B)
geometric :: (A.Elt a, A.IsFloating a) => BlendMode a
geometric overlay background = 2 * overlay * background / (overlay + background)

-- | if A <= 0.5 then 2 * @multiply else 1 - 2*(1-A)*(1-B)
-- Nuke version: if A < 0.5 then multiply else screen - it's an error in docs.
-- In reality, Nuke multiplies by 2 just like Imagemagick.
--
-- Nuke says the condition is A < 0.5, Imagemagick says it's A <= 0.5
-- Since we have no idea why 0.5 would be special, we use Imagemagick version.
--
-- TODO[mm]: It seems that Nuke takes a max of 1 and result - investigate further.
hardLight :: (A.Elt a, A.IsFloating a) => BlendMode a
hardLight overlay background =
    (overlay A.<=* 0.5) A.?
        (2 * multiply overlay background,
        U.invert $ 2 * U.invert overlay * U.invert background)

-- | sqrt(A^2 + B^2)
hypot :: (A.Elt a, A.IsFloating a) => BlendMode a
hypot overlay background = sqrt $ overlay ** 2 + background ** 2

-- | A*b
inBlend :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
inBlend overlay _ _ alphaBackground = overlay * alphaBackground

-- | B*a
withMask :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
withMask _ alphaOverlay background _ = background * alphaOverlay

-- | A*a + B(1-a)
matte :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
matte overlay alphaOverlay background _ = overlay * alphaOverlay + background * U.invert alphaOverlay

-- | max(A,B)
max :: (A.Elt a, A.IsFloating a) => BlendMode a
max = P.max

-- | min(A,B)
min :: (A.Elt a, A.IsFloating a) => BlendMode a
min = P.min

-- | A - B
minus :: (A.Elt a, A.IsFloating a) => BlendMode a
minus = (-)

-- | A*B, A if A < 0 and B < 0
-- Nuke says that when A and B are negative, we should return left value to prevent
-- creating a positive value.
multiply :: (A.Elt a, A.IsFloating a) => BlendMode a
multiply overlay background = (overlay A.<* 0.0 A.&&* background A.<* 0.0) A.? (overlay, overlay * background)

-- | A(1-b)
out :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
out overlay _ _ alphaBackground = overlay * U.invert alphaBackground

-- | A + B(1-a)
over :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
over overlay alphaOverlay background _ = overlay + background * U.invert alphaOverlay

-- | A + B
plus :: (A.Elt a, A.IsFloating a) => BlendMode a
plus = (+)

-- | A + B - A * B
screen :: (A.Elt a, A.IsFloating a) => BlendMode a
screen = union

-- | if A <= 0.5 then 1 - 2*(1-A)*(1-B) else 2 * @multiply
-- See @hardLight
overlayFun :: (A.Elt a, A.IsFloating a) => BlendMode a
overlayFun = flip hardLight

-- | if A <= 0.5 then B - (1 - 2*A) * B * (1-B) else B + (2 * A - 1) * (d(B) - B)
-- where d(B) = if B <= 0.25 then ((16 * B - 12) * B + 4) * B else sqrt(B)
-- Formula from W3C Compositing and Blending
softLight :: (A.Elt a, A.IsFloating a) => BlendMode a
softLight overlay background =
    (overlay A.<=* 0.5) A.?
        (background - U.invert (2 * overlay) * background * U.invert background
        , background + (2 * overlay - 1) * (d background - background))
    where d x = (x A.<=* 0.25) A.? (((16 * x - 12) * x + 4) * x
                                  , sqrt x)

-- | 2AB + B^2 - 2 * B^2 * A
softLightPegtop :: (A.Elt a, A.IsFloating a) => BlendMode a
softLightPegtop overlay background =
    2 * overlay * background + background ** 2 - 2 * (background ** 2) * overlay

-- | B^(2^(2 * (0.5 - A)))
softLightIllusions :: (A.Elt a, A.IsFloating a) => BlendMode a
softLightIllusions overlay background =
    background ** (2 ** (2 * (0.5 - overlay)))

-- | if A <= 0.5 then 2 * A * B + B^2 * (1 - 2 * A) else sqrt(B) * (2 * A - 1) + 2 * B * (1-A)
softLightPhotoshop :: (A.Elt a, A.IsFloating a) => BlendMode a
softLightPhotoshop overlay background =
    (overlay A.<=* 0.5) A.?
        (2 * background * overlay + background ** 2 * U.invert (2 * overlay)
        , sqrt background * (2 * overlay - 1) + 2 * background * U.invert overlay)

-- | B(1-a)
stencil :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
stencil _ alphaOverlay background _ = background * U.invert alphaOverlay

-- | A(1-b) + B
under :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
under overlay _ background alphaBackground = overlay * U.invert alphaBackground + background

-- | A(1-b) + B(1-a)
xor :: (A.Elt a, A.IsFloating a) => ComplicatedBlendMode a
xor overlay alphaOverlay background alphaBackground =
    overlay * U.invert alphaBackground + background * U.invert alphaOverlay
