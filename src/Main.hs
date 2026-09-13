-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import           Miso.Mathml
import qualified Miso.Html.Element    as H
import           Miso.Html.Event      (onInput)
import qualified Miso.Html.Property   as P
import           Miso.String          (MisoString, ms, fromMisoString)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Model = Model
  { _qa      :: Int -- ^ quadratic: a
  , _qb      :: Int -- ^ quadratic: b
  , _qc      :: Int -- ^ quadratic: c
  , _binN    :: Int -- ^ binomial exponent
  , _cfDepth :: Int -- ^ continued fraction depth
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
qa, qb, qc, binN, cfDepth :: Lens Model Int
qa      = lens _qa      $ \m x -> m { _qa      = x }
qb      = lens _qb      $ \m x -> m { _qb      = x }
qc      = lens _qc      $ \m x -> m { _qc      = x }
binN    = lens _binN    $ \m x -> m { _binN    = x }
cfDepth = lens _cfDepth $ \m x -> m { _cfDepth = x }
-----------------------------------------------------------------------------
data Action
  = SetA Int
  | SetB Int
  | SetC Int
  | SetN Int
  | SetDepth Int
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model 1 (-3) (-4) 4 6
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel updateModel viewModel)
#ifndef WASM
  { styles = [ Href "assets/style.css" ]
  }
#endif
-----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  SetA n     -> qa .= if n == 0 then 1 else n
  SetB n     -> qb .= n
  SetC n     -> qc .= n
  SetN n     -> binN .= n
  SetDepth n -> cfDepth .= n
-----------------------------------------------------------------------------
viewModel :: Model -> View () () Model Action
viewModel m =
  H.div_
  [ P.class_ "app" ]
  [ H.header_
    [ P.class_ "hero" ]
    [ H.h1_ [] [ "🍜 ➕ ", H.a_ [ P.href_ repoUrl ] [ "miso-mathml" ] ]
    , H.p_ [ P.class_ "tagline" ]
      [ "Native browser MathML, generated from pure Haskell views. "
      , "Drag the sliders — every equation below is live."
      ]
    , H.a_ [ P.class_ "gh", P.href_ repoUrl ] [ "View source on GitHub" ]
    ]
  , H.main_
    [ P.class_ "grid" ]
    [ quadraticCard m
    , binomialCard m
    , goldenCard m
    ]
  , H.footer_
    [ P.class_ "foot" ]
    [ H.p_ []
      [ "Built with "
      , H.a_ [ P.href_ "https://github.com/dmjio/miso" ] [ "miso" ]
      , ", a Haskell web framework — compiled to WebAssembly. "
      , "Rendered with your browser's native "
      , H.a_ [ P.href_ "https://developer.mozilla.org/en-US/docs/Web/MathML" ] [ "MathML" ]
      , " engine, no math typesetting library involved."
      ]
    ]
  ]
  where
    repoUrl = "https://github.com/haskell-miso/miso-mathml"
-----------------------------------------------------------------------------
-- * Quadratic explorer
-----------------------------------------------------------------------------
quadraticCard :: Model -> View () () Model Action
quadraticCard m =
  H.section_
  [ P.class_ "card" ]
  [ H.h2_ [] [ "Quadratic equations, solved live" ]
  , H.p_ [ P.class_ "hint" ]
    [ "Pick coefficients for ax² + bx + c = 0; the roots are computed in "
    , "Haskell and typeset as MathML."
    ]
  , mathBlock
    [ mrow_ []
      ( coeffTerms a b c
        ++ [ mo_ [] [ "=" ], mn_ [] [ "0" ] ]
      )
    ]
  , mathBlock
    [ mi_ [] [ "x" ]
    , mo_ [] [ "=" ]
    , mfrac_ []
      [ mrow_ []
        [ mrow_ [] (mnum (negate b))
        , mo_ [] [ "±" ]
        , msqrt_ [] [ mn_ [] [ text (ms disc) ] ]
        ]
      , mn_ [] [ text (ms (2 * a)) ]
      ]
    ]
  , H.p_ [ P.class_ "result" ] (rootsText a b c)
  , H.div_
    [ P.class_ "controls" ]
    [ slider "a" (-5) 5 a SetA
    , slider "b" (-9) 9 b SetB
    , slider "c" (-9) 9 c SetC
    ]
  ]
  where
    a = m ^. qa
    b = m ^. qb
    c = m ^. qc
    disc = b * b - 4 * a * c
-----------------------------------------------------------------------------
-- | Render @ax² + bx + c@ with conventional signs (drop 1·, fold + −5 into − 5).
coeffTerms :: Int -> Int -> Int -> [View () () Model Action]
coeffTerms a b c =
  concat
  [ lead a (msup_ [] [ mi_ [] [ "x" ], mn_ [] [ "2" ] ])
  , follow b (Just (mi_ [] [ "x" ]))
  , follow c Nothing
  ]
  where
    lead k v
      | k == 1     = [ v ]
      | k == -1    = [ mo_ [] [ "−" ], v ]
      | otherwise  = mnum k ++ [ v ]
    follow 0 _ = []
    follow k mv =
      [ mo_ [] [ if k > 0 then "+" else "−" ] ]
      ++ (case (abs k, mv) of
            (1, Just v)  -> [ v ]
            (n, Just v)  -> [ mn_ [] [ text (ms n) ], v ]
            (n, Nothing) -> [ mn_ [] [ text (ms n) ] ])
-----------------------------------------------------------------------------
-- | A (possibly negative) integer literal in MathML.
mnum :: Int -> [View () () Model Action]
mnum n
  | n < 0     = [ mo_ [] [ "−" ], mn_ [] [ text (ms (abs n)) ] ]
  | otherwise = [ mn_ [] [ text (ms n) ] ]
-----------------------------------------------------------------------------
rootsText :: Int -> Int -> Int -> [View () () Model Action]
rootsText a b c
  | disc > 0 =
      [ "Two real roots: x₁ = ", em (dec r1), ", x₂ = ", em (dec r2) ]
  | disc == 0 =
      [ "One double root: x = ", em (dec r1) ]
  | otherwise =
      [ "Complex conjugate roots: x = "
      , em (dec re <> " ± " <> dec im <> "i")
      ]
  where
    disc = fromIntegral (b * b - 4 * a * c) :: Double
    a' = fromIntegral a; b' = fromIntegral b
    r1 = (-b' + sqrt disc) / (2 * a')
    r2 = (-b' - sqrt disc) / (2 * a')
    re = -b' / (2 * a')
    im = sqrt (abs disc) / (2 * abs a')
    em t = H.strong_ [] [ text t ]
-----------------------------------------------------------------------------
-- * Binomial theorem
-----------------------------------------------------------------------------
binomialCard :: Model -> View () () Model Action
binomialCard m =
  H.section_
  [ P.class_ "card" ]
  [ H.h2_ [] [ "The binomial theorem" ]
  , H.p_ [ P.class_ "hint" ]
    [ "The expansion of (a + b)ⁿ — coefficients are Pascal's triangle, "
    , "computed recursively and rendered term by term."
    ]
  , mathBlock
    ( [ msup_ []
        [ mrow_ []
          [ mo_ [] [ "(" ]
          , mi_ [] [ "a" ]
          , mo_ [] [ "+" ]
          , mi_ [] [ "b" ]
          , mo_ [] [ ")" ]
          ]
        , mn_ [] [ text (ms n) ]
        ]
      , mo_ [] [ "=" ]
      ]
      ++ expansion
    )
  , H.div_
    [ P.class_ "controls" ]
    [ slider "n" 0 8 n SetN ]
  ]
  where
    n = m ^. binN
    expansion = concat
      [ (if k > 0 then [ mo_ [] [ "+" ] ] else [])
        ++ term (choose n k) (n - k) k
      | k <- [ 0 .. n ]
      ]
    term coeff pa pb =
      concat
      [ [ mn_ [] [ text (ms coeff) ] | coeff /= 1 || (pa == 0 && pb == 0) ]
      , varPow "a" pa
      , varPow "b" pb
      ]
    varPow v p
      | p == 0    = []
      | p == 1    = [ mi_ [] [ v ] ]
      | otherwise = [ msup_ [] [ mi_ [] [ v ], mn_ [] [ text (ms p) ] ] ]
-----------------------------------------------------------------------------
choose :: Int -> Int -> Int
choose n k = foldl (\acc i -> acc * (n - i + 1) `div` i) 1 [ 1 .. k ]
-----------------------------------------------------------------------------
-- * Golden ratio continued fraction
-----------------------------------------------------------------------------
goldenCard :: Model -> View () () Model Action
goldenCard m =
  H.section_
  [ P.class_ "card" ]
  [ H.h2_ [] [ "The golden ratio, unfolded" ]
  , H.p_ [ P.class_ "hint" ]
    [ "φ = [1; 1, 1, 1, …] — the slowest-converging continued fraction. "
    , "The nesting below is generated by a recursive Haskell function."
    ]
  , mathBlock
    [ mi_ [] [ "φ" ]
    , mo_ [] [ "≈" ]
    , continued depth
    ]
  , H.p_ [ P.class_ "result" ]
    [ "Convergent: "
    , H.strong_ [] [ text (ms (fib (depth + 2)) <> " / " <> ms (fib (depth + 1))) ]
    , " ≈ "
    , H.strong_ [] [ text (dec approx) ]
    , "   (φ ≈ 1.618034…)"
    ]
  , H.div_
    [ P.class_ "controls" ]
    [ slider "depth" 1 10 depth SetDepth ]
  ]
  where
    depth = m ^. cfDepth
    approx = fromIntegral (fib (depth + 2)) / fromIntegral (fib (depth + 1)) :: Double
    continued :: Int -> View () () Model Action
    continued 0 = mn_ [] [ "1" ]
    continued k =
      mrow_ []
      [ mn_ [] [ "1" ]
      , mo_ [] [ "+" ]
      , mfrac_ [] [ mn_ [] [ "1" ], continued (k - 1) ]
      ]
-----------------------------------------------------------------------------
fib :: Int -> Int
fib n = go n 0 1 where
  go 0 a _ = a
  go k a b = go (k - 1) b (a + b)
-----------------------------------------------------------------------------
-- * Shared helpers
-----------------------------------------------------------------------------
mathBlock :: [View () () Model Action] -> View () () Model Action
mathBlock children =
  H.div_
  [ P.class_ "math-wrap" ]
  [ math_
    [ display_ "block"
    , P.xmlns_ "http://www.w3.org/1998/Math/MathML"
    ]
    children
  ]
-----------------------------------------------------------------------------
slider :: MisoString -> Int -> Int -> Int -> (Int -> Action) -> View () () Model Action
slider label lo hi val toAction =
  H.label_
  [ P.class_ "slider" ]
  [ H.span_ [ P.class_ "slider-name" ] [ text label ]
  , H.input_
    [ P.type_ "range"
    , P.min_ (ms lo)
    , P.max_ (ms hi)
    , P.step_ "1"
    , P.value_ (ms val)
    , onInput (toAction . fromMisoString)
    ]
  , H.span_ [ P.class_ "slider-val" ] [ text (ms val) ]
  ]
-----------------------------------------------------------------------------
-- | Two decimal places.
dec :: Double -> MisoString
dec x = ms (fromIntegral (round (x * 100) :: Int) / 100 :: Double)
-----------------------------------------------------------------------------
