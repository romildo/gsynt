{-# LANGUAGE OverloadedStrings #-}

-- | Tree interface using the @forest@ package.
--   An example of usage is provided in the /examples/ directory of
--   the source distribution.
module Text.LaTeX.Packages.Trees.Forest (
  -- * Tree re-export
  module Text.LaTeX.Packages.Trees
  -- * Forest package
  , pforest
  -- * Tree to LaTeX rendering
  , tree
  , rendertree
  ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv))
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Trees (Tree(Leaf, Node))

import Data.List (intersperse)

-- | The 'forest' package.
pforest :: PackageName
pforest = "forest"

forest :: LaTeXC l => l -> l
forest = liftL $ TeXEnv "forest" []

tree_ :: LaTeXC l => (a -> l) -> Tree a -> l
tree_ f (Leaf x) = mconcat [ "[", braces $ f x, "]" ]
tree_ f (Node mx ts) =
  mconcat [ "["
          , maybe mempty (braces . f) mx
          , " "
          , mconcat $ intersperse " " $ fmap (tree_ f) ts
          , " ]"
          ]

-- | Given a function to @LaTeX@ values, you can create a @LaTeX@ tree from a
--   Haskell tree. The function specifies how to render the node values.
tree :: LaTeXC l => (a -> l) -> Tree a -> l
tree f t = forest $
              raw "where n children=0{tier=word}{},"
           <> raw "baseline=(current bounding box.north),"
           <> raw "nt/.style={draw={red,thick}},"
           -- <> raw "sn edges/.style={for tree={parent anchor=south,child anchor=north}},"
           -- <> raw "for tree={edge path={\\noexpand\\path[\\forestoption{edge}] (\\forestOve{\\forestove{@parent}}{name}.parent anchor) -- +(0,-14pt) -| (\\forestove{name}.child anchor)\\forestoption{edge label};}},"
           <> raw "\
                  \for tree={%\n\
                  \  % re-establishing the defaults:\n\
                  \  %child anchor=north,\n\
                  \  %parent anchor=south,\n\
                  \  edge path={\\noexpand\\path[\\forestoption{edge}] \n\
                  \    (\\forestOve{\\forestove{@parent}}{name}.parent anchor)\n\
                  \    -- ([shift={(0,-10pt)}]\\forestOve{\\forestove{@parent}}{name} -| \\forestove{name})\n\
                  \    -- (\\forestove{name}.child anchor)\n\
                  \    \\forestoption{edge label};\n\
                  \  }\n\
                  \},\n"
           -- <> raw "sn edges,"
           <> raw "nt"
           <> tree_ f t


-- -- | Instance defined in "Text.LaTeX.Packages.Trees.Forest".
-- instance Texy a => Texy (Tree a) where
--  texy = tree texy

-- | This function works as 'tree', but use 'render' as rendering function.
rendertree :: (Render a, LaTeXC l) => Tree a -> l
rendertree = tree (raw . protectText . render)
