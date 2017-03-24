{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module SrcLoc where

import AlexTools


data SrcLoc = Known {-# UNPACK #-} !SourceRange
            | Unknown
              deriving (Eq,Show)

instance Monoid SrcLoc where
  mempty = Unknown
  {-# INLINE mempty #-}

  mappend (Known a) (Known b) = Known (a <-> b)
  mappend a         Unknown   = a
  mappend Unknown   b         = b


class HasSrcLoc a where
  srcLoc :: a -> SrcLoc

instance HasSrcLoc SrcLoc where
  srcLoc = id
  {-# INLINE srcLoc #-}

instance HasSrcLoc SourceRange where
  srcLoc = Known
  {-# INLINE srcLoc #-}

instance HasSrcLoc a => HasSrcLoc (Maybe a) where
  srcLoc = maybe mempty srcLoc
  {-# INLINE srcLoc #-}

instance HasSrcLoc a => HasSrcLoc [a] where
  srcLoc = foldMap srcLoc
  {-# INLINE srcLoc #-}

instance (HasSrcLoc a, HasSrcLoc b) => HasSrcLoc (a,b) where
  srcLoc (a,b) = mconcat [srcLoc a, srcLoc b]
  {-# INLINE srcLoc #-}

instance (HasSrcLoc a, HasSrcLoc b, HasSrcLoc c) => HasSrcLoc (a,b,c) where
  srcLoc (a,b,c) = mconcat [srcLoc a, srcLoc b, srcLoc c]
  {-# INLINE srcLoc #-}

instance (HasSrcLoc a, HasSrcLoc b, HasSrcLoc c, HasSrcLoc d) => HasSrcLoc (a,b,c,d) where
  srcLoc (a,b,c,d) = mconcat [srcLoc a, srcLoc b, srcLoc c, srcLoc d]
  {-# INLINE srcLoc #-}

instance (HasSrcLoc a, HasSrcLoc b, HasSrcLoc c, HasSrcLoc d, HasSrcLoc e) =>
  HasSrcLoc (a,b,c,d,e) where
  srcLoc (a,b,c,d,e) = mconcat [srcLoc a, srcLoc b, srcLoc c, srcLoc d, srcLoc e]
  {-# INLINE srcLoc #-}
