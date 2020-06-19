{-# LANGUAGE PolyKinds #-}

module My.Data.Proxy
  ( Proxy (..),
  )
where

data Proxy a = Proxy
