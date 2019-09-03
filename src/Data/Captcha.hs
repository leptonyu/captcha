{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Captcha(
    newCaptcha
  , CaptchaConfig(..)
  , testCaptcha
  ) where

import           Codec.Picture
import           Control.Monad.State
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Base64              as B64
import           Data.ByteString.Lazy                (toStrict)
import           Data.Default
import           Data.Word
import           FileEmbedLzma
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture
import           Graphics.Rasterific.Transformations
import           Graphics.Text.TrueType
import           System.Random.SplitMix

testCaptcha = do
  sm <- initSMGen
  let (im, _) = newImage def sm
  writePng "captcha.png" im

newCaptcha :: IO (Word64, ByteString)
newCaptcha = do
  sm <- initSMGen
  let (im, s2) = newImage def sm
  case encodeDynamicPng (ImageRGBA8 im) of
    Left  e -> error e
    Right x -> return (fst (nextWord64 s2), B64.encode $ toStrict x)

data CaptchaConfig = CaptchaConfig
  { bgColor :: PixelRGBA8
  , feColor :: PixelRGBA8
  , words   :: [Char]
  , fonts   :: Font
  , size    :: (Int, Int)
  , len     :: Int
  }

font :: Font
font = case decodeFont $(embedLazyByteString "comic.ttf") of
    Right f -> f
    Left  e -> error e

cBlack :: PixelRGBA8
cBlack = PixelRGBA8 0 0 0 255

cWhite :: PixelRGBA8
cWhite = PixelRGBA8 255 255 255 255

instance Default CaptchaConfig where
  def = CaptchaConfig cBlack cWhite (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) font (200,75) 4

newImage :: CaptchaConfig -> SMGen -> (Image PixelRGBA8, SMGen)
newImage cc@CaptchaConfig{..} = runState $ do
  l0 <- replicateM 5 $ newNoiseCircle cc
  l1 <- replicateM 20 $ newNoiseLine cc
  l2 <- newChar cc
  return
    $ uncurry renderDrawing size feColor
    $ withTexture (uniformTexture bgColor)
    $ sequence_
    $ l2 ++ l1 ++ l0

nextF :: Monad m => Float -> Int -> StateT SMGen m Float
nextF diff v = do
  seed <- get
  let (a, seed') = nextFloat seed
  put seed'
  return $ realToFrac v * (a - diff)

nextI :: Monad m => Int -> StateT SMGen m Int
nextI v = do
  seed <- get
  let (a, seed') = nextInt seed
  put seed'
  return (a `mod` v)

newNoiseCircle :: CaptchaConfig -> State SMGen (Drawing px ())
newNoiseCircle CaptchaConfig{..} = do
  a <- nextF 0 (fst size)
  b <- nextF 0 (snd size)
  c <- nextF 0 (snd size `div` 10)
  return $ fill $ circle (V2 a b) c

newNoiseLine :: CaptchaConfig -> State SMGen (Drawing px ())
newNoiseLine CaptchaConfig{..} = do
  a <- nextF 0   (fst size)
  b <- nextF 0   (snd size)
  w <- nextF 0.5 (fst size)
  h <- nextF 0.5 (fst size `div` 10)
  return $ fill $ line (V2 a b) (V2 (a + w) (b + h))

newChar :: CaptchaConfig -> State SMGen [Drawing px ()]
newChar CaptchaConfig{..} = do
  let fsize = (realToFrac $ snd size) * 0.6
      ftop  = (realToFrac $ snd size) * 0.8
      fleft = (realToFrac $ fst size) * 0.07
  mapM (go fsize ftop fleft) [1..len]
  where
    {-# INLINE go #-}
    go fsize ftop fleft i = do
      c <- (words !!) <$> nextI (length words)
      r <- nextF 0.5 1
      let del = realToFrac (i-1) * fsize
      return
        $ withTransformation (rotateCenter (r/4) $ V2 (fleft + del - fsize/ 2) (ftop + fsize/2))
        $ printTextAt
          fonts
          (PointSize fsize)
          (V2 (fleft + del) ftop)
          [c]






