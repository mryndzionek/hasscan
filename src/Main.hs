{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module Main where

import GHC.Word
import GHC.Int
import Data.Proxy
import Data.List ( sortBy )
import Control.Monad.Trans
import Control.Lens
import OpenCV
import qualified OpenCV.Juicy as CVJ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified Data.Vector as V
import Linear.V2
import Linear.V4 ( V4(..) )
import qualified Linear.Metric as L (norm, distance)

import System.Environment
import Filesystem.Path (splitExtension)
import Filesystem.Path.CurrentOS

import Foreign.C.Types (CFloat)
import qualified Vision.Image.Threshold as Vit
import qualified Vision.Image.JuicyPixels as Vj
import Codec.Picture.Saving as Js
import Codec.Picture.Types as Jt

black = pure 0 :: V4 Double
white = pure 255 :: V4 Double
green = V4 0 255 0 0 :: V4 Double
red   = V4 0 0 255 0 :: V4 Double
defaultBorder = BorderConstant (toScalar black)

saveImage fname src = do
  let b = exceptError $ imencode (OutputPng defaultPngParams) src
  B.writeFile fname b

resizeToHeight h' src = pureExcept $ resize (ResizeAbs ns) InterArea src
  where
    [h, w] = miShape $ matInfo src
    ns = toSize $ V2 ((w * h') `div` h) h'

contourAreas c1 c2 = exceptError $ do
  c1' <- contourArea (V.map fromItoF c1) ContourAreaAbsoluteValue
  c2' <- contourArea (V.map fromItoF c2) ContourAreaAbsoluteValue
  return $ c2' `compare` c1'
  where
    fromItoF p = fmap fromIntegral (fromPoint p :: V2 Int32)

drawCandidates contours src = pureExcept $
  withMatM (h ::: w ::: Z) (Proxy :: Proxy 3) (Proxy :: Proxy Word8) black $ \imgM -> do
    let candidates = V.fromList $ takeWhile ((==4) . length) contours
        rejected = V.fromList $ drop (length candidates) contours
        contour = V.singleton $ V.head candidates
    matCopyToM imgM (V2 0 0) src Nothing
    drawC (V.init candidates) white 1 imgM
    drawC rejected red 1 imgM
    drawC contour green 2 imgM
    where
      info = matInfo src
      [h, w] = miShape info
      drawC c cl t i = lift $ drawContours c cl (OutlineContour LineType_8 t) i

orderPoints pts = let l = V.toList pts
                      srted = sortBy (\p1 p2 -> (p1 ^. _x) `compare` (p2 ^. _x)) l
                      [tl, bl] = sortBy (\p1 p2 -> (p1 ^. _y) `compare` (p2 ^. _y)) $ take 2 srted
                      rm = take 2 (reverse srted)
                      [tr, br] = map snd $ take 2 $ sortBy compareDistance $ zip (repeat tl) rm
                      compareDistance (a, b) (c, d) =
                        let d1 = L.norm (a - b)
                            d2 = L.norm (c - d)
                        in d1 `compare` d2
                  in [tl, tr, br, bl]

fourPointTransform pts src = let src_ = orderPoints pts
                                 wa = L.distance (src_ !! 2) (src_ !! 3)
                                 wb = L.distance (src_ !! 1) (head src_)
                                 mw = max wa wb
                                 ha = L.distance (src_ !! 1) (src_ !! 2)
                                 hb = L.distance (head src_) (src_ !! 3)
                                 mh = max ha hb
                                 dst_ = [V2 0 0, V2 (mw - 1) 0, V2 (mw - 1) (mh - 1), V2 0 (mh - 1)]
                                 pers = getPerspectiveTransform (V.fromList src_) (V.fromList dst_)
                                 subRect = toRect $ HRect (V2 0 0) (V2 (round mw) (round mh))
                             in warpPerspective src pers InterLinear False False defaultBorder >>= flip matSubRect subRect

-- ugly workaround for functions missing in OpenCV bindings
withFridayGrey f src = pureExcept $ coerceMat $ (imdecode ImreadUnchanged . Bl.toStrict . 
                   Js.imageToPng . Jt.ImageY8 . Vj.toJuicyGrey . f . 
                   Vj.toFridayGrey . CVJ.toImage) src :: CvExcept (Mat ('S ' ['D, 'D]) ('S 1) ('S Word8))

process fname = do
  let  morpho se shape s = morphologyEx s shape se (Nothing :: Maybe Point2i) 1 defaultBorder
       name = encodeString . fst . splitExtension $ decodeString fname
  img :: Mat ('S '[ 'D, 'D]) 'D ('S Word8)
         <- imdecode ImreadUnchanged <$> lift (B.readFile fname) >>= pureExcept . coerceMat
  resized <- resizeToHeight 500 img
  edged <- pureExcept $ do
    strElem :: Mat 'D 'D 'D <- getStructuringElement MorphEllipse (Proxy :: Proxy 6) (Proxy :: Proxy 6) >>= coerceMat
    cvtColor bgr gray  resized >>=
      morpho strElem MorphOpen >>=
      morpho strElem MorphClose >>=
      gaussianBlur (7 :: V2 Int32) 0 0 >>=
      canny 75 100 Nothing CannyNormL1
  contours <- thaw edged >>= findContours ContourRetrievalList ContourApproximationSimple
  let ratio = ((fromIntegral . head . miShape . matInfo) img / 500) :: CFloat
      approx = sortBy contourAreas $ V.toList $ V.map (toPoly . contourPoints) contours
      candidates = V.fromList $ filter ((==4) . length) approx
      contour = V.map (fmap ((ratio *) . fromIntegral) . (\ p -> fromPoint p :: V2 Int32)) (V.head candidates)
  warped <- pureExcept $ fourPointTransform contour img
  res :: Mat ('S '[ 'D, 'D]) ('S 3) ('S Word8) <- pureExcept $ coerceMat resized
  outline <- drawCandidates approx res
  lift $ saveImage (name ++ "_edged.png") edged
  lift $ saveImage (name ++ "_outline.png") outline
  lift $ saveImage (name ++ "_warped.png") warped
  scanned <- pureExcept $ do
    strElem :: Mat 'D 'D 'D <- getStructuringElement MorphEllipse (Proxy :: Proxy 1) (Proxy :: Proxy 1) >>= coerceMat
    gr <- cvtColor bgr gray warped
    thr <- withFridayGrey adaptiveMean gr
    otsu <- withFridayGrey (Vit.otsu bintres) gr
    closed <- morpho strElem MorphOpen thr >>= morpho strElem MorphClose >>=
              gaussianBlur (3 :: V2 Int32) 0 0 >>= coerceMat
    bitwiseOr otsu closed
  lift $ saveImage (name ++ "_scanned.png") scanned
  where
    adaptiveMean = Vit.adaptiveThreshold
                       (Vit.MeanKernel :: Vit.AdaptiveThresholdKernel (Const Int Int)) 5 60 bintres
    bintres = Vit.BinaryThreshold 0 255
    toPoly p = exceptError $ do
      peri <- arcLength p True
      return $ approxPolyDP p (0.02 * peri) True

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fname:_) -> exceptErrorIO $ process fname
    _ -> putStrLn "First argument must be a path to an image"        
