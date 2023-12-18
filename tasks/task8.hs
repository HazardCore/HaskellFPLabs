import Data.Bits
import Codec.Picture
import Codec.Picture.Gif

embedInLSB :: Pixel8 -> Bool -> Pixel8
embedInLSB colorValue bitFlag = if bitFlag then colorValue `setBit` 0 else colorValue `clearBit` 0

processAndAlterGif :: FilePath -> IO (Either String DynamicImage)
processAndAlterGif filePath = readGifImage filePath >>= \case
  Left error -> return $ Left error
  Right image -> return $ Right (alterImageContent image)

alterImageContent :: DynamicImage -> DynamicImage
alterImageContent (ImageRGB8 image) = ImageRGB8 $ pixelMap alterPixel image
  where
    alterPixel (PixelRGB8 red green blue) = PixelRGB8 (embedInLSB red signal1) (embedInLSB green signal2) (embedInLSB blue signal3)
    (signal1, temp1) = signal1 `divMod` 2
    (signal2, temp2) = signal2 `divMod` 2
    (signal3, _) = signal3 `divMod` 2
alterImageContent image = image 

exportModifiedGif :: FilePath -> DynamicImage -> IO ()
exportModifiedGif filePath image = writeGif filePath [image]