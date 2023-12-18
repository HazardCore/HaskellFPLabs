import Text.XML
import Text.XML.Writer
import qualified Data.Text.Lazy.IO as TLIO
import Control.Monad (replicateM_)

-- main
main :: IO ()
main = do
  let xmlDocs = replicate 5 $ Document (Prologue [] Nothing []) rootNode []

  mapM_ (\i -> TLIO.writeFile ("example" ++ show i ++ ".xml") $ renderText def (xmlDocs !! i)) [1..5]

  let docTypeDefinitions = map (\i -> DTD
        [ DTDElement ("root" ++ show i) (DTDEmpty
                              (Just $ DTDEntity "data" "CDATA" Nothing)
                              (Just $ DTDEntity "attr" "CDATA" Nothing))
        ]) [1..5]

  mapM_ (\(i, dtd) -> TLIO.writeFile ("example" ++ show i ++ ".dtd") $ renderText def dtd) (zip [1..5] docTypeDefinitions)

rootNode :: Element
rootNode =
  Element
    "root"
    []
    (concatMap (\i -> [ NodeElement $ Element ("data" ++ show i) [] [NodeContent ("Hey ho! " ++ show i)]
                    , NodeElement $ Element ("attr" ++ show i) [("name", "example" ++ show i)] [] ]) [1..5])
