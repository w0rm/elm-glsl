{-# LANGUAGE OverloadedStrings #-}
module NewTests (parsingTests) where

import Test.HUnit (Test(..), assert)
import qualified Text.PrettyPrint.HughesPJClass as TPH
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

import qualified Parse.Helpers as PH
import qualified Reporting.Annotation as RA
import qualified Reporting.Error.Syntax as RE
import qualified Reporting.Report as RR

import qualified Language.GLSL.NewParser as LGNP
import qualified Language.GLSL.Syntax as LGS
import Language.GLSL.Pretty ()

parsingTests :: [Test]
parsingTests =
  [ sampleFileTest
  ]

parsePrettyId :: LGS.TranslationUnit -> Bool
parsePrettyId e = case pass LGNP.translationUnit (TPH.prettyShow e) of
  Left _ -> False
  Right e' -> e == e'

-- Just check if the parser passes of fails
pass :: PH.Parser a -> String -> Either (RA.Located RE.Error) a
pass p str =
  let
    parser = do
      PH.spaces
      r <- p
      PH.endOfFile
      return r
  in
    PH.run parser $ Text.pack str

sampleFileTest :: Test
sampleFileTest = TestLabel "Parse/Pretty glsl/sample-01.glsl test" . TestCase . assert $ do
  content <- TIO.readFile "glsl/sample-01.glsl"
  case LGNP.parse content of
    Left err -> do
      putStrLn $ "parse error: \n" ++ showError err content
      return False
    Right ast ->
      return $ parsePrettyId ast

showError :: RA.Located RE.Error -> Text.Text -> String
showError (RA.A region err) source =
  show $ RR.toDoc "<location>" region (RE.toReport Map.empty err) source
