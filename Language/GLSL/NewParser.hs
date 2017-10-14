module Language.GLSL.NewParser (parse, translationUnit) where

import Data.Text (Text)

import qualified Parse.Helpers as PH
import qualified Reporting.Annotation as RA
import qualified Reporting.Error.Syntax as RE

import qualified Language.GLSL.Syntax as LGS

parse :: Text -> Either (RA.Located RE.Error) LGS.TranslationUnit
parse = PH.run translationUnit

translationUnit :: PH.Parser LGS.TranslationUnit
translationUnit = undefined
