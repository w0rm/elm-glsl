{-# LANGUAGE DoAndIfThenElse, OverloadedStrings #-}
module NewTests (parsingTests) where

import Test.HUnit (Test(..), assert, assertBool)
import qualified Text.PrettyPrint.HughesPJClass as TPH
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

import qualified Parse.Helpers as PH
import qualified Reporting.Annotation as RA
import qualified Reporting.Error.Syntax as RE
import qualified Reporting.Report as Report
import qualified Reporting.Region as Region

import qualified Language.GLSL.NewParser as LGNP
import qualified Language.GLSL.Primitives as P
import qualified Language.GLSL.Syntax as LGS
import Language.GLSL.Pretty ()

import qualified Language.GLSL.Parser as LGP
import qualified Tests

parsingTests :: [Test]
parsingTests =
  [ legalExpressionsTests
  , illegalExpressionsTests
  , compareExpressionsTests
  , legalDeclarationsTests
  , illegalDeclarationsTests
  , compareDeclarationsTests
  , legalFunctionDefinitionsTests
  , compareFunctionDefinitionsTests
  , legalCommentsTests
  , illegalCommentsTests
  , compareCommentsTests
  , legalNumberTests

  , TestLabel "expressions id" $ TestList $
    map expressionsId testExpressionsTrue
  , TestLabel "declarations id" $ TestList $
    map declarationsId testDeclarationsTrue
  , TestLabel "function definitions id" $ TestList $
    map functionDefinitionsId testFunctionDefinitionsTrue

  , sampleFileTest
  ]

parsePrettyId :: LGS.TranslationUnit -> IO Bool
parsePrettyId e =
  let
    source = TPH.prettyShow e
  in
    case pass LGNP.translationUnit source of
      Left err -> do
        putStrLn $ showError err $ Text.pack source
        return False
      Right e' ->
        return $ e == e'

testId :: (TPH.Pretty a, Eq a) => String -> PH.Parser a -> String -> Test
testId name parser str =
  TestLabel (name ++ ": " ++ str) . TestCase . assert $ do
    ast <- case pass parser str of
      Right a ->
        return a
      Left err -> do
        putStrLn $ showError err (Text.pack str)
        error "does not even parse the original string"
    let source = TPH.prettyShow ast
    case pass parser source of
      Left err -> do
        putStrLn $ showError err (Text.pack source)
        return False
      Right e ->
        return $ ast == e

expressionsId :: String -> Test
expressionsId =
  testId "expressionsId" LGNP.expression

declarationsId :: String -> Test
declarationsId =
  testId "declarationsId" LGNP.declaration

functionDefinitionsId :: String -> Test
functionDefinitionsId =
  testId "functionDefinitionsId" LGNP.functionDefinition

-- Just check if the parser passes of fails
pass :: PH.Parser a -> String -> Either (RA.Located RE.Error) a
pass p str =
  let
    parser = do
      P.whitespace
      r <- p
      P.whitespace
      PH.endOfFile
      return r
  in
    PH.run parser $ Text.pack str

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool
isLeft = not . isRight

doesParse :: PH.Parser a -> String -> Test
doesParse p str =
  TestLabel ("doesParse: " ++ str) . TestCase . assert $
    case pass p str of
      Right _ ->
        return True
      Left err -> do
        putStrLn $ showError err $ Text.pack str
        return False

doesNotParse :: PH.Parser a -> String -> Test
doesNotParse p str =
  TestCase . assertBool ("doesNotParse: " ++ str) . isLeft . pass p $ str


compareParse :: (TPH.Pretty a, Eq a) => PH.Parser a -> LGP.P a -> String -> Test
compareParse newParser oldParser str =
  TestLabel ("compareParse: " ++ str) . TestCase . assert $ do
    newAST <- case pass newParser str of
      Right a ->
        return a
      Left err -> do
        putStrLn $ showError err $ Text.pack str
        error "New parser failed parsing"
    oldAST <- case Tests.pass oldParser str of
      Right a ->
        return a
      Left err -> do
        print err
        error "Old parser failed parsing"
    if newAST == oldAST then
      return True
    else do
      putStrLn "New parser's result:"
      putStrLn $ TPH.prettyShow newAST
      putStrLn "Old parser's result:"
      putStrLn $ TPH.prettyShow oldAST
      return False

sampleFileTest :: Test
sampleFileTest =
  TestLabel "Parse/Pretty glsl/sample-01.glsl test" . TestCase . assert $ do
    content <- TIO.readFile "glsl/sample-01.glsl"
    case LGNP.parse content of
      Left err -> do
        putStrLn $ "parse error: \n" ++ showError err content
        return False
      Right ast ->
        parsePrettyId ast

showError :: RA.Located RE.Error -> Text.Text -> String
showError (RA.A region err) source =
  show $ Report.toDoc (Region.toString region) region (RE.toReport Map.empty err) source

----------------------------------------------------------------------
-- expressions
----------------------------------------------------------------------

legalExpressionsTests :: Test
legalExpressionsTests = TestLabel "legal expressions" $
  TestList $ map (doesParse LGNP.expression) testExpressionsTrue

illegalExpressionsTests :: Test
illegalExpressionsTests = TestLabel "illegal expressions" $
  TestList $ map (doesNotParse LGNP.expression) testExpressionsFalse

compareExpressionsTests :: Test
compareExpressionsTests = TestLabel "compare expressions" $
  TestList $ map (compareParse LGNP.expression LGP.expression) testExpressionsTrue

testExpressionsTrue :: [String]
testExpressionsTrue =
  [ "a"
  , "avoid"
  , "filters"
  , "0"
  , "1"
  , ".1"
  , "0x01"
  , "0xF"
  , "07"
  , "a++"
  , "a--"
  , "a++--"
  , "a--++"
  , "a++++"
  , "a----"
  , "++a"
  , "--a"
  , "++--a"
  , "--++a"
  , "++++a"
  , "----a"
  , "a ++"
  , "+a"
  , "+ a"
  , "a + b"
  , "a + b++"
  , "a + ++b"
  , "a + + b"
  , "a ++ +b"
  , "a()"
  , "float()"
  , "a ()"
  , "a( )"
  , "a ( )"
  , "a.a"
  , "a.a()"
  , "a.length()"
  , "a[1]"
  , "a[1].x"
  , "a[1].x()"
  , "a().b"
  , "a().b[1]"
  , "a().b()"
  , "a.b.c"
  , "a ? 1 : 2"
  , "a?1:2"
  , "(a) ? (1) : (2)"
  , "a && b"
  , "a && b || !c && !!d"
  ]

testExpressionsFalse :: [String]
testExpressionsFalse =
  [ "void"
  , "filter"
  , ".A"
  , "08"
  , "0A"
  , "+"
  , "++"
  , "a+"
  , "a +"
  , "a . a" -- TODO should it be allowed ?
  , "a[]"
--  , "a[1][2]" -- TODO it is illegal to declare an array of arrays
  ]

----------------------------------------------------------------------
-- declarations
----------------------------------------------------------------------

legalDeclarationsTests :: Test
legalDeclarationsTests = TestLabel "legal declarations" $
  TestList $ map (doesParse LGNP.declaration) testDeclarationsTrue

illegalDeclarationsTests :: Test
illegalDeclarationsTests = TestLabel "illegal declarations" $
  TestList $ map (doesNotParse LGNP.declaration) testDeclarationsFalse

compareDeclarationsTests :: Test
compareDeclarationsTests = TestLabel "compare declarations" $
  TestList $ map (compareParse LGNP.declaration LGP.declaration) testDeclarationsTrue

testDeclarationsTrue :: [String]
testDeclarationsTrue =
  [ "int a;"
  , "int a, b, c;"
  , "precision highp float;"
  , "int a = 1;"
  , "struct a { int b; };"
  , "struct a { int b; float c; };"
  , "layout (origin_upper_left) in;"
  , "layout (origin_upper_left) in vec4 gl_FragCoord;"
  , "layout (origin_upper_left, pixel_center_integer) in vec4 gl_FragCoord;"
  , "bool success;"
  , "bool done = false;"
  , "int a = 123;"
  , "int a = 123u;"
  , "int a = 123U;"
  , "int a = 0123;"
  , "int a = 0123u;"
  , "int a = 0123U;"
  , "int a = 0x123;"
  , "int a = 0x123u;"
  , "int a = 0x123U;"
  , "int a = 0x12ABu;"
  , "int a = 0x12ABU;"
  , "int a = 0X123;"
  , "int a = 0X123u;"
  , "int a = 0X123U;"
  , "int a = 0X12ABu;"
  , "int a = 0X12ABU;"
  , "float a = 1.0;"
  , "float a = 1.;"
  , "float a = .0;"
  , "float a = 1e1;"
  , "float a = 1.0e1;"
  , "float a = 1.e1;"
  , "float a = 1.0e-1;"
  , "float a = 1.0e+1;"
  , "float a = 1.0e+1f;"
  , "float a = 1.0e+1F;"
  , "vec2 texcoord1, texcoord2;"
  , "mat3x2 m;"
  , "struct light { float intensity; vec3 position; } lightVar;"
  , "const struct light { float intensity; vec3 position; } lightVar;"
  , "float frequencies[3];"
  , "uniform vec4 lightPosition[4];"
  , "light lights[];"
  , "const int numLights = 2;"
  , "light lights[numLights];"
  , "float[5] a;"
  , "float a[5];"
  , "float a[5] = float[](3.4, 4.2, 5.0, 5.2, 1.1);"
  , "float a[5] = float[5](3.4, 4.2, 5.0, 5.2, 1.1);"
  , "const int a;"
  , "in int a;"
  , "centroid in int a;"
  , "smooth in int a;"

  , "bool success;"
  , "bool done = false;"
  , "int i;"
  , "int i, j = 42;"
  , "int j = 1;"
  , "uint k = 3u;"
  , "float x = 1.0;"
  , "float a = 1.5, b;"
  , "vec2 texcoord1, texcoord2;"
  , "vec3 position;"
  , "vec4 myRGBA;"
  , "ivec2 textureLookup;"
  , "bvec3 less;"
  , "mat2 mat2D;"
  , "mat3 optMatrix;"
  , "mat4 view;"
  , "mat3x2 m;"
  , "struct light\
    \{\
    \float intensity;\
    \vec3 position;\
    \} lightVar;"
  , "light lightVar3;"
  , "float frequencies[3];"
  , "uniform vec4 lightPosition[4];"
  , "light lights[];"
  , "const int numLights = 2;"
  , "light lights[numLights];"
  , "int a[5];"
  , "const float coef = 2.75;"
  , "const vec3 zAxis = vec3 (0.0, 0.0, 1.0);"
  , "in vec4 position;"
  , "in vec3 normal;"
  , "in vec2 texCoord[4];"
  , "in float foo[];"
  , "centroid out vec2 TexCoord;"
  , "invariant centroid out vec4 Color;"
  , "invariant flat centroid out vec4 Color;"
  , "noperspective out float temperature;"
  , "flat out vec3 myColor;"
  , "noperspective centroid out vec2 myTexCoord;"
  , "out vec4 FragmentColor;"
  , "out uint Luminosity;"
  , "uniform Transform\
    \{\
    \  mat4 ModelViewMatrix;\
    \  mat4 ModelViewProjectionMatrix;\
    \  uniform mat3 NormalMatrix;\
    \  float Deformation;\
    \};"
  , "in Material\
    \{\
    \  smooth in vec4 Color1;\
    \  smooth vec4 Color2;\
    \  vec2 TexCoord;\
    \};"
  , "out Vertex\
    \{\
    \  vec4 Position;\
    \  vec2 Texture;\
    \} Coords;"
  , "uniform Transform {\
    \  mat4 ModelViewMatrix;\
    \  mat4 ModelViewProjectionMatrix;\
    \  float Deformation;\
    \} transforms[4];"
  , "layout (triangles) in;"
  , "layout (origin_upper_left) in vec4 gl_FragCoord;"
  , "layout (pixel_center_integer) in vec4 gl_FragCoord;"
  , "layout (origin_upper_left, pixel_center_integer) in vec4 gl_FragCoord;"
  , "layout (triangle_strip, max_vertices = 60) out;"
  , "layout (triangle_strip) out;"
  , "layout (max_vertices = 60) out;"
  , "layout (shared, column_major) uniform;"
  , "layout (std140) uniform Transform {\
    \  mat4 M1;\
    \  layout (column_major) mat4 M2;\
    \  mat3 N1;\
    \};"
  , "flat out vec4 gl_FrontColor;"
  , "lowp float color;"
  , "out mediump vec2 P;"
  , "highp mat4 m;"
  , "precision highp float;"
  , "precision highp int;"
  , "precision mediump int;"
  , "invariant gl_Position;"
  , "out vec3 Color;"
  , "invariant Color;"
  , "invariant centroid out vec3 Color;"
  , "vec4 color = vec4(0.0, 1.0, 0.0, 1.0);"
  , "int i = 1 - 5 * 4 + 3;"
  , "int i = 1 - (5 * 4) + 3;"
  , "int i = (1 - 5) * 4 + 3;"
  , "int i = (1 - 5) * (4 + 3);"
  , "bool b = 1 < 2;"

  ]

testDeclarationsFalse :: [String]
testDeclarationsFalse =
  [ "int a"
  , "int a, b c;"
  , "precision high float;"
  , "precision float f();"
  , "int float;"
  , "struct a { };"
  , "struct a { int b, float c; };"
  , "int a = 0128;"
  , "int a = 0xu;"
  , "float a = .e1;"
  , "float a = 1.0e+1G;"
{- TODO embeded structs are not possible.
  , "struct light {\
    \  struct { float intensity; };\
    \  vec3 position;\
    \} lightVar;"
-}
  , "int f();" -- function declaration should be at top level -- TODO put these in legalTranslationUnitsTests
  , "int f(void);"
  , "int f ( void ) ;"
  , "lowp ivec2 foo(lowp mat3);"
  , "float[5] foo();"
  , "void foo (float[5]);"
  , "void foo (float a[5]);"
  , "float[] foo();"
  , "float[5] foo();"
  , "float[5] foo(int[4]);"
  , "int f ();"
-- TODO qualifier only possible where declarators.
--  , "const struct light { float intensity; vec3 position; };"
  , "float a[5][3];"
-- interpolation qualifier may only preced [centroid]in/out.
--  , "smooth const int a;"
  ]

----------------------------------------------------------------------
-- function definitions
----------------------------------------------------------------------

legalFunctionDefinitionsTests :: Test
legalFunctionDefinitionsTests = TestLabel "legal function definition" $
  TestList $ map (doesParse LGNP.functionDefinition) testFunctionDefinitionsTrue

compareFunctionDefinitionsTests :: Test
compareFunctionDefinitionsTests = TestLabel "compare function definitions" $
  TestList $ map (compareParse LGNP.functionDefinition LGP.functionDefinition) testFunctionDefinitionsTrue

testFunctionDefinitionsTrue :: [String]
testFunctionDefinitionsTrue =
  [ "void main ()\n\
    \{\n\
    \}"
  , "void main ()\n\
    \{\n\
    \  if (intensity < 0.0)\n\
    \    return;\n\
    \}"
  ]

----------------------------------------------------------------------
-- comments (inside simple declarations)
----------------------------------------------------------------------

legalCommentsTests :: Test
legalCommentsTests = TestLabel "legal comments" $
  TestList $ map (doesParse LGNP.declaration) testCommentsTrue

illegalCommentsTests :: Test
illegalCommentsTests = TestLabel "illegal comments" $
  TestList $ map (doesNotParse LGNP.declaration) testCommentsFalse

compareCommentsTests :: Test
compareCommentsTests = TestLabel "compare comments" $
  TestList $ map (compareParse LGNP.declaration LGP.declaration) testCommentsTrue

testCommentsTrue :: [String]
testCommentsTrue =
  [ "int a; // a comment"
  , "int a; /* another comment */"
  , "int a; // a comment\n"
  , "int a; /* another comment */\n"
  , "int a; /* another comment\non multiple\nlines.*/"
  , "int a; /* another comment\non multiple\nlines.*/\n"
  , "int a; /* another comment\non multiple\nlines.\n*/"
  , "/* before */ int a;"
  , "// before\nint a;"
  , "int /* middle */ a;"
  , "int/* middle */a;"
  , "int a/* middle */;"
  , "int a; /* not a // nested comment */"
  ]

testCommentsFalse :: [String]
testCommentsFalse =
  [ "int a; /* no /* nested */  comment */"
  ]

----------------------------------------------------------------------
-- numbers
----------------------------------------------------------------------

legalNumberTests :: Test
legalNumberTests = TestLabel "legal number" $
  TestList $ map (doesParse P.number) testNumbersTrue

testNumbersTrue :: [String]
testNumbersTrue =
  [ "23"
  , "23u"
  , "0xA1"
  , "0xA1u"
  , "0x07u"
  , "1.0"
  , "1.0f"
  , "1."
  , "1.f"
  , ".0"
  , "1e1"
  , "1.0e1"
  , "1.e1"
  , "1.e1f"
  , "1.0e-1"
  , "1.0e+1"
  , "1.0e+1f"
  , "1.0e+1F"
  ]
