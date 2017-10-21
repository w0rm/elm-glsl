{-# LANGUAGE OverloadedStrings, UnboxedTuples #-}
module Language.GLSL.NewParser (parse, expression, declaration, functionDefinition, translationUnit) where

import Prelude hiding (length)
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Text
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified GHC.Float as Float

import qualified AST.Literal as AL
import qualified Parse.Helpers as PH
import qualified Reporting.Annotation as RA
import qualified Reporting.Error.Syntax as RE
import qualified Parse.Primitives as PP

import qualified Language.GLSL.Syntax as LGS
import qualified Language.GLSL.Primitives as P

parse :: Text.Text -> Either (RA.Located RE.Error) LGS.TranslationUnit
parse = PH.run translationUnit


----------------------------------------------------------------------
-- Reserved words
----------------------------------------------------------------------

-- List of keywords.
keywords :: Set.Set Text.Text
keywords = Set.fromList $ concatMap Text.words
  [ "attribute const uniform varying"
  , "layout"
  , "centroid flat smooth noperspective"
  , "break continue do for while switch case default"
  , "if else"
  , "in out inout"
  , "float int void bool true false"
  , "invariant"
  , "discard return"
  , "mat2 mat3 mat4"
  , "mat2x2 mat2x3 mat2x4"
  , "mat3x2 mat3x3 mat3x4"
  , "mat4x2 mat4x3 mat4x4"
  , "vec2 vec3 vec4 ivec2 ivec3 ivec4 bvec2 bvec3 bvec4"
  , "uint uvec2 uvec3 uvec4"
  , "lowp mediump highp precision"
  , "sampler1D sampler2D sampler3D samplerCube"
  , "sampler1DShadow sampler2DShadow samplerCubeShadow"
  , "sampler1DArray sampler2DArray"
  , "sampler1DArrayShadow sampler2DArrayShadow"
  , "isampler1D isampler2D isampler3D isamplerCube"
  , "isampler1DArray isampler2DArray"
  , "usampler1D usampler2D usampler3D usamplerCube"
  , "usampler1DArray usampler2DArray"
  , "sampler2DRect sampler2DRectShadow isampler2DRect usampler2DRect"
  , "samplerBuffer isamplerBuffer usamplerBuffer"
  , "sampler2DMS isampler2DMS usampler2DMS"
  , "sampler2DMSArray isampler2DMSArray usampler2DMSArray"
  , "struct"
  ]

-- List of keywords reserved for future use.
reservedWords :: Set.Set Text.Text
reservedWords = Set.fromList $ concatMap Text.words
  [ "common partition active"
  , "asm"
  , "class union enum typedef template this packed"
  , "goto"
  , "inline noinline volatile public static extern external interface"
  , "long short double half fixed unsigned superp"
  , "input output"
  , "hvec2 hvec3 hvec4 dvec2 dvec3 dvec4 fvec2 fvec3 fvec4"
  , "sampler3DRect"
  , "filter"
  , "image1D image2D image3D imageCube"
  , "iimage1D iimage2D iimage3D iimageCube"
  , "uimage1D uimage2D uimage3D uimageCube"
  , "image1DArray image2DArray"
  , "iimage1DArray iimage2DArray uimage1DArray uimage2DArray"
  , "image1DShadow image2DShadow"
  , "image1DArrayShadow image2DArrayShadow"
  , "imageBuffer iimageBuffer uimageBuffer"
  , "sizeof cast"
  , "namespace using"
  , "row_major"
  ]

-- Tokens

semicolon :: PH.Parser ()
semicolon =
  PH.symbol ";"

colon :: PH.Parser ()
colon =
  PH.symbol ":"

lbracket :: PH.Parser ()
lbracket =
  PH.symbol "["

rbracket :: PH.Parser ()
rbracket =
  PH.symbol "]"

lparen :: PH.Parser ()
lparen =
  PH.symbol "("

rparen :: PH.Parser ()
rparen =
  PH.symbol ")"

comma :: PH.Parser ()
comma =
  PH.symbol ","

-- TODO: Is this really necessary?
-- Try to parse a given string, and allow identifier characters
-- (or anything else) to directly follow.
operator :: Text.Text -> PH.Parser ()
operator symbols =
  PH.try $ do
    PH.symbol symbols
    P.whitespace

{-# INLINE expect #-}
expect :: Int -> Int -> RE.ContextStack -> [RE.Theory] -> RE.ParseError
expect row col ctx theories =
  RE.ParseError row col (RE.Theories ctx theories)

identifierTheories :: [RE.Theory]
identifierTheories =
  [RE.LowVar, RE.CapVar, RE.Symbol "_"]

--TODO: use a proper theory
identifier :: PH.Parser Text.Text
identifier =
  PH.Parser $ \(PP.State array offset length indent row col ctx) cok _ _ eerr ->
    if length == 0 then
      eerr (expect row col ctx identifierTheories)
    else
      let
        (Text.Iter char size) = PP.peek array offset
      in
        if Char.isLower char || Char.isUpper char || char == '_' then
          let
            (# newOffset, newLength, newCol #) =
              PP.varPrimHelp array (offset + size) (length - size) (col + 1)

            copiedText =
              PP.copyText array offset (newOffset - offset)
          in
            if Set.member copiedText keywords then
              eerr (expect row newCol ctx [RE.Keyword (Text.append copiedText " is a keyword")])
            else if Set.member copiedText reservedWords then
              eerr (expect row newCol ctx [RE.Keyword (Text.append copiedText " is reserved")])
            else if Text.isInfixOf "__" copiedText then
              eerr (expect row newCol ctx [RE.Keyword (Text.append copiedText " is reserved (two consecutive underscores)")])
            else
              cok copiedText (PP.State array newOffset newLength indent row newCol ctx) PP.noError

        else
          eerr (expect row col ctx identifierTheories)

-- TODO: Preserve decimal/hexadecimal/etc.
intConstant :: PH.Parser LGS.Expr
intConstant = do
  n <- PH.number
  case n of
    AL.IntNum i ->
      return $ LGS.IntConstant LGS.Decimal $ toInteger i
    _ ->
      fail "TODO"

floatingConstant :: PH.Parser LGS.Expr
floatingConstant = do
  n <- PH.number
  case n of
    AL.FloatNum f ->
      return $ LGS.FloatConstant $ Float.double2Float f
    _ ->
      fail "TODO"

-- Parsers

primaryExpression :: PH.Parser LGS.Expr
primaryExpression =
  PH.oneOf
    [ (LGS.Variable . Text.unpack) `fmap` PH.try identifier
    , intConstant
    , floatingConstant
    , PH.keyword "true" >> return (LGS.BoolConstant True)
    , PH.keyword "false" >> return (LGS.BoolConstant False)
    , do
        lparen
        P.whitespace
        e <- expression
        P.whitespace
        rparen
        return e
    ]

postfixExpression :: PH.Parser LGS.Expr
postfixExpression = do
  e <- PH.oneOf
    [ PH.try $ do
        (i, p) <- functionCallGeneric
        return $ LGS.FunctionCall i p
    , primaryExpression
    ]
  p <- P.repeat P.zeroOrMore $ PH.oneOf
    [ do
        lbracket
        P.whitespace
        i <- integerExpression
        P.whitespace
        rbracket
        return $ flip LGS.Bracket i
    , dotFunctionCallGeneric
    , dotFieldSelection
    , operator "++" >> return LGS.PostInc
    , operator "--" >> return LGS.PostDec
    ]
  return $ foldl (flip ($)) e p

dotFunctionCallGeneric :: PH.Parser (LGS.Expr -> LGS.Expr)
dotFunctionCallGeneric = do
  (i, p) <- PH.try $ do
    PH.symbol "."
    functionCallGeneric
  P.whitespace
  return $ \e -> LGS.MethodCall e i p

dotFieldSelection :: PH.Parser (LGS.Expr -> LGS.Expr)
dotFieldSelection = do
  -- TODO: Is this try necessary?
  i <- PH.try $ do
    PH.symbol "."
    identifier
  return $ flip LGS.FieldSelection (Text.unpack i)

integerExpression :: PH.Parser LGS.Expr
integerExpression = expression

functionCallGeneric :: PH.Parser (LGS.FunctionIdentifier, LGS.Parameters)
functionCallGeneric = do
  i <- functionCallHeader
  P.whitespace
  p <- PH.oneOf
    [ PH.keyword "void" >> return LGS.ParamVoid
    , LGS.Params <$> P.sepBy assignmentExpression (P.whitespace >> comma >> P.whitespace)
    ]
  P.whitespace
  rparen
  return (i, p)

functionCallHeader :: PH.Parser LGS.FunctionIdentifier
functionCallHeader = do
  i <- functionIdentifier
  P.whitespace
  lparen
  return i

functionIdentifier :: PH.Parser LGS.FunctionIdentifier
functionIdentifier =
  PH.oneOf
    [ LGS.FuncId . Text.unpack <$> PH.try identifier
    , LGS.FuncIdTypeSpec <$> typeSpecifier -- TODO if the 'identifier' is declared as a type, should be this case
    -- no need for fieldSelection
    ]

unaryExpression :: PH.Parser LGS.Expr
unaryExpression = do
  p <- P.repeat P.zeroOrMore $ PH.oneOf
    [ operator "++" >> return LGS.PreInc
    , operator "--" >> return LGS.PreDec
    , operator "+" >> return LGS.UnaryPlus
    , operator "-" >> return LGS.UnaryNegate
    , operator "!" >> return LGS.UnaryNot
    , operator "~" >> return LGS.UnaryOneComplement
    ]
  e <- postfixExpression
  return $ foldr ($) e p

conditionalExpression :: PH.Parser LGS.Expr
conditionalExpression = do
  loe <- unaryExpression
  P.whitespace
  ter <- P.optionMaybe $ do
    PH.symbol "?"
    P.whitespace
    e <- expression
    P.whitespace
    PH.symbol ":"
    P.whitespace
    a <- assignmentExpression
    return (e, a)
  case ter of
    Nothing ->
      return loe
    Just (e, a) ->
      return $ LGS.Selection loe e a

constantExpression :: PH.Parser LGS.Expr
constantExpression =
  conditionalExpression

-- TODO: implement
assignmentExpression :: PH.Parser LGS.Expr
assignmentExpression =
  conditionalExpression

-- TODO: Implement
expression :: PH.Parser LGS.Expr
expression =
  assignmentExpression

translationUnit :: PH.Parser LGS.TranslationUnit
translationUnit = do
  ex <- externalDeclaration
  LGS.TranslationUnit `fmap` chompExternalDeclarations [ex]

chompExternalDeclarations :: [LGS.ExternalDeclaration] -> PH.Parser [LGS.ExternalDeclaration]
chompExternalDeclarations decls =
  PH.oneOf
    [ PH.try $ do
        P.whitespace
        ex <- externalDeclaration
        chompExternalDeclarations (ex:decls)
    , return $ reverse decls
    ]

externalDeclaration :: PH.Parser LGS.ExternalDeclaration
externalDeclaration =
  PH.oneOf
    [ do
        p <- PH.try functionPrototype
        P.whitespace
        PH.oneOf
          [ semicolon >> return (LGS.FunctionDeclaration p)
          , fmap (LGS.FunctionDefinition p) compoundStatementNoNewScope
          ]
    , LGS.Declaration `fmap` declaration
    ]

-- inside externalDeclaration, used only in tests
functionDefinition :: PH.Parser LGS.ExternalDeclaration
functionDefinition = do
  fp <- functionPrototype
  P.whitespace
  cs <- compoundStatementNoNewScope
  return $ LGS.FunctionDefinition fp cs

declaration :: PH.Parser LGS.Declaration
declaration =
  PH.oneOf
    [ PH.try $ do
        t <- fullySpecifiedType
        P.whitespace
        l <- listOfDeclarations
        P.whitespace
        semicolon
        return $ LGS.InitDeclaration (LGS.TypeDeclarator t) l
    , do
        PH.keyword "invariant"
        l <- listOfDeclarations
        P.whitespace
        semicolon
        return $ LGS.InitDeclaration LGS.InvariantDeclarator l
    , do
        PH.keyword "precision"
        P.whitespace
        q <- precisionQualifier
        P.whitespace
        s <- typeSpecifierNoPrecision
        P.whitespace
        semicolon
        return $ LGS.Precision q s
    , do
        q <- typeQualifier
        P.whitespace
        PH.oneOf
          [ semicolon >> return (LGS.TQ q)
          , do
              i <- identifier
              P.whitespace
              s <- structDeclarationList
              P.whitespace
              m <- P.optionMaybe $ do
                j <- identifier
                P.whitespace
                n <- P.optionMaybe $ do
                  lbracket
                  P.whitespace
                  c <- P.optionMaybe $ do
                    ce <- constantExpression
                    P.whitespace
                    return ce
                  rbracket
                  P.whitespace
                  return c
                return (Text.unpack j, n)
              semicolon
              return $ LGS.Block q (Text.unpack i) s m
          ]
    ]

listOfDeclarations :: PH.Parser [LGS.InitDeclarator]
listOfDeclarations =
  P.sepBy singleDeclaration (P.whitespace >> comma >> P.whitespace)

functionPrototype :: PH.Parser LGS.FunctionPrototype
functionPrototype = do
  (t, i, p) <- functionDeclarator
  return $ LGS.FuncProt t (Text.unpack i) p

functionDeclarator :: PH.Parser (LGS.FullType, Text.Text, [LGS.ParameterDeclaration])
functionDeclarator = do
  (t, i) <- functionHeader
  P.whitespace
  p <- P.sequence "(" ")" "," parameterDeclaration
  return (t, i, p)

functionHeader :: PH.Parser (LGS.FullType, Text.Text)
functionHeader = do
  t <- fullySpecifiedType
  P.whitespace
  i <- identifier
  return (t, i)

parameterDeclaration :: PH.Parser LGS.ParameterDeclaration
parameterDeclaration = do
  tq <- P.optionMaybe $ do
    ptq <- parameterTypeQualifier
    P.whitespace
    return ptq
  q <- P.optionMaybe $ do
    pq <- parameterQualifier
    P.whitespace
    return pq
  s <- typeSpecifier
  P.whitespace
  m <- P.optionMaybe $ do
    i <- identifier
    P.whitespace
    b <- P.optionMaybe $ do
      lbracket
      P.whitespace
      ce <- constantExpression
      P.whitespace
      rbracket
      return ce
    return (Text.unpack i, b)
  return $ LGS.ParameterDeclaration tq q s m

parameterQualifier :: PH.Parser LGS.ParameterQualifier
parameterQualifier =
  -- "empty" case handled in the caller
  PH.oneOf
    [ tryKeyword "inout" LGS.InOutParameter
    , tryKeyword "in" LGS.InParameter
    , tryKeyword "out" LGS.OutParameter
    ]
  where
    tryKeyword kw ast = PH.try (PH.keyword kw) >> return ast

singleDeclaration :: PH.Parser LGS.InitDeclarator
singleDeclaration = do
  i <- identifier
  P.whitespace
  m <- P.optionMaybe $ do
    lbracket
    P.whitespace
    o <- P.optionMaybe constantExpression
    P.whitespace
    rbracket
    return o
  j <- P.optionMaybe $ PH.symbol "=" >> P.whitespace >> initializer
  return $ LGS.InitDecl (Text.unpack i) m j

fullySpecifiedType :: PH.Parser LGS.FullType
fullySpecifiedType = PH.oneOf
  [ LGS.FullType Nothing <$> PH.try typeSpecifier
  , do q <- typeQualifier
       P.whitespace
       s <- typeSpecifier
       return $ LGS.FullType (Just q) s
  ]

invariantQualifier :: PH.Parser LGS.InvariantQualifier
invariantQualifier = do
  PH.keyword "invariant"
  return LGS.Invariant

interpolationQualifier :: PH.Parser LGS.InterpolationQualifier
interpolationQualifier =
  PH.oneOf
    [ PH.keyword "smooth" >> return LGS.Smooth
    , PH.keyword "flat" >> return LGS.Flat
    , PH.keyword "noperspective" >> return LGS.NoPerspective
    ]

layoutQualifier :: PH.Parser LGS.LayoutQualifier
layoutQualifier = do
  PH.keyword "layout"
  P.whitespace
  q <- layoutQualifierIdList
  return $ LGS.Layout q

layoutQualifierIdList :: PH.Parser [LGS.LayoutQualifierId]
layoutQualifierIdList =
  P.sequence "(" ")" "," layoutQualifierId

layoutQualifierId :: PH.Parser LGS.LayoutQualifierId
layoutQualifierId = do
  i <- identifier
  P.whitespace
  c <- P.optionMaybe $ do
    PH.symbol "="
    P.whitespace
    intConstant
  return $ LGS.LayoutQualId (Text.unpack i) c

parameterTypeQualifier :: PH.Parser LGS.ParameterTypeQualifier
parameterTypeQualifier = PH.keyword "const" >> return LGS.ConstParameter

typeQualifier :: PH.Parser LGS.TypeQualifier
typeQualifier = PH.oneOf
  [ do
      s <- storageQualifier
      return $ LGS.TypeQualSto s
  , do
      l <- layoutQualifier
      P.whitespace
      s <- P.optionMaybe storageQualifier
      return $ LGS.TypeQualLay l s
  , do
      i <- interpolationQualifier
      P.whitespace
      s <- P.optionMaybe storageQualifier
      return $ LGS.TypeQualInt i s
  , do
      i <- invariantQualifier
      P.whitespace
      PH.oneOf
        [ do
            j <- interpolationQualifier
            P.whitespace
            s <- storageQualifier
            return $ LGS.TypeQualInv3 i j s
        , do
            s <- P.optionMaybe storageQualifier
            return $  LGS.TypeQualInv i s
        ]
  ]

-- TODO see 4.3 for restrictions
storageQualifier :: PH.Parser LGS.StorageQualifier
storageQualifier =
  PH.oneOf
    [ PH.keyword "const" >> return LGS.Const
    , PH.keyword "attribute" >> return LGS.Attribute -- TODO vertex only, is deprecated
    , PH.keyword "varying" >> return LGS.Varying -- deprecated
    , PH.keyword "in" >> return LGS.In
    , PH.keyword "out" >> return LGS.Out
    , PH.keyword "centroid" >> P.whitespace >> PH.oneOf
      [ PH.keyword "varying" >> return LGS.CentroidVarying -- deprecated
      , PH.keyword "in" >> return LGS.CentroidIn
      , PH.keyword "out" >> return LGS.CentroidOut
      ]
    , PH.keyword "uniform" >> return LGS.Uniform
    ]

typeSpecifier :: PH.Parser LGS.TypeSpecifier
typeSpecifier =
  PH.oneOf
    [ do
        q <- PH.try precisionQualifier
        P.whitespace
        s <- typeSpecifierNoPrecision
        return $ LGS.TypeSpec (Just q) s
    , LGS.TypeSpec Nothing `fmap` typeSpecifierNoPrecision
    ]

typeSpecifierNoPrecision :: PH.Parser LGS.TypeSpecifierNoPrecision
typeSpecifierNoPrecision = do
  s <- typeSpecifierNonArray
  PH.oneOf
    [ PH.try $ do
        lbracket
        P.whitespace
        rbracket
        return (LGS.TypeSpecNoPrecision s (Just Nothing))
    , do
        lbracket
        P.whitespace
        c <- constantExpression
        P.whitespace
        rbracket
        return (LGS.TypeSpecNoPrecision s (Just $ Just c))
    , return $ LGS.TypeSpecNoPrecision s Nothing
    ]

-- Basic types, structs, and user-defined types.
typeSpecifierNonArray :: PH.Parser LGS.TypeSpecifierNonArray
typeSpecifierNonArray =
  PH.oneOf
    [ PH.keyword "void" >> return LGS.Void
    , PH.keyword "float" >> return LGS.Float
    , PH.keyword "int" >> return LGS.Int
    , PH.keyword "uint" >> return LGS.UInt
    , PH.keyword "bool" >> return LGS.Bool
    , PH.keyword "vec2" >> return LGS.Vec2
    , PH.keyword "vec3" >> return LGS.Vec3
    , PH.keyword "vec4" >> return LGS.Vec4
    , PH.keyword "bvec2" >> return LGS.BVec2
    , PH.keyword "bvec3" >> return LGS.BVec3
    , PH.keyword "bvec4" >> return LGS.BVec4
    , PH.keyword "ivec2" >> return LGS.IVec2
    , PH.keyword "ivec3" >> return LGS.IVec3
    , PH.keyword "ivec4" >> return LGS.IVec4
    , PH.keyword "uvec2" >> return LGS.UVec2
    , PH.keyword "uvec3" >> return LGS.UVec3
    , PH.keyword "uvec4" >> return LGS.UVec4
    , PH.keyword "mat2" >> return LGS.Mat2
    , PH.keyword "mat3" >> return LGS.Mat3
    , PH.keyword "mat4" >> return LGS.Mat4
    , PH.keyword "mat2x2" >> return LGS.Mat2x2
    , PH.keyword "mat2x3" >> return LGS.Mat2x3
    , PH.keyword "mat2x4" >> return LGS.Mat2x4
    , PH.keyword "mat3x2" >> return LGS.Mat3x2
    , PH.keyword "mat3x3" >> return LGS.Mat3x3
    , PH.keyword "mat3x4" >> return LGS.Mat3x4
    , PH.keyword "mat4x2" >> return LGS.Mat4x2
    , PH.keyword "mat4x3" >> return LGS.Mat4x3
    , PH.keyword "mat4x4" >> return LGS.Mat4x4
    , PH.keyword "sampler1D" >> return LGS.Sampler1D
    , PH.keyword "sampler2D" >> return LGS.Sampler2D
    , PH.keyword "sampler3D" >> return LGS.Sampler3D
    , PH.keyword "samplerCube" >> return LGS.SamplerCube
    , PH.keyword "sampler1DShadow" >> return LGS.Sampler1DShadow
    , PH.keyword "sampler2DShadow" >> return LGS.Sampler2DShadow
    , PH.keyword "samplerCubeShadow" >> return LGS.SamplerCubeShadow
    , PH.keyword "sampler1DArray" >> return LGS.Sampler1DArray
    , PH.keyword "sampler2DArray" >> return LGS.Sampler2DArray
    , PH.keyword "sampler1DArrayShadow" >> return LGS.Sampler1DArrayShadow
    , PH.keyword "sampler2DArrayShadow" >> return LGS.Sampler2DArrayShadow
    , PH.keyword "isampler1D" >> return LGS.ISampler1D
    , PH.keyword "isampler2D" >> return LGS.ISampler2D
    , PH.keyword "isampler3D" >> return LGS.ISampler3D
    , PH.keyword "isamplerCube" >> return LGS.ISamplerCube
    , PH.keyword "isampler1DArray" >> return LGS.ISampler1DArray
    , PH.keyword "isampler2DArray" >> return LGS.ISampler2DArray
    , PH.keyword "usampler1D" >> return LGS.USampler1D
    , PH.keyword "usampler2D" >> return LGS.USampler2D
    , PH.keyword "usampler3D" >> return LGS.USampler3D
    , PH.keyword "usamplerCube" >> return LGS.USamplerCube
    , PH.keyword "usampler1DArray" >> return LGS.USampler1DArray
    , PH.keyword "usampler2DArray" >> return LGS.USampler2DArray
    , PH.keyword "sampler2DRect" >> return LGS.Sampler2DRect
    , PH.keyword "sampler2DRectShadow" >> return LGS.Sampler2DRectShadow
    , PH.keyword "isampler2DRect" >> return LGS.ISampler2DRect
    , PH.keyword "usampler2DRect" >> return LGS.USampler2DRect
    , PH.keyword "samplerBuffer" >> return LGS.SamplerBuffer
    , PH.keyword "isamplerBuffer" >> return LGS.ISamplerBuffer
    , PH.keyword "usamplerBuffer" >> return LGS.USamplerBuffer
    , PH.keyword "sampler2DMS" >> return LGS.Sampler2DMS
    , PH.keyword "isampler2DMS" >> return LGS.ISampler2DMS
    , PH.keyword "usampler2DMS" >> return LGS.USampler2DMS
    , PH.keyword "sampler2DMSArray" >> return LGS.Sampler2DMSArray
    , PH.keyword "isampler2DMSArray" >> return LGS.ISampler2DMSArray
    , PH.keyword "usampler2DMSArray" >> return LGS.USampler2DMSArray
    , structSpecifier
    , LGS.TypeName . Text.unpack <$> identifier  -- verify if it is declared
    ]

precisionQualifier :: PH.Parser LGS.PrecisionQualifier
precisionQualifier =
  PH.oneOf
    [ PH.keyword "highp" >> return LGS.HighP
    , PH.keyword "mediump" >> return LGS.MediumP
    , PH.keyword "lowp" >> return LGS.LowP
    ]

structSpecifier :: PH.Parser LGS.TypeSpecifierNonArray
structSpecifier = do
  PH.keyword "struct"
  P.whitespace
  i <- P.optionMaybe identifier
  P.whitespace
  d <- structDeclarationList
  return $ LGS.StructSpecifier (Text.unpack `fmap` i) d

structDeclarationList :: PH.Parser [LGS.Field]
structDeclarationList = do
  list <- P.repeating "{" "}" structDeclaration
  case list of
    [] -> PH.deadend [RE.Keyword "Expect struct declaration"]
    _ -> return list

structDeclaration :: PH.Parser LGS.Field
structDeclaration = do
  q <- P.optionMaybe typeQualifier
  P.whitespace
  s <- typeSpecifier
  P.whitespace
  l <- structDeclaratorList
  P.whitespace
  semicolon
  return $ LGS.Field q s l

structDeclaratorList :: PH.Parser [LGS.StructDeclarator]
structDeclaratorList =
  P.sepBy structDeclarator (P.whitespace >> comma >> P.whitespace)

structDeclarator :: PH.Parser LGS.StructDeclarator
structDeclarator = do
  i <- identifier
  P.whitespace
  PH.oneOf
    [ do
        lbracket
        P.whitespace
        e <- P.optionMaybe constantExpression
        P.whitespace
        rbracket
        return $ LGS.StructDeclarator (Text.unpack i) (Just e)
    , return $ LGS.StructDeclarator (Text.unpack i) Nothing
    ]

initializer :: PH.Parser LGS.Expr
initializer = assignmentExpression

declarationStatement :: PH.Parser LGS.Declaration
declarationStatement = declaration

statement :: PH.Parser LGS.Statement
statement =
  PH.oneOf
    [ fmap LGS.CompoundStatement compoundStatement
    , simpleStatement
    ]

simpleStatement :: PH.Parser LGS.Statement
simpleStatement =
  PH.oneOf
    [ LGS.DeclarationStatement <$> declarationStatement
    -- TODO: expression statment
    , selectionStatement
    , switchStatement
    , LGS.CaseLabel <$> caseLabel
    -- TODO: Add more
    ]

compoundStatement :: PH.Parser LGS.Compound
compoundStatement = LGS.Compound <$>
  P.repeating "{" "}" statement

compoundStatementNoNewScope :: PH.Parser LGS.Compound
compoundStatementNoNewScope = compoundStatement

selectionStatement :: PH.Parser LGS.Statement
selectionStatement = do
  PH.keyword "if"
  P.whitespace
  lparen
  P.whitespace
  c <- expression
  rparen
  P.whitespace
  t <- statement
  f <- P.optionMaybe $ do
    PH.keyword "else"
    P.whitespace
    -- TODO: If whitespace does not exist here, `{` is required.
    statement
  return $ LGS.SelectionStatement c t f

switchStatement :: PH.Parser LGS.Statement
switchStatement = do
  PH.keyword "switch"
  P.whitespace
  lparen
  P.whitespace
  e <- expression
  P.whitespace
  rparen
  P.whitespace
  l <- P.repeating "{" "}" statement
  return $ LGS.SwitchStatement e l

caseLabel :: PH.Parser LGS.CaseLabel
caseLabel =
  PH.oneOf
    [ do
        PH.keyword "case"
        P.whitespace
        e <- expression
        P.whitespace
        colon
        return $ LGS.Case e
    , do
        PH.keyword "default"
        P.whitespace
        colon
        return LGS.Default
    ]
