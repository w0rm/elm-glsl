{-# LANGUAGE OverloadedStrings #-}
module Language.GLSL.NewParser (parse, translationUnit) where

import Data.Text (Text)

import qualified Parse.Helpers as PH
import qualified Reporting.Annotation as RA
import qualified Reporting.Error.Syntax as RE

import qualified Language.GLSL.Syntax as LGS

parse :: Text -> Either (RA.Located RE.Error) LGS.TranslationUnit
parse = PH.run translationUnit

-- Tokens

semicolon :: PH.Parser ()
semicolon =
  PH.keyword ";"

lbrace :: PH.Parser ()
lbrace =
  PH.keyword "{"

rbrace :: PH.Parser ()
rbrace =
  PH.keyword "}"

lbracket :: PH.Parser ()
lbracket =
  PH.keyword "["

rbracket :: PH.Parser ()
rbracket =
  PH.keyword "]"

lparen :: PH.Parser ()
lparen =
  PH.keyword "("

rparen :: PH.Parser ()
rparen =
  PH.keyword ")"

-- Parsers

translationUnit :: PH.Parser LGS.TranslationUnit
translationUnit = do
  ex <- externalDeclaration
  LGS.TranslationUnit `fmap` chompExternalDeclarations [ex]

chompExternalDeclarations :: [LGS.ExternalDeclaration] -> PH.Parser [LGS.ExternalDeclaration]
chompExternalDeclarations decls =
  PH.oneOf
    [ PH.try $ do
        PH.spaces
        ex <- externalDeclaration
        chompExternalDeclarations (ex:decls)
    , return $ reverse decls
    ]

externalDeclaration :: PH.Parser LGS.ExternalDeclaration
externalDeclaration =
  PH.oneOf
    -- TODO: Function
    [ LGS.Declaration `fmap` declaration
    ]

declaration :: PH.Parser LGS.Declaration
declaration =
  PH.oneOf
    -- TODO: Add more
    [ do
        PH.keyword "precision"
        PH.spaces
        q <- precisionQualifier
        PH.spaces
        s <- typeSpecifierNoPrecision
        PH.spaces
        semicolon
        return $ LGS.Precision q s
    , do
        q <- typeQualifier
        PH.oneOf
          [ semicolon >> return (LGS.TQ q)
          -- TODO: Add more
          ]
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
  PH.spaces
  lparen
  PH.spaces
  -- TODO: Separated by comma
  q <- layoutQualifierId
  PH.spaces
  rparen
  return $ LGS.Layout [q]

layoutQualifierId :: PH.Parser LGS.LayoutQualifierId
layoutQualifierId =
  undefined

typeQualifier :: PH.Parser LGS.TypeQualifier
typeQualifier = PH.oneOf
  [ do
      s <- storageQualifier
      return $ LGS.TypeQualSto s
  -- TODO: Storage qualifier can be Nothing
  , do
      l <- layoutQualifier
      s <- storageQualifier
      return $ LGS.TypeQualLay l (Just s)
  -- TODO: Storage qualifier can be Nothing
  , do
      i <- interpolationQualifier
      s <- storageQualifier
      return $ LGS.TypeQualInt i (Just s)
  , do
      i <- invariantQualifier
      PH.oneOf
        [ do
            j <- interpolationQualifier
            s <- storageQualifier
            return $ LGS.TypeQualInv3 i j s
        , do
            s <- storageQualifier
            return $ LGS.TypeQualInv i (Just s)
        , return $ LGS.TypeQualInv i Nothing
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
    , PH.keyword "centroid" >> PH.oneOf
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
        s <- typeSpecifierNoPrecision
        return $ LGS.TypeSpec (Just q) s
    , LGS.TypeSpec Nothing `fmap` typeSpecifierNoPrecision
    ]

typeSpecifierNoPrecision :: PH.Parser LGS.TypeSpecifierNoPrecision
typeSpecifierNoPrecision = do
  s <- typeSpecifierNonArray
  PH.oneOf
    -- TODO: Add more
    [ return $ LGS.TypeSpecNoPrecision s Nothing
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
    -- TODO:
    -- , structSpecifier
    -- , identifier >>= return . TypeName -- verify if it is declared
    ]

precisionQualifier :: PH.Parser LGS.PrecisionQualifier
precisionQualifier =
  PH.oneOf
    [ PH.keyword "highp" >> return LGS.HighP
    , PH.keyword "mediump" >> return LGS.MediumP
    , PH.keyword "lowp" >> return LGS.LowP
    ]
