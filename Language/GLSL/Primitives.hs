{-# LANGUAGE BangPatterns, DoAndIfThenElse, OverloadedStrings, UnboxedTuples #-}
module Language.GLSL.Primitives
  ( whitespace
  , number
  , optionMaybe
  , sepBy
  , repeat
  , oneOrMore
  , zeroOrMore
  , sequence
  , repeating
  , notFollowedBy
  , buildExpressionParser
  , Operator(..)
  , Assoc(..)
  )
  where

import Prelude hiding (repeat, sequence, length)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Array as Text
import qualified Data.Text.Internal as Text
import qualified Data.Char as Char
import GHC.Word (Word16)

import qualified Parse.Helpers as PH
import qualified Parse.Primitives as PP
import qualified Reporting.Error.Syntax as RE
import qualified Language.GLSL.Syntax as LGS

data Count
  = Exactly Int
  | AtLeast Int

oneOrMore :: Count
oneOrMore = AtLeast 1

zeroOrMore :: Count
zeroOrMore = AtLeast 0

repeat :: Count -> PH.Parser a -> PH.Parser [a]
repeat count parser =
  case count of
    Exactly n ->
      repeatExactlyHelp n parser []
    AtLeast n ->
      repeatAtLeastHelp n parser []

repeatExactlyHelp :: Int -> PH.Parser a -> [a] -> PH.Parser [a]
repeatExactlyHelp count parser revItems =
  if count <= 0 then
    return $ reverse revItems
  else do
    item <- parser
    repeatExactlyHelp (count - 1) parser (item:revItems)

repeatAtLeastHelp :: Int -> PH.Parser a -> [a] -> PH.Parser [a]
repeatAtLeastHelp count parser revItems =
  let
    recurse = do
      item <- parser
      repeatAtLeastHelp (count - 1) parser (item:revItems)
  in
    if count <= 0 then
      PH.oneOf
        [ recurse
        , return $ reverse revItems
        ]
    else
      recurse

sepBy :: PH.Parser a -> PH.Parser sep -> PH.Parser [a]
sepBy parser sep =
  sepByHelp parser sep []

-- TODO: Remove duplication
sepByHelp :: PH.Parser a -> PH.Parser sep -> [a] -> PH.Parser [a]
sepByHelp parser sep revItems =
  PH.oneOf
    [ PH.try $ do
        item <- parser
        _ <- sep
        sepByHelp parser sep (item:revItems)
    , do item <- parser
         return $ reverse (item:revItems)
    , return $ reverse revItems
    ]

optionMaybe :: PH.Parser a -> PH.Parser (Maybe a)
optionMaybe parser =
  PH.oneOf
    [ fmap Just parser
    , return Nothing
    ]

sequence :: Text.Text -> Text.Text -> Text.Text -> PH.Parser a -> PH.Parser [a]
sequence start end sep parser = do
  PH.symbol start
  whitespace
  PH.oneOf
    [ PH.symbol end >> return []
    , do
        item <- parser
        whitespace
        sequenceHelp start end sep parser [item]
    ]

sequenceHelp :: Text.Text -> Text.Text -> Text.Text -> PH.Parser a -> [a] -> PH.Parser [a]
sequenceHelp start end sep parser revItems =
  PH.oneOf
    [ do
        PH.symbol end
        return $ reverse revItems
    , do
        PH.symbol sep
        whitespace
        item <- parser
        sequenceHelp start end sep parser (item:revItems)
    ]

repeating :: Text.Text -> Text.Text -> PH.Parser a -> PH.Parser [a]
repeating start end parser = do
  PH.symbol start
  whitespace
  PH.oneOf
    [ PH.symbol end >> return []
    , do
        item <- parser
        whitespace
        repeatingHelp start end parser [item]
    ]

repeatingHelp :: Text.Text -> Text.Text -> PH.Parser a -> [a] -> PH.Parser [a]
repeatingHelp start end parser revItems =
  PH.oneOf
    [ do
        PH.symbol end
        return $ reverse revItems
    , do
        item <- parser
        whitespace
        repeatingHelp start end parser (item:revItems)
    ]

notFollowedBy :: PH.Parser a -> PH.Parser ()
notFollowedBy parser =
  PH.try $ PH.oneOf
    [ PH.try parser >> PH.deadend [RE.Keyword "Didn't expect this here"]
    , return ()
    ]

-- WHITESPACE

whitespace :: PH.Parser ()
whitespace =
  PH.Parser $ \(PP.State array offset length indent row col ctx) cok cerr _ _ ->
    case eatSpaces array offset length row col of
      Left err ->
        cerr err

      Right (newOffset, newLength, newRow, newCol) ->
        cok
          ()
          (PP.State array newOffset newLength indent newRow newCol ctx)
          PH.noError


eatSpaces :: Text.Array -> Int -> Int -> Int -> Int -> Either RE.ParseError ( Int, Int, Int, Int )
eatSpaces array offset length row col =
  if length == 0 then
    Right ( offset, length, row, col )

  else
    case Text.unsafeIndex array offset of
      0x0020 {-   -} ->
        eatSpaces array (offset + 1) (length - 1) row (col + 1)

      0x0009 {- \t -} ->
        eatSpaces array (offset + 1) (length - 1) row (col + 1)

      0x000A {- \n -} ->
        eatSpaces array (offset + 1) (length - 1) (row + 1) 1

      0x002F {- / -} ->
        eatComment array offset length row col

      0x000D {- \r -} ->
        eatSpaces array (offset + 1) (length - 1) row col

      _ ->
        Right ( offset, length, row, col )


-- LINE COMMENTS


eatComment :: Text.Array -> Int -> Int -> Int -> Int -> Either RE.ParseError ( Int, Int, Int, Int )
eatComment array offset length row col =
  if length == 1 then
    Right ( offset, length, row, col )

  else
    case Text.unsafeIndex array (offset + 1) of
      0x002F {- / -} ->
        eatLineCommentHelp array (offset + 2) (length - 2) row (col + 2)

      0x002A {- * -} ->
        do  (newOffset, newLength, newRow, newCol) <-
              eatMultiCommentHelp array (offset + 2) (length - 2) row (col + 2)
            eatSpaces array newOffset newLength newRow newCol

      _ ->
        Right ( offset, length, row, col )


eatLineCommentHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either RE.ParseError ( Int, Int, Int, Int )
eatLineCommentHelp array offset length row col =
  if length == 0 then
    Right ( offset, length, row, col )

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x000A {- \n -} then
        eatSpaces array (offset + 1) (length - 1) (row + 1) 1

      else if word < 0xD800 || 0xDBFF < word then
        eatLineCommentHelp array (offset + 1) (length - 1) row (col + 1)

      else
        eatLineCommentHelp array (offset + 2) (length - 2) row (col + 1)



-- MULTI COMMENTS


eatMultiCommentHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either RE.ParseError ( Int, Int, Int, Int )
eatMultiCommentHelp array offset length row col =
  if length == 0 then
    Left (RE.ParseError row col RE.EndOfFile_Comment)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x000A {- \n -} then

        eatMultiCommentHelp array (offset + 1) (length - 1) (row + 1) 1

      else if word == 0x002A {- * -} && length > 1 && Text.unsafeIndex array (offset + 1) == 0x002F {- / -} then

        eatMultiCommentHelp array (offset + 2) (length - 2) row (col + 2)

      else if word < 0xD800 || 0xDBFF < word then

        eatMultiCommentHelp array (offset + 1) (length - 1) row (col + 1)

      else

        eatMultiCommentHelp array (offset + 2) (length - 2) row (col + 1)

-- EXPRESSION

data Assoc
  = AssocNone
  | AssocLeft
  | AssocRight

-- We only use infix
data Operator a
  = Infix (PH.Parser (a -> a -> a)) Assoc
  -- | Prefix (PH.Parser (a -> a))
  -- | Postfix (PH.Parser (a -> a))

type OperatorTable a = [[Operator a]]

-- Based on https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Expr.html#v:buildExpressionParser
buildExpressionParser :: OperatorTable a -> PH.Parser a -> PH.Parser a
buildExpressionParser operators simpleExpr =
  foldl makeParser simpleExpr operators

makeParser :: PH.Parser a -> [Operator a] -> PH.Parser a
makeParser termParser ops =
  let
    (rassoc, lassoc, nassoc) =
      foldr splitOp ([], [], []) ops

    rassocOp = PH.oneOf rassoc
    lassocOp = PH.oneOf lassoc
    nassocOp = PH.oneOf nassoc

    ambiguous assoc op =
      PH.try $ do
        _ <- op
        PH.deadend [RE.Keyword $ "ambiguous use of a " <> assoc <> " associative operator"]

    ambiguousRight = ambiguous "right" rassocOp
    ambiguousLeft = ambiguous "left" lassocOp
    ambiguousNone = ambiguous "non" nassocOp

    rassocParser x =
      PH.oneOf
        [ do
            f <- rassocOp
            y <- termParser >>= rassocParser1
            return $ f x y
        , ambiguousLeft
        , ambiguousNone
        ]

    rassocParser1 x =
      PH.oneOf
        [ rassocParser x
        , return x
        ]

    lassocParser x =
      PH.oneOf
        [ do
            f <- lassocOp
            y <- termParser
            lassocParser1 $ f x y
        , ambiguousRight
        , ambiguousNone
        ]

    lassocParser1 x =
      PH.oneOf
        [ lassocParser x
        , return x
        ]

    nassocParser x = do
      f <- nassocOp
      y <- termParser
      PH.oneOf
        [ ambiguousRight
        , ambiguousLeft
        , ambiguousNone
        , return $ f x y
        ]
  in
    do
      x <- termParser
      PH.oneOf
        [ rassocParser x
        , lassocParser x
        , nassocParser x
        , return x
        ]

type InfixParser a = PH.Parser (a -> a -> a)
type AssocParsers a = ([InfixParser a], [InfixParser a], [InfixParser a])

splitOp :: Operator a -> AssocParsers a -> AssocParsers a
splitOp (Infix op assoc) (rassoc, lassoc, nassoc) =
  case assoc of
    AssocNone ->
      (rassoc, lassoc, op:nassoc)
    AssocLeft ->
      (rassoc, op:lassoc, nassoc)
    AssocRight ->
      (op:rassoc, lassoc, nassoc)

-- NUMBERS

number :: PH.Parser LGS.Expr
number =
  PH.Parser $ \(PP.State array offset length indent row col ctx) cok cerr _ eerr ->
    if length == 0 then
      eerr PH.noError

    else
      let
        !word = Text.unsafeIndex array offset
      in
        if not (isDigit word) && word /= 0x002E {- . -} then
          eerr PH.noError

        else
          let
            chompResults =
              if word == 0x0030 {- 0 -} then
                chompZero array offset (offset + 1) (length - 1)
              else if word == 0x002E {- . -} then
                chompFraction array offset (offset + 1) (length - 1) False
              else
                chompInt array offset (offset + 1) (length - 1)
          in
            case chompResults of
              Left (newOffset, problem) ->
                cerr (RE.ParseError row (col + (newOffset - offset)) problem)

              Right (newOffset, newLength, literal) ->
                if isDirtyEnd array newOffset newLength then
                  cerr (RE.ParseError row (col + (newOffset - offset)) RE.BadNumberEnd)

                else
                  cok
                    literal
                    (PP.State array newOffset newLength indent row (col + (newOffset - offset)) ctx)
                    PH.noError

isDirtyEnd :: Text.Array -> Int -> Int -> Bool
isDirtyEnd array offset length =
  if length == 0 then
    False
  else
    let
      !char = PP.peekChar array offset
    in
      Char.isAlpha char || char == '_'

chompInt :: Text.Array -> Int -> Int -> Int -> Either (Int, RE.Problem) (Int, Int, LGS.Expr)
chompInt array startOffset offset length =
  if length == 0 then
    Right ( offset, length, readInt LGS.Decimal array startOffset offset )
  else
    let
      !word = Text.unsafeIndex array offset
    in
      if isDigit word then
        chompInt array startOffset (offset + 1) (length - 1)
      else if word == 0x002E {- . -} then
        chompFraction array startOffset (offset + 1) (length - 1) True
      else if word == 0x0065 {- e -} || word == 0x0045 {- E -} then
        chompExponent array startOffset (offset + 1) (length - 1)
      else if word == 0x0055 {- U -} || word == 0x0075 {- u -} then
        Right ( offset + 1, length - 1, readInt LGS.Decimal array startOffset offset )
      else
        Right ( offset, length, readInt LGS.Decimal array startOffset offset )

chompFraction :: Text.Array -> Int -> Int -> Int -> Bool -> Either (Int, RE.Problem) (Int, Int, LGS.Expr)
chompFraction array startOffset offset length hadDigit =
  if length == 0 then
    Right (offset, length, readFloat array startOffset offset)
  else
    let
      !word = Text.unsafeIndex array offset
    in
      if isDigit word then
        chompFractionHelp array startOffset (offset + 1) (length - 1)
      else if hadDigit then
        if word == 0x0065 {- e -} || word == 0x0045 {- E -} then
          chompExponent array startOffset (offset + 1) (length - 1)
        else if word == 0x0066 {- f -} || word == 0x0046 {- F -} then
          Right (offset + 1, length - 1, readFloat array startOffset offset)
        else
          Right (offset, length, readFloat array startOffset offset)
      else
        Left (offset, RE.BadNumberExp)

chompFractionHelp :: Text.Array -> Int -> Int -> Int -> Either (Int, RE.Problem) (Int, Int, LGS.Expr)
chompFractionHelp array startOffset offset length =
  if length == 0 then
    Right (offset, length, readFloat array startOffset offset)
  else
    let
      !word = Text.unsafeIndex array offset
    in
      if isDigit word then
        chompFractionHelp array startOffset (offset + 1) (length - 1)
      else if word == 0x0065 {- e -} || word == 0x0045 {- E -} then
        chompExponent array startOffset (offset + 1) (length - 1)
      else if word == 0x0066 {- f -} || word == 0x0046 {- F -} then
        Right (offset + 1, length - 1, readFloat array startOffset offset)
      else
        Right (offset, length, readFloat array startOffset offset)


chompExponent :: Text.Array -> Int -> Int -> Int -> Either (Int, RE.Problem) (Int, Int, LGS.Expr)
chompExponent array startOffset offset length =
  if length == 0 then
    Right (offset, length, readFloat array startOffset offset)
  else
    let
      !word = Text.unsafeIndex array offset
    in
      if isDigit word then
        chompExponentHelp array startOffset (offset + 1) (length - 1)

      else if word == 0x002B {- + -} || word == 0x002D {- - -} then

        if length > 1 && isDigit (Text.unsafeIndex array (offset + 1)) then
          chompExponentHelp array startOffset (offset + 2) (length - 2)
        else
          Left (offset, RE.BadNumberExp)

      else
        Left (offset, RE.BadNumberExp)


chompExponentHelp :: Text.Array -> Int -> Int -> Int -> Either (Int, RE.Problem) (Int, Int, LGS.Expr)
chompExponentHelp array startOffset offset length =
  if length == 0 then
    Right (offset, length, readFloat array startOffset offset)
  else
    let
      !word = Text.unsafeIndex array offset
    in
      if isDigit word then
        chompExponentHelp array startOffset (offset + 1) (length - 1)
      else if word == 0x0066 {- f -} || word == 0x0046 {- F -} then
        Right (offset + 1, length - 1, readFloat array startOffset offset)
      else
        Right (offset, length, readFloat array startOffset offset)


chompZero :: Text.Array -> Int -> Int -> Int -> Either (Int, RE.Problem) (Int, Int, LGS.Expr)
chompZero array startOffset offset length =
  if length == 0 then
    Right ( offset, length, LGS.IntConstant LGS.Decimal 0 )
  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x0078 {- x -} || word == 0x0058 {- X -} then
        chompHexNumber array (offset + 1) (length - 1)

      else if word == 0x002E {- . -} then
        chompFraction array startOffset (offset + 1) (length - 1) True

      else if isDigit word then
        chompOctNumber array offset length

      else
        Right ( offset, length, LGS.IntConstant LGS.Decimal 0 )


chompHexNumber :: Text.Array -> Int -> Int -> Either (Int, RE.Problem) (Int, Int, LGS.Expr)
chompHexNumber array offset length =
  let
    (# newOffset, newLength, hexNumber #) =
      chompHex array offset length
  in
  if hexNumber == -1 then
    Left ( newOffset, RE.BadNumberHex )
  else
    Right ( newOffset, newLength, LGS.IntConstant LGS.Hexadecimal $ toInteger hexNumber )


chompOctNumber :: Text.Array -> Int -> Int -> Either (Int, RE.Problem) (Int, Int, LGS.Expr)
chompOctNumber array offset length =
  let
    (# newOffset, newLength, octNumber #) =
      chompOct array offset length
  in
  if octNumber == -1 then
    Left ( newOffset, RE.BadNumberHex )
  else
    Right ( newOffset, newLength, LGS.IntConstant LGS.Octal $ toInteger octNumber )


-- NUMBER HELPERS


{-# INLINE isDigit #-}
isDigit :: Word16 -> Bool
isDigit word =
  word <= 0x0039 {- 9 -} && word >= 0x0030 {- 0 -}


{-# INLINE readInt #-}
readInt :: LGS.IntConstantKind -> Text.Array -> Int -> Int -> LGS.Expr
readInt constantKind array startOffset endOffset =
  LGS.IntConstant constantKind $ read $ Text.unpack $
    Text.Text array startOffset (endOffset - startOffset)


{-# INLINE readFloat #-}
readFloat :: Text.Array -> Int -> Int -> LGS.Expr
readFloat array startOffset endOffset =
  LGS.FloatConstant $ read $ Text.unpack $
    Text.Text array startOffset (endOffset - startOffset)



-- CHOMP HEX


-- Return -1 if it has NO digits
-- Return -2 if it has BAD digits

{-# INLINE chompHex #-}
chompHex :: Text.Array -> Int -> Int -> (# Int, Int, Int #)
chompHex array offset length =
  chompHexHelp array offset length (-1) 0


chompHexHelp :: Text.Array -> Int -> Int -> Int -> Int -> (# Int, Int, Int #)
chompHexHelp array offset length finalNumber hexNumber =
  if length == 0 then
    (# offset, length, finalNumber #)
  else
    let
      !newNumber =
        stepHex (Text.unsafeIndex array offset) hexNumber
    in
      if newNumber < 0 then
        if newNumber == -3 then -- ends with U
          (# offset + 1, length - 1, finalNumber #)
        else
          (# offset, length, if newNumber == -1 then finalNumber else -2 #)
      else
        chompHexHelp array (offset + 1) (length - 1) newNumber newNumber


{-# INLINE stepHex #-}
stepHex :: Word16 -> Int -> Int
stepHex word hexNumber =
  if word <= 0x0039 {- 9 -} && word >= 0x0030 {- 0 -} then
    16 * hexNumber + fromIntegral (word - 0x0030 {- 0 -})

  else if word <= 0x0066 {- f -} && word >= 0x0061 {- a -} then
    16 * hexNumber + 10 + fromIntegral (word - 0x0061 {- a -})

  else if word <= 0x0046 {- F -} && word >= 0x0041 {- A -} then
    16 * hexNumber + 10 + fromIntegral (word - 0x0041 {- A -})

  else if word == 0x0055 {- U -} || word == 0x0075 {- u -} then
    -3

  else
    -1


-- CHOMP OCT

-- Return -1 if it has NO digits
-- Return -2 if it has BAD digits

{-# INLINE chompOct #-}
chompOct :: Text.Array -> Int -> Int -> (# Int, Int, Int #)
chompOct array offset length =
  chompOctHelp array offset length (-1) 0


chompOctHelp :: Text.Array -> Int -> Int -> Int -> Int -> (# Int, Int, Int #)
chompOctHelp array offset length finalNumber octNumber =
  if length == 0 then
    (# offset, length, finalNumber #)
  else
    let
      !newNumber =
        stepOct (Text.unsafeIndex array offset) octNumber
    in
    if newNumber < 0 then
      if newNumber == -3 then -- ends with U
        (# offset + 1, length - 1, finalNumber #)
      else
        (# offset, length, if newNumber == -1 then finalNumber else -2 #)
    else
      chompOctHelp array (offset + 1) (length - 1) newNumber newNumber


{-# INLINE stepOct #-}
stepOct :: Word16 -> Int -> Int
stepOct word octNumber =
  if word <= 0x0037 {- 7 -} && word >= 0x0030 {- 0 -} then
    8 * octNumber + fromIntegral (word - 0x0030 {- 0 -})

  else if word == 0x0055 {- U -} || word == 0x0075 {- u -} then
    -3

  else
    -1
