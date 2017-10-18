{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Language.GLSL.Primitives (whitespace, optionMaybe) where

import qualified Data.Text.Array as Text

import qualified AST.Literal as AL
import qualified Parse.Helpers as PH
import qualified Parse.Primitives as PP
import qualified Reporting.Annotation as RA
import qualified Reporting.Region as RR
import qualified Reporting.Error.Syntax as RE

import qualified Language.GLSL.Syntax as LGS

optionMaybe :: PH.Parser a -> PH.Parser (Maybe a)
optionMaybe parser =
  PH.oneOf
    [ fmap Just parser
    , return Nothing
    ]

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
