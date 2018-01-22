module Brainfuck.Parser where

import Brainfuck.Program

import Data.Maybe

import Text.Parsec
import Text.Parsec.Char (anyChar)
import Text.Parsec.String (Parser)

fullProgram :: Parser Program
fullProgram =
  catMaybes <$> manyTill opCom eof

program :: Parser Program
program = do
  catMaybes <$> many opCom

opCom :: Parser (Maybe Op)
opCom =
  Just <$> operation <|> Nothing <$ (notFollowedBy (char ']') >> anyChar)

operation :: Parser Op
operation =
  Incr <$ char '+' <|>
  Decr <$ char '-' <|>
  MoveR <$ char '>' <|>
  MoveL <$ char '<' <|>
  Input <$ char ',' <|>
  Output <$ char '.' <|>
  (Loop <$> between (char '[') (char ']') program)

parseProgram :: String -> Either ParseError Program
parseProgram = parse fullProgram "(error)"
