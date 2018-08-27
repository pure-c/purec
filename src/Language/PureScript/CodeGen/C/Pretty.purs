module Language.PureScript.CodeGen.C.Pretty
  ( empty
  , prettyPrint
  , PrintError(..)
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..), ask, local, runReaderT)
import Control.Monad.State (StateT(..), evalStateT)
import Control.Monad.Writer (class MonadWriter, WriterT(..), execWriterT, mapWriterT, runWriterT, tell)
import Data.Array as A
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as Str
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for_, traverse, traverse_)
import Effect.Aff (bracket)
import Language.PureScript.CodeGen.C.AST (AST, PrimitiveType, Type, ValueQualifier)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.Common (dotsTo, safeName)
import Language.PureScript.CodeGen.C.Constants as C

data PrintError
  = NotImplementedError String
  | InternalError String

empty :: AST
empty = AST.Raw ""

lf :: ∀ m. Monad m => PrinterT m
lf = tell [ "\n" ]

type PrinterState =
  { indent :: Int
  }

type PrinterT m =
  WriterT
    (Array String)
    (ReaderT PrinterState (ExceptT PrintError m))
    Unit

prettyPrint
  :: Array AST
  -> Either PrintError String
prettyPrint asts =
  let
    x :: Identity (Either _ _)
    x =
      runPrinterT $
        for_ asts \ast ->
          indent *> prettyPrintAst ast *> lf
  in unwrap x

runPrinterT
  :: ∀ m
   . Monad m
  => PrinterT m
  -> m (Either PrintError String)
runPrinterT action =
  rmap (A.intercalate "") <$> do
    runExceptT $
      runReaderT <@> { indent: 0 } $
        execWriterT action

prettyPrintAst
  :: ∀ m
   . Monad m
  => AST
  -> PrinterT m
prettyPrintAst (AST.Raw x) = do
  emit x
prettyPrintAst (AST.Include { path }) = do
  emit $ "#include \"" <> path <> ".h\""
prettyPrintAst AST.EndOfHeader =
  pure unit
prettyPrintAst (AST.Enum { name, members }) = do
  emit "enum"
  for_ name \x -> emit $ " "   <> x
  emit " { "
  emit $ A.intercalate ", " members
  emit " }"
prettyPrintAst (AST.VariableIntroduction { name, type: typ, qualifiers, initialization }) = do
  unless (A.null qualifiers) do
    emit $ A.intercalate " " $ map renderValueQualifier qualifiers
    emit " "
  emit $ renderType typ
  emit " "
  emit name
  for_ initialization \ast -> do
    emit " ="
    lf
    withNextIndent do
      indent
      prettyPrintAst ast
  emit ";"
prettyPrintAst (AST.NumericLiteral (Left n)) =
  emit $ show n
prettyPrintAst (AST.NumericLiteral (Right n)) =
  emit $ show n
prettyPrintAst (AST.StringLiteral s) =
  emit $ show s
prettyPrintAst (AST.CharLiteral c)
  | isAscii c
  = emit $ "'" <> encodeChar c <> "'"
prettyPrintAst (AST.Accessor field o)
  = do
  prettyPrintAst o
  emit $ "->"
  prettyPrintAst field
prettyPrintAst (AST.Lambda
  { arguments
  , returnType
  , body
  }) = do
  emit "PURS_ANY_BLOCK("
  emit "("
  for_ (A.init arguments) $ traverse \arg -> do
    emit $ renderArg arg
    emit ","
  for_ (A.last arguments) \arg ->
    emit $ renderArg arg
  emit ") "
  prettyPrintAst body
  emit ")"
  where
  renderArg { name, type: typ } =
    renderType typ <> " " <> name
prettyPrintAst (AST.Function
  { name
  , arguments
  , returnType
  , qualifiers
  , body
  }) = do
  emit $ renderType returnType
  emit " "
  emit name
  emit "("
  for_ (A.init arguments) $ traverse \arg -> do
    emit $ renderArg arg
    emit ","
  for_ (A.last arguments) \arg ->
    emit $ renderArg arg
  emit ")"
  case body of
    Just ast -> do
      emit " "
      prettyPrintAst ast
    Nothing ->
      emit ";"
  lf
  where
  renderArg { name, type: typ } =
    renderType typ <> " " <> name
prettyPrintAst (AST.Cast typ ast) = do
  emit "(("
  emit $ renderType typ
  emit ") "
  prettyPrintAst ast
  emit ")"
prettyPrintAst (AST.App fnAst argsAsts) = do
  prettyPrintAst fnAst
  case A.unsnoc argsAsts of
    Nothing ->
      emit "()"
    Just { init, last } -> do
      emit "("
      lf
      withNextIndent do
        for_ init \ast -> do
          indent *> prettyPrintAst ast
          emit ","
          lf
        indent *> prettyPrintAst last
      lf
      indent *> emit ")"
prettyPrintAst (AST.Assignment l r) = do
  prettyPrintAst l
  emit " ="
  lf
  withNextIndent $
    indent *> prettyPrintAst r
prettyPrintAst (AST.Indexer v k) = do
  prettyPrintAst k
  emit "["
  prettyPrintAst v
  emit "]"
prettyPrintAst (AST.StructLiteral o) = do
  emit "{"
  withNextIndent do
    lf
    traverseWithIndex_ <@> o $ \k v -> do
      indent *> do emit $ "." <> k <> " ="
      withNextIndent do
        lf
        indent *> prettyPrintAst v
        emit ","
        lf
    lf
  emit "}"
prettyPrintAst (AST.IfElse condAst thenAst mElseAst) = do
  emit "if ("
  prettyPrintAst condAst
  emit ")"
  lf
  withNextIndent do
    indent
    prettyPrintAst thenAst
  for_ mElseAst \elseAst -> do
    lf
    emit " else "
    lf
    withNextIndent do
      indent
      prettyPrintAst elseAst
prettyPrintAst (AST.Block asts) = do
  emit "{"
  lf
  withNextIndent $
    for_ asts \ast ->
      indent *> prettyPrintAst ast *> emit ";" *> lf
  indent *> emit "}"
prettyPrintAst (AST.Return ast) = do
  emit "return "
  prettyPrintAst ast
prettyPrintAst (AST.Var name) = do
  emit $ renderName name
prettyPrintAst (AST.Binary op lhsAst rhsAst) = do
  emit "("
  prettyPrintAst lhsAst
  emit ") "
  emit
    case op of
      AST.Add                  -> "+"
      AST.Subtract             -> "-"
      AST.Multiply             -> "*"
      AST.Divide               -> "/"
      AST.Modulus              -> "%"
      AST.EqualTo              -> "=="
      AST.NotEqualTo           -> "!="
      AST.LessThan             -> "<"
      AST.LessThanOrEqualTo    -> "<="
      AST.GreaterThan          -> ">"
      AST.GreaterThanOrEqualTo -> ">="
      AST.And                  -> "&&"
      AST.Or                   -> "||"
      AST.BitwiseAnd           -> "&"
      AST.BitwiseOr            -> "|"
      AST.BitwiseXor           -> "^"
      AST.ShiftLeft            -> "<<"
      AST.ShiftRight           -> ">>"
  emit " ("
  prettyPrintAst rhsAst
  emit ")"
prettyPrintAst AST.NoOp =
  pure unit
prettyPrintAst AST.Null =
  emit "NULL"
prettyPrintAst (AST.DefineTag name tag) =
  emit $ "#define " <> name <> " " <> show tag
prettyPrintAst x = do
  lf
  emit ("xTODO: " <> show x)
  lf
  pure unit -- throwError $ NotImplementedError $ show x

emit
  :: ∀ m
   . Monad m
  => String
  -> PrinterT m
emit x = tell [ x ]

indent
  :: ∀ m
   . Monad m
  => PrinterT m
indent = do
  { indent } <- ask
  emit $ CodeUnits.fromCharArray $ A.replicate indent ' '

withNextIndent
  :: ∀ m
   . Monad m
  => PrinterT m
  -> PrinterT m
withNextIndent =
  local (\st -> st { indent = st.indent + 2 })

encodeChar :: Char -> String
encodeChar '\0' = "\\0"
encodeChar '\b' = "\\b"
encodeChar '\t' = "\\t"
encodeChar '\n' = "\\n"
encodeChar '\v' = "\\v"
encodeChar '\f' = "\\f"
encodeChar '\r' = "\\r"
encodeChar '"'  = "\\\""
encodeChar '\'' = "\\'"
encodeChar '\\' = "\\\\"
-- TODO (implement: ctrl chrs):
-- encodeChar c | isControl c = T.pack $ "\\x" ++ showHex (fromEnum c) ""
encodeChar c = CodeUnits.singleton c

-- TODO (implement)
isAscii :: Char -> Boolean
isAscii c = true

renderName :: String -> String
renderName name = name

renderType :: Type -> String
renderType = case _ of
  Type.Pointer t ->
    renderType t <> " *"
  Type.Any qs ->
    renderTypeQualifiers qs <>
      "purs_any_t"
  Type.RawType name qs ->
    renderTypeQualifiers qs <>
      name
  Type.Primitive t qs ->
    renderTypeQualifiers qs <>
      renderPrimitiveType t
  where
  renderTypeQualifiers qs =
    A.intercalate " " (map renderTypeQualifier qs) <>
      if A.null qs
        then ""
        else " "
  renderTypeQualifier Type.Const = "const"
  renderTypeQualifier Type.BlockStorage = "__block"

renderPrimitiveType :: PrimitiveType -> String
renderPrimitiveType Type.Int = "int"
renderPrimitiveType Type.Void = "void"

renderValueQualifier :: ValueQualifier -> String
renderValueQualifier _ = "" -- TODO
