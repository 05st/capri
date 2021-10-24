{-# Language LambdaCase #-}

module Codegen where

import Control.Monad.State
import Control.Monad.Except

import Syntax

type Gen = ExceptT String (StateT GenState IO)
data GenState = GenState
    { filePath :: FilePath
    , tmpVarCount :: Int
    , indent :: Int
    } deriving (Show)

generate :: FilePath -> [TypedDecl] -> IO ()
generate file decls = do
    result <- runStateT (runExceptT (initGen decls)) (GenState {
        filePath = file,
        tmpVarCount = 0,
        indent = 0
    })
    print result

append :: String -> Gen ()
append str = do
    file <- gets filePath
    lift (lift (appendFile file str))
    return ()

appendln :: String -> Gen ()
appendln str = do
    file <- gets filePath
    indents <- gets indent
    lift (lift (appendFile file (str ++ "\n")))
    return ()

write :: String -> Gen ()
write str = do
    file <- gets filePath
    lift (lift (writeFile file str))
    return ()

upIndent :: Gen ()
upIndent = do
    state <- get
    put (state {indent = indent state + 1})

downIndent :: Gen ()
downIndent = do
    state <- get
    put (state {indent = indent state - 1})

beforeStmt :: Gen String
beforeStmt = undefined

tmpVar :: Gen String
tmpVar = do
    state <- get
    let count = tmpVarCount state
    put (state {tmpVarCount = count + 1})
    return (names !! count)
    where
        names = map ('_' :) $ [1..] >>= flip replicateM ['a'..'z']

initGen :: [TypedDecl] -> Gen ()
initGen decls = do
    write "// Juno\n"
    appendln "#include <stdint.h>"
    appendln "#include <stdbool.h>"
    appendln "typedef char UNIT;"
    genTopLevelDecls decls

genType :: Type -> Gen ()
genType = \case
    TInt8 -> append "int8_t"
    TInt16 -> append "int16_t"
    TInt32 -> append "int32_t"
    TInt64 -> append "int64_t"
    TUInt8 -> append "uint8_t"
    TUInt16 -> append "uint16_t"
    TUInt32 -> append "uint32_t"
    TUInt64 -> append "uint64_t"
    TUnit -> append "UNIT"
    other -> throwError $ "Unknown type " ++ show other

genTopLevelDecls :: [TypedDecl] -> Gen ()
genTopLevelDecls = foldr ((>>) . genDecl) (return ())

genDecl :: TypedDecl -> Gen ()
genDecl = \case
    DFunc (TFunc pts rt) name params tann body -> do
        genType rt
        appendln $ " " ++ name ++ "() {"
        genExpr body
        appendln ";}"
    DVar t _ name tann expr -> do
        genType t
        append $ " " ++ name ++ " = "
        genExpr expr
        appendln ";"
    DStmt s -> genStmt s
    _ -> append "/* invalid decl */"

genStmt :: TypedStmt -> Gen ()
genStmt = \case
    SExpr e -> genExpr e >> appendln ";"
    SRet e -> append "return " >> genExpr e >> appendln ";"
    other -> throwError $ "Unknown/not implemented stmt " ++ show other

genExpr :: TypedExpr -> Gen ()
genExpr = \case
    ELit _ l -> genLit l
    EVar _ name -> append name
    EAssign _ a b -> do
        genExpr a
        append " = "
        genExpr b
    EBlock _ decls res -> do
        mapM_ genDecl decls
        genExpr res
    EIf t c a b -> do
        tvar <- tmpVar
        genType t >> appendln (" " ++ tvar ++ ";")
        append "if (" >> genExpr c >> appendln ") {"
        append $ tvar ++ " = "
        genExpr a
        appendln ";\n} else {"
        append $ tvar ++ " = "
        genExpr b
        appendln ";\n}"
        append tvar
    ECall _ fn args -> do
        genExpr fn
        append "("
        mapM_ genExpr args
        append ")"
    ECast _ t e -> do
        append "("
        genType t
        append ")"
        genExpr e
    other -> throwError $ "Unknown/not implemented expr " ++ show other

genLit :: Lit -> Gen ()
genLit = \case
    LInt n -> append (show n)
    LFloat n -> append (show n)
    LString s -> throwError "String literals not implemented"
    LChar c -> append $ '\'' : c : "'"
    LBool b -> append $ if b then "true" else "false"
    LUnit -> append "(UNIT)0";
