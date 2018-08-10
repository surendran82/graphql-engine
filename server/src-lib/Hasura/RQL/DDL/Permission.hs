{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DDL.Permission
    ( CreatePerm
    , SetPermComment(..)
    , purgePerm
    , PermDef(..)


    , InsPerm(..)
    , InsPermDef
    , CreateInsPerm
    , clearInsInfra
    , buildInsInfra
    , buildInsPermInfo
    , DropInsPerm
    , dropInsPermP2

    , SelPerm(..)
    , SelPermDef
    , CreateSelPerm
    , buildSelPermInfo
    , DropSelPerm
    , dropSelPermP2

    , UpdPerm(..)
    , UpdPermDef
    , CreateUpdPerm
    , buildUpdPermInfo
    , DropUpdPerm
    , dropUpdPermP2

    , DelPerm(..)
    , DelPermDef
    , CreateDelPerm
    , buildDelPermInfo
    , DropDelPerm
    , dropDelPermP2

    , IsPerm(..)
    , addPermP1
    , addPermP2
    ) where

import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DML.Internal            (onlyPositiveInt)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query                  as Q
import qualified Hasura.SQL.DML                     as S

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Data.ByteString.Builder            as BB
import qualified Data.HashMap.Strict                as M
import qualified Data.HashSet                       as HS
import qualified Data.Text                          as T

-- Insert permission
data InsPerm
  = InsPerm
  { icCheck       :: !BoolExp
  , icAllowUpsert :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''InsPerm)

type InsPermDef = PermDef InsPerm
type CreateInsPerm = CreatePerm InsPerm

buildFnName :: QualifiedTable -> QualifiedTable
buildFnName (QualifiedTable sn tn) =
  QualifiedTable hdbViewsSchema $ TableName
  (snTxt <> "__" <> tnTxt)
  where
    hdbViewsSchema = SchemaName "hdb_views"
    snTxt = getSchemaTxt sn
    tnTxt = getTableTxt tn

buildInsTrig :: QualifiedTable -> Q.Query
buildInsTrig qt =
  Q.fromBuilder $ mconcat
  [ BB.string7 "CREATE TRIGGER __hasura_insert_permissions"
  , BB.string7 " BEFORE INSERT ON " <> toSQL qt
  , BB.string7 " FOR EACH ROW EXECUTE PROCEDURE "
  , toSQL (buildFnName qt) <> BB.string7 "();"
  ]

dropInsTrigFn :: QualifiedTable -> Q.Query
dropInsTrigFn tn =
  Q.fromBuilder $
  BB.string7 "DROP FUNCTION IF EXISTS " <> toSQL (buildFnName tn)
  <> "() CASCADE"

buildInsTrigFn :: Int -> QualifiedTable -> TableInsPerms -> Q.Query
buildInsTrigFn pgVer tn insPerms =
  Q.fromBuilder $ mconcat
  [ BB.string7 "CREATE FUNCTION " <> toSQL fn
  , BB.string7 "() RETURNS trigger LANGUAGE plpgsql AS $$ "
  , BB.string7 "DECLARE hasura_role text;"
  , BB.string7 "BEGIN "
  , roleFrag
  , BB.string7 "CASE WHEN (hasura_role IS NULL) THEN return NEW;"
  , mconcat $ map roleCase insPerms
  , BB.string7 "ELSE RAISE EXCEPTION 'unexpected role: %', hasura_role"
  , BB.string7 "      USING HINT = 'this a hasura bug, contact support';"
  , BB.string7 " END CASE; "

  , BB.string7 "END "
  , BB.string7 "$$;"
  ]
  where
    fn = buildFnName tn

    roleCase (rn, be) =
      "WHEN (hasura_role = " <> toSQL (S.SELit $ getRoleTxt rn) <> ") THEN " <>
      roleCheck be

    roleCheck be =
      mconcat
      [ BB.string7 "IF (" <> toSQL (ipiCheck be) <> BB.string7 ") "
      , BB.string7 "THEN RETURN NEW;"
      , BB.string7 "ELSE RAISE check_violation using message = 'insert check constraint failed'; return NULL;"
      , BB.string7 "END IF; "
      ]

    roleFrag =
      if pgVer >= 96000
      then "hasura_role := current_setting('hasura.role', 't');"
      else mconcat $ map BB.string7
           [ "BEGIN"
           , "  hasura_role := current_setting('hasura.role')::text;"
           , "EXCEPTION WHEN OTHERS THEN"
           , "  hasura_role = NULL;"
           , "END;"
           ]

buildInsPermInfo
  :: (QErrM m, CacheRM m)
  => TableInfo
  -> PermDef InsPerm
  -> m InsPermInfo
buildInsPermInfo tabInfo (PermDef _ (InsPerm chk upsrt) _) = do
  (be, beDeps) <- withPathK "check" $
    procBoolExp tn fieldInfoMap (S.QualVar "NEW") chk
  let deps = mkParentDep tn : beDeps
      depHeaders = getDependentHeaders chk
  return $ InsPermInfo tn be (fromMaybe False upsrt) deps depHeaders
  where
    fieldInfoMap = tiFieldInfoMap tabInfo
    tn = tiName tabInfo

buildInsInfra :: QualifiedTable -> TableInsPerms -> Q.TxE QErr ()
buildInsInfra tn insPerms =
  Q.catchE defaultTxErrorHandler $ do
    Q.unitQ (dropInsTrigFn tn) () False
    serverVer <- Q.serverVersion
    Q.unitQ (buildInsTrigFn serverVer tn insPerms) () False
    -- Add trigger to the table
    Q.unitQ (buildInsTrig tn) () False

clearInsInfra :: QualifiedTable -> TableInsPerms -> Q.TxE QErr ()
clearInsInfra tn insPerms =
  bool (buildInsInfra tn insPerms) dropTrigFn $ null insPerms
  where
    dropTrigFn =
      Q.catchE defaultTxErrorHandler $
      Q.unitQ (dropInsTrigFn tn) () False

type DropInsPerm = DropPerm InsPerm

type instance PermInfo InsPerm = InsPermInfo

type TableInsPerms = [(RoleName, InsPermInfo)]

dropInsPermP2 :: (P2C m) => DropInsPerm -> m ()
dropInsPermP2 = dropPermP2

instance IsPerm InsPerm where

  permAccessor = PAInsert

  buildPermInfo = buildInsPermInfo

  addPermP2Setup tn _ tabInfo = do
    let insPerms = catMaybes
           [ (r,) <$> _permIns p
           | (r, p) <- M.toList $ tiRolePermInfoMap tabInfo
           ]
    liftTx $ buildInsInfra tn insPerms

  dropPermP2Setup dp ti = do
    let insPerms = catMaybes
          [ (r,) <$> _permIns p
          | (r, p) <- M.toList $ tiRolePermInfoMap ti
          ]
    liftTx $ clearInsInfra (dipTable dp) insPerms

-- Select constraint
data SelPerm
  = SelPerm
  { spColumns :: !PermColSpec       -- Allowed columns
  , spFilter  :: !BoolExp   -- Filter expression
  , spLimit   :: !(Maybe Int) -- Limit value
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''SelPerm)

buildSelPermInfo
  :: (QErrM m, CacheRM m)
  => TableInfo
  -> SelPerm
  -> m SelPermInfo
buildSelPermInfo tabInfo sp = do
  let pgCols     = convColSpec fieldInfoMap $ spColumns sp

  (be, beDeps) <- withPathK "filter" $
    procBoolExp tn fieldInfoMap (S.mkQual tn) $ spFilter sp

  -- check if the columns exist
  void $ withPathK "columns" $ indexedForM pgCols $ \pgCol ->
    askPGType fieldInfoMap pgCol autoInferredErr

  let deps = mkParentDep tn : beDeps ++ map (mkColDep "untyped" tn) pgCols
      depHeaders = getDependentHeaders $ spFilter sp
      mLimit = spLimit sp

  withPathK "limit" $ mapM_ onlyPositiveInt mLimit

  return $ SelPermInfo (HS.fromList pgCols) tn be mLimit deps depHeaders

  where
    tn = tiName tabInfo
    fieldInfoMap = tiFieldInfoMap tabInfo
    autoInferredErr = "permissions for relationships are automatically inferred"

type SelPermDef = PermDef SelPerm
type CreateSelPerm = CreatePerm SelPerm
type DropSelPerm = DropPerm SelPerm

type instance PermInfo SelPerm = SelPermInfo

dropSelPermP2 :: (P2C m) => DropSelPerm -> m ()
dropSelPermP2 = dropPermP2

instance IsPerm SelPerm where

  permAccessor = PASelect

  buildPermInfo ti (PermDef _ a _) =
    buildSelPermInfo ti a

  addPermP2Setup _ _ _ = return ()

  dropPermP2Setup _ _ = return ()

-- Update constraint
data UpdPerm
  = UpdPerm
  { ucColumns :: !PermColSpec -- Allowed columns
  , ucFilter  :: !BoolExp     -- Filter expression
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''UpdPerm)

type UpdPermDef = PermDef UpdPerm
type CreateUpdPerm = CreatePerm UpdPerm

buildUpdPermInfo
  :: (QErrM m, CacheRM m)
  => TableInfo
  -> UpdPerm
  -> m UpdPermInfo
buildUpdPermInfo tabInfo (UpdPerm colSpec fltr) = do
  (be, beDeps) <- withPathK "filter" $
    procBoolExp tn fieldInfoMap (S.mkQual tn) fltr

  -- check if the columns exist
  _ <- withPathK "columns" $ indexedForM updCols $ \updCol ->
       askPGType fieldInfoMap updCol relInUpdErr

  let deps = mkParentDep tn : beDeps ++ map (mkColDep "untyped" tn) updCols
      depHeaders = getDependentHeaders fltr

  return $ UpdPermInfo (HS.fromList updCols) tn be deps depHeaders

  where
    tn = tiName tabInfo
    fieldInfoMap = tiFieldInfoMap tabInfo
    updCols     = convColSpec fieldInfoMap colSpec
    relInUpdErr = "relationships can't be used in update"

type instance PermInfo UpdPerm = UpdPermInfo

type DropUpdPerm = DropPerm UpdPerm

dropUpdPermP2 :: (P2C m) => DropUpdPerm -> m ()
dropUpdPermP2 = dropPermP2

instance IsPerm UpdPerm where

  permAccessor = PAUpdate

  buildPermInfo ti (PermDef _ a _) =
    buildUpdPermInfo ti a

  addPermP2Setup _ _ _ = return ()

  dropPermP2Setup _ _ = return ()

-- Delete permission
data DelPerm
  = DelPerm { dcFilter :: !BoolExp }
  deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DelPerm)

type DelPermDef = PermDef DelPerm
type CreateDelPerm = CreatePerm DelPerm

buildDelPermInfo
  :: (QErrM m, CacheRM m)
  => TableInfo
  -> DelPerm
  -> m DelPermInfo
buildDelPermInfo tabInfo (DelPerm fltr) = do
  (be, beDeps) <- withPathK "filter" $
    procBoolExp tn fieldInfoMap  (S.mkQual tn) fltr
  let deps = mkParentDep tn : beDeps
      depHeaders = getDependentHeaders fltr
  return $ DelPermInfo tn be deps depHeaders
  where
    tn = tiName tabInfo
    fieldInfoMap = tiFieldInfoMap tabInfo

type DropDelPerm = DropPerm DelPerm

dropDelPermP2 :: (P2C m) => DropDelPerm -> m ()
dropDelPermP2 = dropPermP2

type instance PermInfo DelPerm = DelPermInfo

instance IsPerm DelPerm where

  permAccessor = PADelete

  buildPermInfo ti (PermDef _ a _) =
    buildDelPermInfo ti a

  addPermP2Setup _ _ _ = return ()

  dropPermP2Setup _ _ = return ()

data SetPermComment
  = SetPermComment
  { apTable      :: !QualifiedTable
  , apRole       :: !RoleName
  , apPermission :: !PermType
  , apComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase) ''SetPermComment)

setPermCommentP1 :: (P1C m) => SetPermComment -> m ()
setPermCommentP1 (SetPermComment qt rn pt _) = do
  adminOnly
  tabInfo <- askTabInfo qt
  action tabInfo
  where
    action tabInfo = case pt of
      PTInsert -> assertPermDefined rn PAInsert tabInfo
      PTSelect -> assertPermDefined rn PASelect tabInfo
      PTUpdate -> assertPermDefined rn PAUpdate tabInfo
      PTDelete -> assertPermDefined rn PADelete tabInfo

setPermCommentP2 :: (P2C m) => SetPermComment -> m RespBody
setPermCommentP2 apc = do
  liftTx $ setPermCommentTx apc
  return successMsg

instance HDBQuery SetPermComment where

  type Phase1Res SetPermComment = ()
  phaseOne = setPermCommentP1

  phaseTwo q _ = setPermCommentP2 q

  schemaCachePolicy = SCPNoChange

setPermCommentTx
  :: SetPermComment
  -> Q.TxE QErr ()
setPermCommentTx (SetPermComment (QualifiedTable sn tn) rn pt comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.hdb_permission
           SET comment = $1
           WHERE table_schema =  $2
             AND table_name = $3
             AND role_name = $4
             AND perm_type = $5
                |] (comment, sn, tn, rn, permTypeToCode pt) True

purgePerm :: (P2C m) => QualifiedTable -> RoleName -> PermType -> m ()
purgePerm qt rn pt =
  case pt of
    PTInsert -> dropInsPermP2 dp
    PTSelect -> dropSelPermP2 dp
    PTUpdate -> dropUpdPermP2 dp
    PTDelete -> dropDelPermP2 dp
  where
    dp :: DropPerm a
    dp = DropPerm qt rn
