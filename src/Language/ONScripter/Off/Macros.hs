{-# LANGUAGE GADTs, NoMonomorphismRestriction, QuasiQuotes, TemplateHaskell #-}
module Language.ONScripter.Off.Macros (makeSmartCon) where
import Control.Monad.Operational  (Program, singleton)
import Data.Char                  (isAscii, toLower)
import Data.String
import Data.Text                  (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

makeSmartCon :: Name -> DecsQ
makeSmartCon n0 = do
  TyConI (DataD cxt n tvs cs _) <- reify n0
  concat <$> mapM (buildSingleton n tvs cxt) cs

normalize :: Name -> Name
normalize n =
  let nam = nameBase n
  in if all isAscii nam
     then mkName $ escapeKeywords $ map toLower nam
     else mkName $ '%' : nam

escapeKeywords :: String -> String
escapeKeywords "if" = "if_"
escapeKeywords "then" = "then_"
escapeKeywords "else" = "else_"
escapeKeywords "let" = "let_"
escapeKeywords "in" = "in_"
escapeKeywords "case" = "case_"
escapeKeywords "of" = "of_"
escapeKeywords "do" = "do_"
escapeKeywords "mdo" = "mdo_"
escapeKeywords "rec" = "rec_"
escapeKeywords "proc" = "proc_"
escapeKeywords "infix" = "infix_"
escapeKeywords "infixr" = "infixr_"
escapeKeywords "infixl" = "infixl_"
escapeKeywords "data" = "data_"
escapeKeywords "type" = "type_"
escapeKeywords "newtype" = "newtype_"
escapeKeywords "instance" = "instance_"
escapeKeywords "class" = "class_"
escapeKeywords "where" = "where_"
escapeKeywords "module" = "module_"
escapeKeywords "import" = "import_"
escapeKeywords "deriving" = "deriving_"
escapeKeywords a = a

type Args = ([TyVarBndr], [Pred], Name, [StrictType])

extractArgs :: Con -> Args
extractArgs = go [] []
  where
    go tvs cxt (ForallC tvs' cxt' con) =
      go (tvs ++ tvs') (cxt ++ cxt') con
    go tvs cxt (NormalC cname sts) = (tvs, cxt, cname, sts)
    go tvs cxt (RecC cname vsts)   = (tvs, cxt, cname, map (\(a,b,c) -> (b,c)) vsts)
    go tvs cxt (InfixC st1 cname st2) = (tvs, cxt, cname, [st1,st2])


buildSingleton :: Name -> [TyVarBndr] -> [Pred] -> Con -> DecsQ
buildSingleton tname tvs cxt con =
  let (tvs', cxt', cname, sts) = extractArgs con
      fname = normalize cname
  in sequence [sigD fname $ buildSig  tname tvs tvs' (cxt ++ cxt') cname sts
              ,funD fname [buildBody cname sts]
              ]

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT a ts = foldl appT a ts

tyName :: TyVarBndr -> TypeQ
tyName (PlainTV n) = varT n
tyName (KindedTV n _) = varT n

funT :: TypeQ -> TypeQ -> TypeQ
funT a b = arrowT `appT` a `appT` b

buildSig :: Name -> [TyVarBndr] -> [TyVarBndr] -> [Pred] -> Name -> [StrictType] -> TypeQ
buildSig tname tvs tvs' cxt fname sts = do
  let vs = map tyName tvs
      programT = [t| Program $(appsT (conT tname) (init vs)) $(last vs) |]
      funType = foldr (funT . return . snd) programT sts
  forallT (tvs ++ tvs') (return cxt) funType

buildBody :: Name -> [StrictType] -> ClauseQ
buildBody cname sts = do
  ps <- mapM (const $ newName "arg") sts
  clause (map varP ps) (normalB [|singleton $(appsE (conE cname:map varE ps))|]) []
