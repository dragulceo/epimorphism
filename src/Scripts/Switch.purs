module Switch where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (modifySTRef, readSTRef)
import Control.Monad.Trans.Class (lift)
import Data.Array (index, length, updateAt) as A
import Data.Library (apD, buildSearch, component, dat, family, getLib, getPattern, idM, mD, modLibD, searchLib)
import Data.Library (idx) as L
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Set (singleton)
import Data.StrMap (insert, fromFoldable, union)
import Data.Tuple (Tuple(..))
import Data.Types (Component(..), EpiS, Family(..), Module(..), ModuleD, Section(..), Library, PMut(..), ScriptRes(ScriptRes), ScriptFn)
import Pattern (CloneRes(CloneRes), purgeModule, ImportObj(ImportRef, ImportModule), replaceModule, findParent, importModule)
import ScriptUtil (getClone, addScript, purgeScript)
import System (loadLib)
import Text.Format (precision, format)
import Util (log, intFromStringE, inj, randInt, numFromStringE, gmod, fromJustE)


-- CLEAN THIS UP
switch :: forall eff h. ScriptFn eff h
switch ssRef lib t midPre self dt = do
  lift $ log $ "BEGININNG SWITCH FOR: " <> midPre
  pattern' <- getPattern lib "switch pattern"
  CloneRes newRootN pattern mid <- getClone lib pattern' midPre

  systemST <- lift $ readSTRef ssRef

  spd <- (loadLib "spd" dt "switch spd") >>= numFromStringE

  -- get the root, name of child & id of child to be switched
  op  <- loadLib "op" dt "switch op" -- either load or clone
  (Tuple rootId childN) <- case op of
    "load" -> do
      childN' <- loadLib "childN" dt "switch childN"
      pure $ Tuple mid childN'
    "clone" -> do
      findParent lib (dat pattern) mid
    x -> throwError $ "invalid 'op' for switch, must be load | clone : " <> x

  -- get the relevant name to be used to either load or for the mutator
  by <- loadLib "by" dt "switch by" -- either query or value
  name <- case by of
    "val" -> do
      loadLib "val" dt "switch val"
    "query" -> do
      accs <- loadLib "accs" dt "switch accs"
      query <- loadLib "query" dt "switch query"
      typ <- loadLib "typ" dt "switch typ" -- either mod or idx
      lib' <- case typ of
        "mod" -> do
          mod <- idM <$> getLib lib rootId "switch mod"
          (Component _ comp) <- component lib mod
          childT <- loadLib childN comp.children "switch child type"
          let search = buildSearch [query] ["live"] [Tuple "family" childT]

          res <- searchLib lib search
          pure $ map (\x -> (L.idx x).id) (res :: Array Module)
        "idx" -> do
          (Section _ index) <- getLib lib query "switch index"
          pure $ index.lib
        x -> throwError $ "invalid 'typ' for switch, must be mod | idx : " <> x

      when (lib' == []) do
        throwError "your index is empty!"

      idx' <- case accs of
        "rand" -> do
          lift $ randInt $ A.length lib'
        iS -> do
          i <- (pure iS) >>= intFromStringE
          pure $ i `gmod` (A.length lib')

      fromJustE (A.index lib' idx') "idx out of bounds in switch"

    x -> throwError $ "invalid 'by' for switch, must be query | val : " <> x

  -- import new module
  lift $ log $ "SWITCH REMOVING SELF SCRIPT @" <> self
  purgeScript lib mid self
  let nxtN = if (op == "load") then name else mid
  nxtId <- importModule lib (ImportRef nxtN)

  -- if cloning, perform mutation
  when (op == "clone") do
    nxt <- idM <$> getLib lib nxtId "switch nxtId"
    idx' <- loadLib "idx" dt "switch idx"

    mutatorN <- loadLib "mut" dt "switch mut"
    mutator <- getMutator mutatorN idx' name

    modLibD lib nxt mutator

    pure unit

  -- switch! (should we inline this?)
  switchModules lib systemST.t rootId childN nxtId spd

  pure $ ScriptRes (PMut (dat pattern) (singleton newRootN)) Nothing


getMutator :: forall eff h. String -> String -> String -> EpiS eff h (ModuleD -> ModuleD)
getMutator mut idx name  = do
  case mut of
    "image" -> do
      idx' <- intFromStringE idx
      pure $ \mod ->
        let images' = fromMaybe mod.images (A.updateAt idx' name mod.images) in
        mod {images = images'}
    "script" -> do
      idx' <- intFromStringE idx
      pure $ \mod ->
        let scripts' = fromMaybe mod.scripts (A.updateAt idx' name mod.scripts) in
        mod {scripts = scripts'}
    "sub" -> do
      pure $ \mod ->
        let sub' = insert idx name mod.sub in
        mod {sub = sub'}
    _ -> throwError $ "unknown mutator: " <> name


-- should check if dim & var are  the same across m0 & m1
-- m1 is a reference id(we assume also that it was previously imported & floating)
switchModules :: forall eff h. Library h -> Number -> String -> String -> String -> Number -> EpiS eff h Unit
switchModules lib sysT rootId childN m1 spd = do
  modD  <- mD <$> getLib lib rootId "switch module"
  m0    <- loadLib childN modD.modules "switch find child"
  mod0@(Module _ mod0D) <- getLib lib m0 "switch m0"
  mod1D <- mD <$> getLib lib m1 "switch m0"

  (Family _ fm) <- family lib mod0
  dim <- case (fm.dim) of
    0 -> pure ""
    1 -> pure "float"
    2 -> pure "vec2"
    4 -> pure "vec4"
    _ -> throwError $ "invalid dim: " <> (show fm.dim)

  -- create switch module
  switchMod@(Module _ switchModD) <- getLib lib "smooth_switch" "switchMod"

  let modules = fromFoldable [(Tuple "m0" m0), (Tuple "m1" m1)]
  let sub'    = union (fromFoldable [(Tuple "dim" dim), (Tuple "var" fm.var)]) switchModD.sub
  let path    = inj "linear@%0 %1" [(format (precision 2) sysT), (format (precision 2) spd)]
  let par     = fromFoldable [(Tuple "intrp" path)]
  let switch' = apD switchMod _ {par=par, sub = sub', modules = modules}

  swid <- replaceModule lib rootId childN m0 (ImportModule switch')
  purgeModule lib m1 -- we assume this was imported previously, so it was imported again by replace

  -- create & import blending script
  addScript lib sysT swid "finishSwitch" (inj "delay:%0" [show spd])

  pure unit


finishSwitch :: forall eff h. ScriptFn eff h
finishSwitch ssRef lib t rootIdPre self dt = do
  pattern' <- getPattern lib "switch pattern"

  -- get data
  delay <- (loadLib "delay" dt "finishSwitch delay") >>= numFromStringE

  case t * delay of
    -- we're done
    x | x >= 1.0 -> do
      --let a = lg "DONE SWITCHING"
      CloneRes newRootN pattern rootId <- getClone lib pattern' rootIdPre

      systemST <- lift $ readSTRef ssRef

      -- find parent & m1
      (Tuple parent subN) <- findParent lib (dat pattern) rootId

      modD <- mD <$> getLib lib rootId "finishSwitch module"
      m1id <- loadLib "m1" modD.modules "finishSwitch module m1"
      m1 <- getLib lib m1id "finishSwitch m1"

      -- replace.  this removes all scripts wrt this as well
      replaceModule lib parent subN rootId (ImportModule m1)

      -- this is pretty ghetto.  its for the dev ui
      when systemST.pauseAfterSwitch do
        lift $ modifySTRef ssRef (\s -> s {pauseAfterSwitch = false})
        addScript lib t parent "pause" ""
        pure unit

      lift $ log "Finish Switch"
      pure $ ScriptRes (PMut (dat pattern) (singleton newRootN)) Nothing
    _ -> do
      pure $ ScriptRes PMutNone Nothing
