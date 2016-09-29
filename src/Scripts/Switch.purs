module Switch where

import Prelude
import Config (Script, ScriptFn, EpiS, SystemST)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (modifySTRef, STRef, readSTRef)
import Data.Array (filter, index, length, null, updateAt) as A
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (empty, fromFoldable, insert, member, union)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, Tuple(..))
import Pattern (purgeModule, ImportObj(ImportRef, ImportModule), replaceModule, findParent, importModule, purgeScript)
import ScriptUtil (createScript, parseAndImportScript)
import System (loadLib, checkFlags, family, flagFamily)
import Util (lg, inj, randInt, numFromStringE, intFromStringE, gmod, clickPause)




switch :: forall eff h. ScriptFn eff h
switch ssRef pRef scrId t modId scrRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef scrRef
  let dt = scr.dt
  spd <- (loadLib "spd" dt "switch spd") >>= numFromStringE

  -- get the root, name of child & id of child to be switched
  op  <- loadLib "op" dt "switch op" -- either load or clone
  (Tuple rootId childN) <- case op of
    "load" -> do
      childN' <- loadLib "childN" dt "switch childN"
      return $ Tuple modId childN'
    "clone" -> findParent systemST.moduleRefPool modId
    x -> throwError $ "invalid 'op' for switch, must be load | clone : " ++ x

  by  <- loadLib "by" dt "switch by" -- either query or value
  typ <- loadLib "typ" dt "switch typ" -- either mod or idx


  -- get the relevant name to be used to either load or for the mutator
  name <- case by of
    "val" -> do
      loadLib "val" dt "switch val"
    "query" -> do
      accs <- loadLib "accs" dt "switch accs"
      query <- loadLib "query" dt "switch query"
      lib <- case typ of
        "mod" -> do
          return $ family systemST.moduleLib childN [query] [] -- using childN here is wrong - seed1, etc
        "idx" -> loadLib query systemST.indexLib "switch index" >>= \x -> return x.lib
        x -> throwError $ "invalid 'typ' for switch, must be mod | idx : " ++ x

      when (lib == []) do
        throwError "your index is empty!"

      idx <- case accs of
        "rand" -> do
          lift $ randInt $ A.length lib
        iS -> do
          i <- (return iS) >>= intFromStringE
          return $ i `gmod` (A.length lib)

      return $ fromJust (A.index lib idx)

    x -> throwError $ "invalid 'by' for switch, must be query | val : " ++ x



  -- remove self (do this so as not to be duplicated)
  purgeScript ssRef modId scrId

  let nxtN = if (op == "load") then name else modId
  nxtId <- importModule ssRef (ImportRef nxtN)

  switchModules ssRef rootId childN nxtId spd
  purgeModule ssRef nxtId

  return true


  -- load module via name (from moduleLib, needs -> name)
  -- load module via lib (from moduleLib, needs query + (rand | nxt | prev))
  -- load module via index? (from moduleLib via index, needs index name + (rand | nxt | prev))

  -- clone module + mutate needs mutator
  -- mutate via name (needs -> name)
  -- mutate via index (needs index name + (rand | nxt | prev)


  -- [mod, val]
  -- [mod, queryM, accs]
  -- [idx, queryI, accs]
  --
  -- [mutatorN]
  -- [mutatorN, val]
  -- [mutatorN, queryI, accs]

  -- so we use either val, or query + accs to get - name
  -- for load, we load a module via that name from somewhere & switch to it
  -- for clone, we clone the module & pass the name to the mutator we loaded



  -- either
  -- 1. get module from library
  -- 2. clone module + mutute somehow via library
  -- SO
  -- need a library & a way to get something from it
  --

  -- get data

  return false





incData :: forall eff h. SystemST h -> Script -> String -> (String -> String -> EpiS eff h (Array String)) -> EpiS eff h {childN :: String, nxt :: String, spd :: Number}
incData systemST scr rootId loader = do
  let dt = scr.dt

  -- get data
  idx    <- (loadLib "idx" dt "incData idx") >>= intFromStringE
  spd    <- (loadLib "spd" dt "incData spd") >>= numFromStringE
  childN <- loadLib "sub" dt "incData sub"
  lib    <- loadLib "lib" dt "incData lib"

  --let a = lg lib
  --let b = lg childN

  -- index & next data
  index <- loader childN lib

  when (A.null index) do
    throwError $ "your index doesnt exist"

  nxtPos <-
    if idx < -10000 then
      lift $ randInt $ A.length index
    else
      return $ idx `gmod` (A.length index)

  nxt <- case (A.index index nxtPos) of
    Nothing -> throwError $ "your index doesnt exist" -- doesn't look like this works
    Just v -> return v

  --let nul = lg $ "SWITCHING : " ++ rootId ++ ":" ++ childN ++ " to : " ++ nxt

  return {childN, nxt, spd}


-- increment a module within the specified families
incMod :: forall eff h. ScriptFn eff h
incMod ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {childN, nxt, spd} <- incData systemST scr rootId
    \fam flag -> return $ family systemST.moduleLib fam [flag] []

  -- remove self (do this so as not to be duplicated)
  purgeScript ssRef rootId self

  nxt'id <- importModule ssRef (ImportRef nxt)
  switchModules ssRef rootId childN nxt'id spd
  purgeModule ssRef nxt'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED

  return true


-- increment a substitution through an index
incSub :: forall eff h. ScriptFn eff h
incSub ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {childN: subVar, nxt, spd} <- incData systemST scr rootId
    \l' s' -> loadLib s' systemST.indexLib "incSub index" >>= \x -> return x.lib

  -- remove self (do this before duplicating module)
  purgeScript ssRef rootId self

  -- duplicate & switch
  rootRef <- loadLib rootId systemST.moduleRefPool "incSub module"
  root    <- lift $ readSTRef rootRef
  case (member subVar root.sub) of
    true -> do
      let sub' = insert subVar nxt root.sub
      let root' = root {sub = sub'}
      root'id <- importModule ssRef (ImportModule root') -- this is kind of hackish, as its reimported

      (Tuple parent child) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent child root'id spd
      purgeModule ssRef root'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED
      return true
    false -> do  -- HRM, I think we can do better here
      let nul' = lg "TEMP: can't find subVar!"
      return false


incScript :: forall eff h. ScriptFn eff h
incScript ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {childN: idxS, nxt, spd} <- incData systemST scr rootId
    \fam flag -> return $ flagFamily systemST.scriptLib [flag] []

  -- remove self (do this before duplicating module)
  purgeScript ssRef rootId self

  -- duplicate & switch
  mRef  <- loadLib rootId systemST.moduleRefPool "incScript module"
  m     <- lift $ readSTRef mRef
  idx   <- intFromStringE idxS
  case (A.updateAt idx nxt m.scripts) of
    Just scripts' -> do
      let m' = m {scripts = scripts'}
      m'id <- importModule ssRef (ImportModule m') -- this is kind of hackish, as its reimported

      (Tuple parent childN) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent childN m'id spd
      purgeModule ssRef m'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED
      return true
    Nothing -> do  -- HRM, maybe we want to be able to expand the number of existing scripts
      let nul' = lg "TEMP: don't have enough scripts!"
      return false

-- DOESNT WORK YET
incScript2 :: forall eff h. ScriptFn eff h
incScript2 ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef
  scr <- lift $ readSTRef sRef

  {childN: idxS, nxt, spd} <- incData systemST scr rootId
    \l' s' -> loadLib l' systemST.indexLib "incScript index" >>= \x -> return x.lib

  -- remove self (do this before duplicating module)
  purgeScript ssRef rootId self

  -- duplicate & switch
  mRef  <- loadLib rootId systemST.moduleRefPool "incScript2 module"
  m     <- lift $ readSTRef mRef
  --idx   <- intFromStringE idxS
  let idx = 0
  case (idx <= A.length m.scripts) of
    true -> do
      scrP <- traverse (handle systemST) m.scripts
      let scripts' = map fst $ A.filter (\s -> not $ checkFlags (snd s) ["path"] []) scrP
      let m' = m {scripts = scripts' ++ ["circle_0"]}
      m'id <- importModule ssRef (ImportModule m') -- this is kind of hackish, as its reimported
      --scr <- parseAndImportScript systemST m'id pattern nxt

      (Tuple parent childN) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent childN m'id spd
      purgeModule ssRef m'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED

      return true
    false -> do  -- HRM, maybe we want to be able to expand the number of existing scripts
      let nul' = lg "TEMP: don't have enough scripts!"
      return false
  where
    handle systemST scrId = do
      scRef <- loadLib scrId systemST.scriptRefPool "incScript2 handle"
      sc    <- lift $ readSTRef scRef
      return $ Tuple scrId sc

incImage :: forall eff h. ScriptFn eff h
incImage ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {childN: idxS, nxt, spd} <- incData systemST scr rootId
    \l' s' -> loadLib s' systemST.indexLib "incImage index" >>= \x -> return x.lib

  -- remove self (do this before duplicating module)
  purgeScript ssRef rootId self

  -- duplicate & switch
  mRef  <- loadLib rootId systemST.moduleRefPool "incImage module"
  m     <- lift $ readSTRef mRef
  idx   <- intFromStringE idxS
  case (A.updateAt idx nxt m.images) of
    Just images' -> do
      let m' = m {images = images'}
      m'id <- importModule ssRef (ImportModule m') -- this is kind of hackish, as its reimported

      (Tuple parent childN) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent childN m'id spd
      purgeModule ssRef m'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED

      return true
    Nothing -> do  -- HRM, maybe we want to be able to expand the number of existing images
      let nul' = lg "TEMP: don't have enough images!"
      return false

-- ABSORB THIS INTO incMod
switchChild :: forall eff h. ScriptFn eff h
switchChild ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  -- get data
  let dt = scr.dt
  spd    <- (loadLib "spd" dt "switchChild spd") >>= numFromStringE
  childN <- loadLib "childN" dt "switchChild childN"
  to     <- loadLib "to" dt "switchChild to"

  m'id <- importModule ssRef (ImportRef to)

  switchModules ssRef rootId childN m'id spd
  purgeModule ssRef m'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED

  purgeScript ssRef rootId self
  return true


-- ABSORB THIS INTO incSub
switchSub :: forall eff h. ScriptFn eff h
switchSub ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  -- get data
  let dt = scr.dt
  spd    <- (loadLib "spd" dt "switchSub spd") >>= numFromStringE
  subN   <- loadLib "subN" dt "switchSub childN"
  to     <- loadLib "to" dt "switchSub to"

  -- remove self (do this before duplicating module)
  purgeScript ssRef rootId self

  -- duplicate & switch
  rootRef <- loadLib rootId systemST.moduleRefPool "incSub module"
  root    <- lift $ readSTRef rootRef
  case (member subN root.sub) of
    true -> do
      let sub' = insert subN to root.sub
      let root' = root {sub = sub'}
      root'id <- importModule ssRef (ImportModule root') -- this is kind of hackish, as its reimported

      (Tuple parent child) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent child root'id spd
      --purgeModule ssRef root'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED
      return true
    false -> do  -- HRM, I think we can do better here
      let nul' = lg "TEMP: can't find subN!"
      return false


switchImage :: forall eff h. ScriptFn eff h
switchImage ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  -- get data
  let dt = scr.dt
  spd <- (loadLib "spd" dt "switchImage spd") >>= numFromStringE
  idx <- (loadLib "idx" dt "switchImage idx") >>= intFromStringE
  to  <- loadLib "to" dt "switchImage to"

  -- remove self (do this before duplicating module)
  purgeScript ssRef rootId self

  -- duplicate & switch
  mRef <- loadLib rootId systemST.moduleRefPool "incImage module"
  m    <- lift $ readSTRef mRef

  case (A.updateAt idx to m.images) of
    Just images' -> do
      let m' = m {images = images'}
      m'id <- importModule ssRef (ImportModule m') -- this is kind of hackish, as its reimported

      (Tuple parent childN) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent childN m'id spd
      --- SHOULDNT WE PURGE m'id HERE??????

      return true
    Nothing -> do  -- HRM, maybe we want to be able to expand the number of existing images
      let nul' = lg "TEMP: don't have enough images(switch)!"
      return false



-- should check if dim & var are the same across m0 & m1
-- m1 is a reference id
switchModules :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> Number -> EpiS eff h Unit
switchModules ssRef rootId childN m1 spd = do
  systemST <- lift $ readSTRef ssRef
  mRef  <- loadLib rootId systemST.moduleRefPool "switch module"
  m     <- lift $ readSTRef mRef
  m0    <- loadLib childN m.modules "switch find child"
  m0Ref <- loadLib m0 systemST.moduleRefPool "switch m0"
  m0M   <- lift $ readSTRef m0Ref

  m1Ref <- loadLib m1 systemST.moduleRefPool "switch m1"
  m1M   <- lift $ readSTRef m1Ref

  -- create switch module
  switch <- loadLib "smooth_switch" systemST.moduleLib "switchModules"

  let modules = fromFoldable [(Tuple "m0" m0), (Tuple "m1" m1)]
  let sub'    = union (fromFoldable [(Tuple "dim" m0M.dim), (Tuple "var" m0M.var)]) switch.sub
  let switch' = switch {sub = sub', modules = modules, var = m0M.var, dim = m0M.dim}

  swid <- replaceModule ssRef rootId childN m0 (ImportModule switch')

  -- create & import blending script
  --parseAndImportScript ssRef $ inj "finishSwitch %0 delay:%1" [swid, (show spd)]
  --parseAndImportScript ssRef $ inj "finishSwitch %0 par:intrp path:linear spd:%1" [swid, show spd]
  createScript ssRef swid "default" "finishSwitch" $ fromFoldable [(Tuple "delay" (show spd))]
  createScript ssRef swid "default" "ppath" $ fromFoldable [(Tuple "par" "intrp"), (Tuple "path" "linear"), (Tuple "spd" (show spd))]

  --systemST' <- lift $ readSTRef ssRef
  --swRef <- loadLib swid systemST'.moduleRefPool "switch module"
  --swm <- lift $ readSTRef swRef

  --let path = inj "linear %0" [show spd]
  --let paths' = insert "intrp" path swm.parPaths
  --lift $ modifySTRef swRef (\m' -> m' {parPaths = paths'})


  return unit


finishSwitch :: forall eff h. ScriptFn eff h
finishSwitch ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  delay <- (loadLib "delay" dt "finishSwitch delay") >>= numFromStringE

  case t * delay of
    -- we're done
    x | x >= 1.0 -> do
      --let a = lg "DONE SWITCHING"

      -- find parent & m1
      (Tuple parent subN) <- findParent systemST.moduleRefPool rootId
      mRef   <- loadLib rootId systemST.moduleRefPool "finishSwitch module"
      m      <- lift $ readSTRef mRef
      m1id   <- loadLib "m1" m.modules "finishSwitch module"
      m1Ref  <- loadLib m1id systemST.moduleRefPool "finishSwitch m1"
      m1     <- lift $ readSTRef m1Ref

      -- replace.  this removes all scripts wrt this as well
      replaceModule ssRef parent subN rootId (ImportModule m1)

      -- this is pretty ghetto.  its for the dev ui
      when systemST.pauseAfterSwitch do
        lift $ modifySTRef ssRef (\s -> s {pauseAfterSwitch = false})
        createScript ssRef parent "default" "pause" empty
        return unit

      return true
    _ -> do
      return false


pause :: forall eff h. ScriptFn eff h
pause ssRef pRef self t rootId sRef = do
  lift $ clickPause
  purgeScript ssRef rootId self
  return false

randomize :: forall eff h. ScriptFn eff h
randomize ssRef pRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef
  scr      <- lift $ readSTRef sRef

  dly <- (loadLib "dly" scr.dt "randomComponent") >>= numFromStringE
  spd <-  loadLib "spd" scr.dt "randomComponent"
  lib <-  loadLib "lib" scr.dt "randomComponent"
  sub <-  loadLib "sub" scr.dt "randomComponent"
  typ <-  loadLib "typ" scr.dt "randomComponent"
  adr <-  loadLib "adr" scr.dt "randomComponent"

  nxt <- case (member "nxt" scr.dt) of
    false -> return t
    true  -> (loadLib "nxt" scr.dt "randomMain1 nxt") >>= numFromStringE

  -- next iteration
  case t of
    t | t >= nxt -> do
      --let a = lg "ITERATE COMPONENT"
      let dt' = insert "nxt" (show (t + dly)) scr.dt
      lift $ modifySTRef sRef (\s -> s {dt = dt'})
      adr' <- case (adr == "!") of
        true -> return mid
        false -> return adr

      parseAndImportScript ssRef pattern adr' $ inj "inc%0 sub:%1 lib:%2 spd:%3 idx:-1000000" [typ, sub, lib, spd]

      return unit
    _ -> return unit

  return false
