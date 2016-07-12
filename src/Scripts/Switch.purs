module Switch where

import Prelude
import Command (ScrPS(ScrFn), parseScript)
import Config (Script, ScriptFn, EpiS, SystemST)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, readSTRef)
import Data.Array (foldM, index, length, null, updateAt) as A
import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (fromFoldable, insert, member, union)
import Data.String (split)
import Data.Tuple (Tuple(..))
import Pattern (purgeModule, importScript, ImportObj(ImportScript, ImportModule), replaceModule, findParent, importModule, purgeScript, flagFamily)
import ScriptUtil (createScript)
import System (loadLib)
import Util (randInt, lg, numFromStringE, intFromStringE, gmod)

incData :: forall eff h. SystemST h -> Script -> String -> (String -> String -> EpiS eff h (Array String)) -> EpiS eff h {childN :: String, nxt :: String, dim :: String, spd :: Number}
incData systemST scr rootId loader = do
  let dt = scr.dt

  -- get data
  idx    <- (loadLib "idx" dt "incMod idx") >>= intFromStringE
  spd    <- (loadLib "spd" dt "incMod spd") >>= numFromStringE
  childN <- loadLib "sub" dt "incMod sub"
  lib    <- loadLib "lib" dt "incMod lib"
  dim    <- loadLib "dim" dt "incMod dim"

  --let a = lg lib
  --let b = lg childN

  -- index & next data
  index <- loader lib childN

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

  return {childN, nxt, dim, spd}


-- increment a module within the specified families
incMod :: forall eff h. ScriptFn eff h
incMod ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {childN, nxt, dim, spd} <- incData systemST scr rootId
    \l' s' -> return $ flagFamily systemST.moduleLib $ fromFoldable [(Tuple "family" s'), (Tuple l' "true")]

  switchModules ssRef rootId childN nxt dim spd

  -- remove self
  purgeScript ssRef self

  return true


-- increment a substitution through an index
incSub :: forall eff h. ScriptFn eff h
incSub ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {childN: subVar, nxt, dim, spd} <- incData systemST scr rootId
    \l' s' -> loadLib l' systemST.indexLib "incSub index" >>= \x -> return x.lib

  -- remove self (do this before duplicating module)
  purgeScript ssRef self

  -- duplicate & switch
  rootRef <- loadLib rootId systemST.moduleRefPool "incSub module"
  root    <- lift $ readSTRef rootRef
  case (member subVar root.sub) of
    true -> do
      let sub' = insert subVar nxt root.sub
      let root' = root {sub = sub'}
      root'id <- importModule ssRef (ImportModule root') -- this is kind of hackish, as its reimported

      (Tuple parent child) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent child root'id dim spd
      purgeModule ssRef root'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED
      return true
    false -> do  -- HRM, I think we can do better here
      let nul' = lg "TEMP: can't find subVar!"
      return false


incScript :: forall eff h. ScriptFn eff h
incScript ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {childN: idxS, nxt, dim, spd} <- incData systemST scr rootId
    \l' s' -> return $ flagFamily systemST.scriptLib $ fromFoldable [(Tuple l' "true")]

  -- remove self (do this before duplicating module)
  purgeScript ssRef self

  -- duplicate & switch
  mRef  <- loadLib rootId systemST.moduleRefPool "incScript2 module"
  m     <- lift $ readSTRef mRef
  idx   <- intFromStringE idxS
  case (A.updateAt idx nxt m.scripts) of
    Just scripts' -> do
      let m' = m {scripts = scripts'}
      m'id <- importModule ssRef (ImportModule m') -- this is kind of hackish, as its reimported

      (Tuple parent childN) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent childN m'id dim spd
      purgeModule ssRef m'id --  THIS IS REALLY TRICKY!  WILL CAUSE MEMORY LEAK IF NOT PURGED
      return true
    Nothing -> do  -- HRM, maybe we want to be able to expand the number of existing scripts
      let nul' = lg "TEMP: don't have enough scripts!"
      return false


incScript2 :: forall eff h. ScriptFn eff h
incScript2 ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef
  scr <- lift $ readSTRef sRef

  {childN: idxS, nxt, dim, spd} <- incData systemST scr rootId
    \l' s' -> loadLib l' systemST.indexLib "incScript index" >>= \x -> return x.lib

  -- remove self (do this before duplicating module)
  purgeScript ssRef self

  -- duplicate & switch
  mRef  <- loadLib rootId systemST.moduleRefPool "incScript module"
  m     <- lift $ readSTRef mRef
  idx   <- intFromStringE idxS

  case (idx < A.length m.scripts) of
    true -> do
      def <- loadLib "default" systemST.scriptLib "building script in inc"
      let dt = split " " nxt
      Tuple scr' _ <- A.foldM (parseScript systemST.moduleRefPool pattern) (Tuple def ScrFn) dt

      m'id <- importModule ssRef (ImportModule m) -- this is kind of hackish, as its reimported
      let scr'' = scr' {mid = m'id}
      importScript ssRef (ImportScript scr'') m'id

      (Tuple parent childN) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent childN m'id dim spd

      return true
    false -> do  -- HRM, maybe we want to be able to expand the number of existing scripts
      let nul' = lg "TEMP: don't have enough scripts!"
      return false

incImage :: forall eff h. ScriptFn eff h
incImage ssRef pRef self t rootId sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {childN: idxS, nxt, dim, spd} <- incData systemST scr rootId
    \l' s' -> loadLib l' systemST.indexLib "incImage index" >>= \x -> return x.lib

  -- remove self (do this before duplicating module)
  purgeScript ssRef self

  -- duplicate & switch
  mRef  <- loadLib rootId systemST.moduleRefPool "incImage module"
  m     <- lift $ readSTRef mRef
  idx   <- intFromStringE idxS
  case (A.updateAt idx nxt m.images) of
    Just images' -> do
      let m' = m {images = images'}
      m'id <- importModule ssRef (ImportModule m') -- this is kind of hackish, as its reimported

      (Tuple parent childN) <- findParent systemST.moduleRefPool rootId
      switchModules ssRef parent childN m'id dim spd
      return true
    Nothing -> do  -- HRM, maybe we want to be able to expand the number of existing images
      let nul' = lg "TEMP: don't have enough images!"
      return false


switchModules :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> String -> Number -> EpiS eff h Unit
switchModules ssRef rootId childN m1 dim spd = do
  systemST <- lift $ readSTRef ssRef
  mRef  <- loadLib rootId systemST.moduleRefPool "switch module"
  m     <- lift $ readSTRef mRef
  m0    <- loadLib childN m.modules "switch find child"
  m0Ref <- loadLib m0 systemST.moduleRefPool "switch m0"
  m0M   <- lift $ readSTRef m0Ref

  -- create switch module
  switch <- loadLib "switch" systemST.moduleLib "switchModules"

  let modules = fromFoldable [(Tuple "m0" m0), (Tuple "m1" m1)]
  let sub'    = union (fromFoldable [(Tuple "dim" dim), (Tuple "var" m0M.var)]) switch.sub
  let switch' = switch {sub = sub', modules = modules, var = m0M.var}

  swid <- replaceModule ssRef rootId childN m0 (ImportModule switch')

  -- create & import blending script
  createScript ssRef swid "default" "finishSwitch" $ fromFoldable [(Tuple "delay" (show spd))]
  createScript ssRef swid "default" "ppath" $ fromFoldable [(Tuple "par" "intrp"), (Tuple "path" "linear"), (Tuple "spd" (show spd))]

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

      return true
    _ -> do
      return false
