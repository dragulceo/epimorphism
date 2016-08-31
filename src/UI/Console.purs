module Console where

import Prelude
import Config (Script, scriptSchema, EpiS, moduleSchema, Module, Pattern, SystemST, UIST, UIConf)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array ((:), filter, partition)
import Data.DOM.Simple.Element (setInnerHTML)
import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (foldM, StrMap)
import Data.String (joinWith, trim, split, replace)
import Data.String.Regex (match, noFlags, regex)
import Data.Traversable (traverse)
import Serialize (unsafeSerialize)
import System (family, loadLib)
import UIUtil (findElt)
import Util (inj, lg, indentLines)

foreign import addEventListeners :: forall eff. Eff eff Unit

renderConsole :: forall eff h. UIConf -> UIST -> SystemST h -> Pattern -> EpiS eff h Unit
renderConsole uiConf uiST systemST pattern = do
  dsmDiv <- findElt "debugMain"
  str0 <- renderModule systemST.moduleLib systemST.moduleRefPool pattern.main "MAIN" Nothing
  lift $ setInnerHTML str0 dsmDiv

  dsdDiv <- findElt "debugDisp"
  str1 <- renderModule systemST.moduleLib systemST.moduleRefPool pattern.disp "DISP" Nothing
  lift $ setInnerHTML str1 dsdDiv

  dssDiv <- findElt "debugScripts"
  str2 <- serializeScripts systemST.scriptRefPool
  lift $ setInnerHTML ("<span style='color:pink'>SCRIPTS:</span>\n" ++ str2) dssDiv

  lift $ addEventListeners


-- serialize scripts for debug view
serializeScripts :: forall eff h. StrMap (STRef h Script) -> EpiS eff h String
serializeScripts pool = do
  foldM handle "" pool
  where
    handle res k sRef = do
      scr <- lift $ readSTRef sRef
      let title = ("<span style='color:blue;font-weight:bold;'>" ++ k ++ "</span>")
      str <- unsafeSerialize scriptSchema (Just title) scr
      return $ str ++ "<br/><br/>" ++ res


-- serializes the modRefPool into an html string for debugging
renderModule :: forall eff h. StrMap Module -> StrMap (STRef h Module) -> String -> String -> Maybe String -> EpiS eff h String
renderModule lib pool mid title pid = do
  mRef <- loadLib mid pool "renderModule"
  mod <- lift $ readSTRef mRef
  str <- unsafeSerialize moduleSchema Nothing mod

  -- find & parse module line
  let lines = split "\n" str
  {yes: modules, no: rst} <- return $ partition isModules lines
  modRes <- case modules of
    [line] -> parseModLine mid mod line
    _ -> return ""

  -- parse other lines
  let rst' = filter (\x -> x /= "") rst
  let idLine = "id " ++ mid
  lines' <- traverse handleLine (idLine : rst')
  let res = joinWith "" $ lines'

  -- title
  let titlePre = "<span class='consoleModTitle'>" ++ title ++ ": </span>"
  sel <- case pid of
    Just pid' -> do
      sel' <- renderSelect lib mod pid' title
      return $ sel' ++ "<span class='consoleModTitle consoleUI'>" ++ mod.libName ++ "</span>"
    _ -> return $ "<span class='consoleModTitle'>"  ++ mod.libName ++ "</span>"

  let title' = titlePre ++ sel

  let res' = title' ++ indentLines 2 (res ++ modRes)

  return res'
  where
    isModules :: String -> Boolean
    isModules line =
      let rgx = regex "^modules" noFlags in
      case (match rgx line) of
        (Just _) -> true
        _ -> false
    parseModLine :: String -> Module -> String -> EpiS eff h String
    parseModLine mid mod line = do
      let rgx = regex "modules \\{([^\\{\\}]*)\\}" noFlags
      case (match rgx line) of
        (Just [(Just _), (Just "")]) -> do
          return line
        (Just [(Just m0), (Just m1)]) -> do
          dt <- traverse (exp <<< split ":") $ split "," m1
          let modS = joinWith "\n" dt
          return $ (trim (replace m0 "" $ replace (m0 ++ "\n") "" line)) ++ "\n" ++ modS
        _ ->
          throwError "not a module line, dingus"
    exp [a, b] = do
      renderModule lib pool (trim b) (trim a) (Just mid)
    exp _ = throwError $ "invalid map syntax in " ++ mid
    handleLine line = do
      let rgx = regex "^(component|par|zn|images|sub|scripts|--)\\s?(.*)$" noFlags
      res <- case (match rgx line) of
        (Just [(Just _), (Just m0), (Just m1)]) -> do
          case m0 of
            _ -> return $ "\n" ++ line
        _ ->
          return $ "<span class='extraData'>\n" ++ line ++ "</span>"

      return res

renderSelect :: forall eff h. StrMap Module -> Module -> String -> String -> EpiS eff h String
renderSelect lib mod pid cname = do
  let fam = family lib mod.family [] []
  let fam' = map (\x -> inj "<option value='%0'>%0</option>" [x]) fam
  let options = joinWith "\n" fam'
  let options' = (inj "<option selected disabled>%0</option>" [mod.libName]) ++ options
  let r0 = inj "<select class='consoleUI switchChild' style='display:none;' data-mid='%0' data-cname='%1'>%2</select>" [pid, cname, options']
  return r0
