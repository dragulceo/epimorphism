module Console where

import Prelude
import Config (Script, scriptSchema, EpiS, moduleSchema, Module, Pattern, SystemST, UIST, UIConf)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array ((!!), length, (..), (:), filter, partition)
import Data.DOM.Simple.Element (setInnerHTML)
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (lookup, foldM, StrMap)
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
  str0 <- renderModule systemST pattern.main "MAIN" Nothing
  lift $ setInnerHTML str0 dsmDiv

  dsdDiv <- findElt "debugDisp"
  str1 <- renderModule systemST pattern.disp "DISP" Nothing
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


-- serializes the modRefPool into an html string for debugging.  shitcode
renderModule :: forall eff h. SystemST h -> String -> String -> Maybe String -> EpiS eff h String
renderModule systemST mid title pid = do
  let lib = systemST.moduleLib
  let pool = systemST.moduleRefPool

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
  lines' <- traverse (handleLine mod) (idLine : rst')
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
      renderModule systemST (trim b) (trim a) (Just mid)
    exp _ = throwError $ "invalid map syntax in " ++ mid
    handleLine mod line = do
      let rgx = regex "^(component|par|zn|images|sub|scripts|paths|--)\\s?(.*)$" noFlags
      res <- case (match rgx line) of
        (Just [(Just _), (Just m0), (Just m1)]) -> do
          case m0 of
            "component" -> do
              comp <- loadLib m1 systemST.componentLib "load component console"
              let ui = inj "<span class='consoleUI' style='display:none'>component </span><span class='componentUI consoleUI' style='display:none;' data-mid='%1'>%0</span><div id='%1' class='hidden' style='display:none'><div>%2</div></div>" [m1, m1, comp.body]
              return $ "\n<span class='consoleUI'>" ++ line ++ "</span>" ++ ui
            "sub" -> do
              let rgx' = regex "^\\{(.*)\\}$" noFlags
              uiCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split "," cts
                  cmp' <- flip traverse cmp \x -> do
                    let dt = map trim $ split ":" x
                    case dt of
                      [var, val] -> do
                        let inp = inj "<input type='text' class='consoleSub' data-mid='%0' data-subN='%1' value='%2'>" [mid, var, val]
                        return $ var ++ ": " ++ inp
                      _ -> throwError "invalide sub fmt :"
                  return $ joinWith ", " cmp'
                _ -> throwError "invalid sub fmt {"
              let ui = inj "<span class='consoleUI' style='display:none;'>sub {%0}</span>" [uiCts]
              return $ "\n<span class='consoleUI'>" ++ line ++ "</span>" ++ ui
            "images" -> do
              let rgx' = regex "^\\[(.*)\\]$" noFlags
              uiCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split "," cts
                  let idxs = (0 .. (length cmp - 1))
                  let cmp' = flip map idxs \x ->
                    inj "<span class='consoleImage' data-mid='%0' data-idx='%1'>%2</span>" [mid, (show x), fromJust $ cmp !! x]
                  return $ joinWith ", " cmp'
                _ -> throwError "invalid images fmt ["
              let ui = inj "<span class='consoleUI' style='display:none;'>images [%0]</span>" [uiCts]
              return $ "\n<span class='consoleUI'>" ++ line ++ "</span>" ++ ui
            "par" -> do
              let rgx' = regex "^\\{(.*)\\}$" noFlags
              uiCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split "," cts
                  cmp' <- flip traverse cmp \x -> do
                    let dt = map trim $ split ":" x
                    case dt of
                      [var, val] -> do
                        let path = fromMaybe "" (lookup var mod.paths)
                        let inp = inj "<span class='consolePar consoleVar' data-mid='%0' data-var='%1' data-path='%2' data-val='%3' data-type='par'>%3</span>" [mid, var, path, val]
                        return $ var ++ ": " ++ inp
                      _ -> throwError "invalide par fmt :"
                  return $ joinWith ", " cmp'
                _ -> throwError "invalid par fmt {"
              let ui = inj "<span class='consoleUI' style='display:none;'>par {%0}</span>" [uiCts]
              return $ "\n<span class='consoleUI'>" ++ line ++ "</span>" ++ ui
            "zn" -> do
              let rgx' = regex "^\\[(.*)\\]$" noFlags
              uiCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split "," cts
                  let idxs = (0 .. (length cmp - 1))
                  let cmp' = flip map idxs \x ->
                    let path = fromMaybe "" (lookup (show x) mod.paths) in
                    inj "<span class='consoleZn consoleVar' data-mid='%0' data-var='%1' data-path='%2' data-val='%3' data-type='zn'>%3</span>" [mid, (show x), path, fromJust $ cmp !! x]
                  return $ joinWith ", " cmp'
                _ -> throwError "invalid zn fmt ["

              let ui = inj "<span class='consoleUI' style='display:none;'>zn [%0]</span>" [uiCts]
              return $ "\n<span class='consoleUI'>" ++ line ++ "</span>" ++ ui
            "scripts" -> do
              let rgx' = regex "^\\[(.*)\\]$" noFlags
              case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split "," cts
                  cmp' <- flip traverse cmp \sid -> do
                    sRef <- loadLib sid systemST.scriptRefPool "console script"
                    scr <- lift $ readSTRef sRef
                    return scr.fn
                  let res' = joinWith ", " cmp'
                  let ui = inj "\n<span>scripts [%0]</span>" [res']
                  return $ (inj "<span class='extraData'>\nscriptIds [%0] </span>" [cts]) ++ ui

                _ -> throwError "invalid scripts ["
            _ -> return $ "\n" ++ line
        _ ->
          return $ "<span class='extraData'>\n" ++ line ++ "</span>"

      return res

renderSelect :: forall eff h. StrMap Module -> Module -> String -> String -> EpiS eff h String
renderSelect lib mod pid cname = do
  let fam = family lib mod.family ["lib"] []
  let fam' = map (\x -> inj "<option value='%0'>%0</option>" [x]) fam
  let options = joinWith "\n" fam'
  let options' = (inj "<option selected disabled>%0</option>" [mod.libName]) ++ options
  let r0 = inj "<select class='consoleUI switchChild' style='display:none;' data-mid='%0' data-cname='%1'>%2</select>" [pid, cname, options']
  return r0
