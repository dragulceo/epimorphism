module Console where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array ((:), filter, partition)
import Data.DOM.Simple.Element (setInnerHTML)
import Data.Library (buildSearch, family, getLib, getPatternD, getUIConfD, idM, idx, mD, searchLib)
import Data.Library (idx) as L
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Replacement(..), joinWith, trim, split, replace)
import Data.String (Pattern(..)) as S
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Types (Component(..), EngineST, EpiS, Kernel(..), Library, Module(..), ModuleD, SystemST, UIST, kGet, moduleSchema)
import Paths (isConstantPath, runPath)
import Serialize (showCX, unsafeSerialize)
import Text.Format (format, precision)
import UIUtil (findElt)
import Util (indentLines, inj, log, real, zipI)

foreign import addEventListeners :: forall eff. Eff eff Unit

renderConsole :: forall eff h. UIST -> SystemST h -> EngineST -> Library h -> EpiS eff h Unit
renderConsole uiST systemST engineST lib = do
  uiConfD  <- getUIConfD lib "renderConsole uiConf"
  patternD <- getPatternD lib "renderConsole pattern"
  dsmDiv <- findElt "debugMain"
  str0 <- renderModule systemST lib uiConfD.uiCompLib patternD.main "MAIN" Nothing
  lift $ setInnerHTML str0 dsmDiv

  dsmSDiv <- findElt "debugMainSrc"
  case (kGet engineST.curST.src Main) of
    Just src -> lift $ setInnerHTML src dsmSDiv
    Nothing -> pure unit

  dssDiv <- findElt "debugSeed"
  str0 <- renderModule systemST lib uiConfD.uiCompLib patternD.seed "SEED" Nothing
  lift $ setInnerHTML str0 dssDiv

  dssSDiv <- findElt "debugSeedSrc"
  case (kGet engineST.curST.src Seed) of
    Just src -> lift $ setInnerHTML src dssSDiv
    Nothing -> pure unit

  dsdDiv <- findElt "debugDisp"
  str1 <- renderModule systemST lib uiConfD.uiCompLib patternD.disp "DISP" Nothing
  lift $ setInnerHTML str1 dsdDiv

  dsdSDiv <- findElt "debugDispSrc"
  case (kGet engineST.curST.src Disp) of
    Just src -> lift $ setInnerHTML src dsdSDiv
    Nothing -> pure unit

  lift $ addEventListeners

-- serializes the modRefPool into an html string for debugging.  shitcode
renderModule :: forall eff h. SystemST h -> Library h -> String -> String -> String -> Maybe String -> EpiS eff h String
renderModule systemST lib modLib mid title pid = do
  mod@(Module _ modD) <- getLib lib mid "renderModule module"
  str <- unsafeSerialize moduleSchema Nothing modD

  -- find & parse module line
  let lines = split (S.Pattern "\n") str
  {yes: modules, no: rst} <- pure $ partition isModules lines
  modRes <- case modules of
    [line] -> parseModLine mid modD line
    _ -> pure ""

  -- parse other lines
  let rst' = filter (\x -> x /= "") rst
  let idLine = "id " <> mid
  lines' <- traverse (handleLine modD) (idLine : rst')
  let res = joinWith "" $ lines'

  -- title
  let titlePre = "<span class='consoleModTitle prefix'>" <> title <> ": </span>"
  sel <- case pid of
    Just pid' -> do
      sel' <- renderSelect modLib lib mod pid' title
      pure $ sel' <> "<span class='consoleModTitle consoleUI'>" <> (idx mod).orig <> "</span>"
    _ -> pure $ "<span class='consoleModTitle'>"  <> (idx mod).orig <> "</span>"

  let title' = titlePre <> sel

  let res' = title' <> indentLines 2 (res <> modRes)

  pure res'
  where
    isModules :: String -> Boolean
    isModules line =
      let rgx = unsafeRegex "^modules" noFlags in
      case (match rgx line) of
        (Just _) -> true
        _ -> false
    parseModLine :: String -> ModuleD -> String -> EpiS eff h String
    parseModLine _ modD line = do
      let rgx = unsafeRegex "modules \\{([^\\{\\}]*)\\}" noFlags
      case (match rgx line) of
        (Just [(Just _), (Just "")]) -> do
          pure line
        (Just [(Just m0), (Just m1)]) -> do
          dt <- traverse (exp <<< split (S.Pattern ":")) $ split (S.Pattern ",") m1
          let modS = joinWith "\n" dt
          pure $ (trim (replace (S.Pattern m0) (Replacement "") $ replace (S.Pattern (m0 <> "\n")) (Replacement "") line)) <> "\n" <> modS
        _ ->
          throwError "not a module line, dingus"
    exp [a, b] = do
      renderModule systemST lib modLib (trim b) (trim a) (Just mid)
    exp _ = throwError $ "invalid map syntax in " <> mid
    handleLine modD line = do
      let rgx = unsafeRegex "^(component|par|zn|images|sub|scripts|--)\\s?(.*)$" noFlags
      res <- case (match rgx line) of
        (Just [(Just _), (Just m0), (Just m1)]) -> do
          case m0 of
            "component" -> do
              (Component _ comp) <- getLib lib m1 "load component console"
              let ui = inj "<span class='consoleUI' style='display:none'>component </span><span class='componentUI consoleUI' style='display:none;' data-mid='%1'>%0</span><div id='%1' class='hidden' style='display:none'><div>%2</div></div>" [m1, m1, comp.code]
              pure $ "\n<span class='consoleUI'>" <> line <> "</span>" <> ui
            "sub" -> do
              let rgx' = unsafeRegex "^\\{(.*)\\}$" noFlags
              uiCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split (S.Pattern ",") cts
                  cmp' <- flip traverse cmp \x -> do
                    let dt = map trim $ split (S.Pattern ":") x
                    case dt of
                      [var, val] -> do
                        let inp = inj "<input type='text' class='consoleSub' data-mid='%0' data-subN='%1' value='%2'>" [mid, var, val]
                        pure $ var <> ": " <> inp
                      _ -> throwError "invalid sub fmt :"
                  pure $ joinWith ", " cmp'
                _ -> throwError "invalid sub fmt {"
              let ui = inj "<span class='consoleUI' style='display:none;'>sub {%0}</span>" [uiCts]
              pure $ "\n<span class='consoleUI'>" <> line <> "</span>" <> ui
            "images" -> do
              let rgx' = unsafeRegex "^\\[(.*)\\]$" noFlags
              uiCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split (S.Pattern ",") cts

                  let cmp' = flip map (zipI cmp) \(Tuple i c) ->
                    inj "<span class='consoleImage' data-mid='%0' data-idx='%1'>%2</span>" [mid, (show i), c]
                  pure $ joinWith ", " cmp'
                _ -> throwError "invalid images fmt ["
              let ui = inj "<span class='consoleUI' style='display:none;'>images [%0]</span>" [uiCts]
              pure $ "\n<span class='consoleUI'>" <> line <> "</span>" <> ui
            "par" -> do
              let rgx' = unsafeRegex "^\\{(.*)\\}$" noFlags
              uiCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split (S.Pattern ",") cts
                  cmp' <- flip traverse cmp \x -> do
                    let dt = map trim $ split (S.Pattern ":") x
                    case dt of
                      [var, val] -> do
                        (Tuple res _) <- runPath systemST.t val
                        let inp = inj "<span class='consolePar consoleVar' data-mid='%0' data-var='%1' data-val='%2' data-type='par'>%2</span>" [mid, var, val]
                        pure $ inj "%0: (%1)%2" [var, (format (precision 2) $ real res), inp]
                      _ -> throwError "invalid par fmt :"
                  pure $ joinWith ", " cmp'
                _ -> throwError "invalid par fmt {"
              let ui = inj "<span class='consoleUI' style='display:none;'>par {%0}</span>" [uiCts]
              lineCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split (S.Pattern ",") cts
                  cmp' <- flip traverse cmp \x -> do
                    let dt = map trim $ split (S.Pattern ":") x
                    case dt of
                      [var, val] -> do
                        case isConstantPath val of
                          true -> pure $ var <> ": " <> val
                          false -> do
                            (Tuple res _) <- runPath systemST.t val
                            let val' = (inj "<span class='consoleVal'>%0</span>" [(format (precision 2) $ real res)])
                            pure $ inj "%0: (%1)%2" [var, val', val]
                      _ -> throwError "invalid par fmt :"
                  pure $ joinWith ", " cmp'
                _ -> throwError "invalid par fmt {"
              let line' = inj "par {%0}" [lineCts]
              pure $ "\n<span class='consoleUI'>" <> line' <> "</span>" <> ui
            "zn" -> do
              let rgx' = unsafeRegex "^\\[(.*)\\]$" noFlags
              uiCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split (S.Pattern ",") cts
                  cmp' <- flip traverse (zipI cmp) \(Tuple i c) -> do
                    (Tuple res _) <- runPath systemST.t c
                    pure $ inj "<span class='consoleZn consoleVar' data-mid='%0' data-var='%1' data-val='%2' data-type='zn'>(%2)%3</span>" [mid, (show i), (showCX res), c]
                  pure $ joinWith ", " cmp'
                _ -> throwError "invalid zn fmt ["
              let ui = inj "<span class='consoleUI' style='display:none;'>zn [%0]</span>" [uiCts]
              lineCts <- case (match rgx' m1) of
                (Just [(Just _), (Just cts)]) -> do
                  let cmp = map trim $ split (S.Pattern ",") cts
                  cmp' <- flip traverse (zipI cmp) \(Tuple i c) -> do
                    case isConstantPath c of
                      true -> pure c
                      false -> do
                        (Tuple res _) <- runPath systemST.t c
                        let val' = inj "<span class='consoleVal'>%0</span>" [showCX res]
                        pure $ inj "(%0)%1" [val', c]
                  pure $ joinWith ", " cmp'
                _ -> throwError "invalid zn fmt ["
              let line' = inj "zn [%0]" [lineCts]
              pure $ "\n<span class='consoleUI'>" <> line' <> "</span>" <> ui
            "scripts" -> do
              pure $ "\nscripts " <> m1
              --let rgx' = unsafeRegex "^\\[(.*)\\]$" noFlags
              --case (match rgx' m1) of
              --  (Just [(Just _), (Just cts)]) -> do
              --    let cmp = map trim $ split (S.Pattern ",") cts
              --    cmp' <- flip traverse cmp \sid -> do
              --      sRef <- loadLib sid systemST.scriptRefPool "console script"
              --      scr <- lift $ readSTRef sRef
              --      pure scr.fn
              --    let res' = joinWith ", " cmp'
              --    let ui = inj "\n<span>scripts [%0]</span>" [res']
              --    pure $ (inj "<span class='extraData'>\nscriptIds [%0] </span>" [cts]) <> ui
              --
              --  _ -> throwError "invalid scripts ["
            _ -> pure $ "\n" <> line
        _ ->
          pure $ "<span class='extraData'>\n" <> line <> "</span>"

      pure res

renderSelect :: forall eff h. String -> Library h -> Module -> String -> String -> EpiS eff h String
renderSelect modLib lib mod@(Module _ modD) pid cname = do
  fm <- family lib mod
  let search = buildSearch [modLib] ["live"] [Tuple "family" (idx fm).id]
  res <- searchLib lib search
  let fam = map (\x -> (L.idx x).id) (res :: Array Module)
  let fam' = map (\x -> inj "<option value='%0'>%0</option>" [x]) fam
  let options = joinWith "\n" fam'
  let options' = (inj "<option selected disabled>%0</option>" [(idx mod).orig]) <> options
  let r0 = inj "<select class='consoleUI switchChild' style='display:none;' data-mid='%0' data-cname='%1'>%2</select>" [pid, cname, options']
  pure r0
