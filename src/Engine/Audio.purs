module Audio where

import Prelude
import Graphics.WebGL.Raw.Types as GLT
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(Just, Nothing))
import Data.System (AudioAnalyser)
import Data.Tuple (Tuple(Tuple))
import Data.Types (EngineConfD)
import Graphics.WebGL.Raw.Types (ArrayBufferView)
import Graphics.WebGL.Types (WebGLTexture, WebGL, WebGLContext)
import Texture (newTex)

foreign import audioData :: forall eff. AudioAnalyser -> Eff eff (ArrayBufferView)
foreign import initAudioAnalyzer :: forall eff. Int -> Eff eff AudioAnalyser

-- initialize audio texture
initAudio :: EngineConfD -> WebGLContext -> GLT.TexImageSource -> WebGL (Maybe (Tuple WebGLTexture AudioAnalyser))
initAudio engineConfD ctx empty = do
  case engineConfD.audioAnalysisEnabled of
    false -> pure Nothing
    true -> do
      audioTex <- newTex
      analyser <- lift $ lift $ initAudioAnalyzer engineConfD.audioBufferSize
      pure $ Just (Tuple audioTex analyser)
