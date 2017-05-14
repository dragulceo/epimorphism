module Data.Serializable where

import Data.Set
import Control.Monad.Eff (Eff)
import Data.Types (Component(Component), EngineConf(EngineConf), Family(Family), Image(Image), Index, Module(Module), Pattern(Pattern), Schema, SystemConf(SystemConf), UIConf(UIConf), componentSchema, engineConfSchema, familySchema, imageSchema, indexSchema, moduleSchema, patternSchema, systemConfSchema, uiConfSchema)

foreign import unsafeSetDataTableAttr :: forall a b eff. a -> String -> String -> b -> Eff eff a
foreign import unsafeGenericDataTableImpl :: forall a r. Schema -> Schema -> (Index -> (Record r) -> a) -> (Set String) -> a

unsafeGenericDataTable :: forall a r. Schema -> Schema -> (Index -> (Record r) -> a) -> a
unsafeGenericDataTable idxSchema schema construct =
  unsafeGenericDataTableImpl idxSchema schema construct empty

class Serializable a where
  schema :: a -> Schema
  generic :: a

instance scSerializable :: Serializable SystemConf where
  schema a = systemConfSchema
  generic = unsafeGenericDataTable indexSchema systemConfSchema SystemConf

instance ecSerializable :: Serializable EngineConf where
  schema a = engineConfSchema
  generic = unsafeGenericDataTable indexSchema engineConfSchema EngineConf

instance ucSerializable :: Serializable UIConf where
  schema a = uiConfSchema
  generic = unsafeGenericDataTable indexSchema uiConfSchema UIConf

instance pSerializable :: Serializable Pattern where
  schema a = patternSchema
  generic = unsafeGenericDataTable indexSchema patternSchema Pattern

instance mSerializable :: Serializable Module where
  schema a = moduleSchema
  generic = unsafeGenericDataTable indexSchema moduleSchema Module

instance cSerializable :: Serializable Component where
  schema a = componentSchema
  generic = unsafeGenericDataTable indexSchema componentSchema Component

instance fSerializable :: Serializable Family where
  schema a = familySchema
  generic = unsafeGenericDataTable indexSchema familySchema Family

instance iSerializable :: Serializable Image where
  schema a = imageSchema
  generic = unsafeGenericDataTable indexSchema imageSchema Image
