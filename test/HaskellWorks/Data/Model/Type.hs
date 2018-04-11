module HaskellWorks.Data.Model.Type where

data Storage
  = StorageDisk
    { storageDevice :: Maybe String
    , storageLabel  :: Maybe String
    }
  | StorageTmpFs
    { storageSizeMB :: Int
    }
  | StorageNfs
    { storageServer     :: String
    , storageRemotePath :: String
    } deriving (Eq, Show)

data Mount = Mount
  { mountPoint    :: String
  , mountStorage  :: Storage
  , mountFsType   :: String
  , mountReadOnly :: Bool
  , mountOptions  :: [String]
  } deriving (Eq, Show)
