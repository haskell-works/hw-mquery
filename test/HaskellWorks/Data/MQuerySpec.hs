module HaskellWorks.Data.MQuerySpec (spec) where

import HaskellWorks.Data.Model.Type
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fstab :: [Mount]
fstab =
  [ Mount
    { mountPoint    = "/"
    , mountStorage  = StorageDisk
      { storageDevice = Just "/dev/sda1"
      , storageLabel  = Nothing
      }
    , mountFsType   = "btrfs"
    , mountReadOnly = True
    , mountOptions  = []
    }
  , Mount
    { mountPoint = "/var"
    , mountStorage = StorageDisk
      { storageLabel = Just "8f3ba6f4-5c70-46ec-83af-0d5434953e5f"
      }
    , mountFsType  = "ext4"
    , mountOptions = ["nosuid"]
    }
  , Mount
    { mountStorage = StorageTmpFs
      { storageSizeMB = 64
      }
    }
  , Mount
    { mountStorage = StorageNfs
      { storageServer     = "my.nfs.server"
      , storageRemotePath = "/exports/mypath"
      }
    }
  ]

-- {
--     "/var/www": {
--         "storage": {
--             "type": "nfs",
--             "server": "my.nfs.server",
--             "remotePath": "/exports/mypath"
--         }
--     }
-- }

spec :: Spec
spec = describe "HaskellWorks.Data.MQuerySpec" $ do
  it "Stub" $ requireProperty $ do
    1 === (1 :: Int)
