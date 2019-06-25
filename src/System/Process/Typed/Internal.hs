{-# LANGUAGE CPP #-}
module System.Process.Typed.Internal (
  nullDevice
) where

-- | The name of the system null device
nullDevice :: FilePath
#if WINDOWS
nullDevice = "\\\\.\\NUL"
#else
nullDevice = "/dev/null"
#endif
