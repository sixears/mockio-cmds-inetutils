{-# LANGUAGE QuasiQuotes #-}

{-| Filesystem paths to external programs; to be filled in at build time. -}
module MockIO.Cmds.InetUtils.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

{-| Path to @hostname@ -}
hostname :: AbsFile
hostname = [absfile|__inetutils__/bin/hostname|]
