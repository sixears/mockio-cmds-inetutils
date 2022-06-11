{-# LANGUAGE QuasiQuotes #-}

{-| Filesystem paths to external programs; to be filled in at build time. -}
module MockIO.Cmds.InetUtils.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

{-| Path to @hostname@ -}
hostname :: AbsFile
hostname = [absfile|/nix/store/sqiphymcpky1yysgdc1aj4lr9jg9n53a-inetutils-2.2/bin/hostname|]
