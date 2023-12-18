{-| Interface to the @hostname@ cmdline utility. -}
module MockIO.Cmds.InetUtils.Hostname
  ( hostname )
where

import Base1T

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), parseText )

-- domainnames -------------------------

import DomainNames.Hostname  ( Hostname )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO.DoMock  ( HasDoMock( doMock ) )

-- mockio-log --------------------------

import MockIO.IOClass      ( IOClass( IOCmdR ), ioClass )
import MockIO.Log          ( logResult )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process            ( ꙩ )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks )

-- safe --------------------------------

import Safe  ( succSafe )

-- stdmain -----------------------------

import StdMain.ProcOutputParseError ( AsProcOutputParseError
                                    , throwAsProcOutputParseError )

-- text --------------------------------

import Data.Text  ( dropWhileEnd )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified MockIO.Cmds.InetUtils.Paths  as  Paths

--------------------------------------------------------------------------------

chomp ∷ 𝕋 → 𝕋
chomp = dropWhileEnd (\ c → c ∈ "\r\n")

hostname ∷ ∀ ε ρ μ .
           (MonadIO μ, HasDoMock ρ, MonadReader ρ μ,
            MonadLog (Log MockIOClass) μ,
            AsProcOutputParseError ε, AsProcExitError ε, AsCreateProcError ε,
            AsFPathError ε, AsIOError ε, Printable ε,
            MonadError ε μ, HasCallStack) ⇒
           Severity → μ Hostname
hostname sev = do
  do_mock ← asks (view doMock)
  let vmsg ∷ 𝕄 (Hostname → [𝕋])
      vmsg = 𝕵 $ pure ∘ [fmt|%T|]
      log_attr = def & ioClass ⊢ IOCmdR & doMock ⊢ do_mock
  t ← snd ⊳ ꙩ (Paths.hostname,["--fqdn"∷𝕋],succSafe sev,"myhost.foo.bar"∷𝕋)
  h ← case parseText (chomp t ⊕ ".") of
    Parsed    h   → return h
    Malformed _ e → throwAsProcOutputParseError e
  logResult sev log_attr do_mock "hstnm" vmsg (𝕽 h)

-- that's all, folks! ----------------------------------------------------------
