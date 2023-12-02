module Common.State where

import Control.Monad.State
import Env

type EnvStateT = StateT Env IO
