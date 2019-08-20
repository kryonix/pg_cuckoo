{-|
Module      : OperSem
Description : Operational Semantics Monad
Copyright   : (c) Torsten Grust, 2014
                  Denis Hirn, 2019
License     : AllRightsReserved
Maintainer  : Denis Hirn <denis.hirn@uni-tuebingen.de>
Stability   : stable

Generic support to implement operational semantics (inference rules).
-}

module Database.PgCuckoo.OperSem
    ( OperSem
    , runOperSem
    , runOperSemST
    , module Monads
    ) where

import Control.Monad.Except as Monads
import Control.Monad.Reader as Monads
import Control.Monad.Writer as Monads
import Control.Monad.State  as Monads

-- |A monad for operational semantics (provides log to trace rule application)
--
-- * s: state (e.g., Dewey code supply)
-- * e: environment (e.g., let/for variable bindings)
-- * a: inferred semantic object
-- * l: logged information
type OperSem s e a l = ExceptT String (WriterT l (StateT s (Reader e))) a

-- |Run operational semantics
-- (= apply inference rule r in state s under environment e)
runOperSem :: (Monoid l) => OperSem s e a l -> s -> e -> (Either String a, l)
runOperSem r s e = runReader (evalStateT (runWriterT (runExceptT r)) s) e


-- |Run operational semantics and return final state
-- (= apply inference rule r in state s under environment e)
runOperSemST :: (Monoid l) => OperSem s e a l -> s -> e -> ((Either String a, l), s)
runOperSemST r s e = runReader (runStateT (runWriterT (runExceptT r)) s) e
