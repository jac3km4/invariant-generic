module Record.Invariant where

import Data.Functor.Invariant (class Invariant, imap)
import Data.Functor.Monoidal (class Monoidal, class Semigroupal, fproduct, funit)
import Data.Tuple (Tuple(..))
import Prelude (const, flip, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), SProxy(SProxy))

class Invariant m <= SequenceRecord rl row rec m | rl -> row rec m where
  sequenceIRImpl :: RLProxy rl -> Record row -> m {| rec }

instance sequenceRecordSingle ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Invariant m
  , Row.Lacks name ()
  , Row.Cons name ty () to
  ) => SequenceRecord (RL.Cons name (m ty) RL.Nil) row to m where
  sequenceIRImpl _ a  = imap (flip (Record.insert namep) {}) (Record.get namep) head
    where
      namep = SProxy :: SProxy name
      head = Record.get namep a

else instance sequenceRecordCons ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Semigroupal m
  , SequenceRecord tail row from' m
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) => SequenceRecord (RL.Cons name (m ty) tail) row to m where
  sequenceIRImpl _ rec  = imap to from (fproduct head rest)
    where
      namep = SProxy :: SProxy name
      head = Record.get namep rec
      tailp = RLProxy :: RLProxy tail
      rest = sequenceIRImpl tailp rec
      to (Tuple head' tail') = Record.insert namep head' tail'
      from rec' = Tuple (Record.get namep rec') (Record.delete namep rec')

instance sequenceRecordNil :: (Invariant m, Monoidal m) => SequenceRecord RL.Nil row () m where
  sequenceIRImpl _ _ = imap (const {}) (const unit) funit

sequenceIR
  :: forall row row' rl m
   . RL.RowToList row rl
  => SequenceRecord rl row row' m
  => Record row
  -> m (Record row')
sequenceIR a = sequenceIRImpl (RLProxy :: RLProxy rl) a
