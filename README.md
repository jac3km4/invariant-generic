# invariant-generic

Experiments with generic invariant combinators.

# Usage
Those combinators could be useful for bidirectional parsers.
Here's an example JSON codec:

```purescript
newtype CodecF f a = CodecF
  { read :: f Foreign -> F a
  , write :: a -> f Foreign
  }

type Codec = CodecF Identity
type ObjectCodec = CodecF Object

instance invariantCodec :: Invariant (CodecF f) where
  imap fn fn' (CodecF fa) = CodecF
    { read: map fn <<< fa.read
    , write: fa.write <<< fn'
    }

instance semigroupalCodec :: Semigroupal (CodecF Object) where
  fproduct (CodecF fa) (CodecF fb) = CodecF
    { read: \inp -> Tuple <$> fa.read inp <*> fb.read inp
    , write: \(Tuple a b) -> Object.union (fa.write a) (fb.write b)
    }

instance monoidalCodec :: Monoidal (CodecF Object) where
  funit = CodecF
    { read: const $ pure unit
    , write: const $ Object.empty
    }

codec :: ∀ a. (Foreign -> F a) -> (a -> Foreign) -> Codec a
codec read write = CodecF { read: read <<< unwrap, write: Identity <$> write }

int :: Codec Int
int = codec readInt unsafeToForeign

string :: Codec String
string = codec readString unsafeToForeign 

field :: ∀ a. String -> Codec a -> ObjectCodec a
field name (CodecF fa) = CodecF
  { read: \inp ->
      case Object.lookup name inp of
        Nothing -> fail $ ForeignError $ "Missing field " <> name
        Just value -> fa.read $ Identity value
  , write: \val -> Object.fromFoldable [ Tuple name (unwrap $ fa.write val) ]
  }

-- usage with a Person record

newtype Person = Person { name :: String, age :: Int }
derive instance newtypePerson :: Newtype Person _

person :: ObjectCodec Person
person = imap wrap unwrap $ sequenceIR
  { name: field "name" string
  , age: field "age" int
  }
```
