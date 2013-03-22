--data NonEmptyList a = a :| Rest a
--    deriving (Eq, Show)
--data Rest a = NonEmptyList a | ListEnd
--    deriving (Eq, Show)

--data NonEmptyList a = a :| Maybe (NonEmptyList a)

--data NonEmptyList a = a :| NonEmptyList a | NEEnd a
--
--
-- infixr 5 :|
data NonEmptyList a = NEL a [a]
    deriving (Eq, Show)

