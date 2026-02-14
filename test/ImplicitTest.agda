module ImplicitTest where

-- Test file to expose unsolved metavariables in implicit arguments

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

-- A function with an implicit argument
id : {A : Set} → A → A
id x = x

-- Test case 1: Explicit hole - this WILL be reported
test1 : Nat
test1 = {! !}

-- Test case 2: Unsolved implicit argument - this might NOT be reported
-- The implicit {A} should be inferred but if it can't be, it's a hidden metavariable
test2 : Nat
test2 = id zero

-- Test case 3: Function that doesn't fully constrain its implicit argument
-- This should leave an unsolved metavariable for the implicit argument
data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)

-- This should work fine
test3 : Vec Nat (suc zero)
test3 = zero ∷ []

-- Test case 4: Incomplete definition with implicit that can't be solved
head : {A : Set} {n : Nat} → Vec A (suc n) → A
head (x ∷ xs) = x

-- This might leave unsolved implicits if type inference fails
test4 : Nat
test4 = head (zero ∷ [])

-- Test case 5: Using a function where the implicit can't be fully determined
-- without more context (intentionally ambiguous)
ambiguous : {A : Set} → A
ambiguous = {! !}

-- This will have an unsolved metavariable for the implicit type A
test5 : Nat
test5 = ambiguous
