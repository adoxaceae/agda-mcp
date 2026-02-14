module ImplicitTest2 where

-- Test file with functions that have implicit arguments

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)

-- Function with implicit arguments
replicate : {A : Set} {n : Nat} → A → Vec A n
replicate = {! !}

-- Test: Goal should show implicit arguments when using _implicits version
test1 : Vec Nat (suc (suc zero))
test1 = replicate {! !}

-- Another test with constraints
map : {A B : Set} {n : Nat} → (A → B) → Vec A n → Vec B n
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

test2 : Vec Nat (suc zero) → Vec Nat (suc zero)
test2 = map {! !}
