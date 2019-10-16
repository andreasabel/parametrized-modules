module Example1 where

  data Empty : Set where

  data Unit : Set where
    unit : Unit

  data Bool : Set where
    true false : Bool

  If : Bool → (A B : Set) → Set
  If true  A B = A
  If false A B = B

  True : Bool → Set
  True true  = Unit
  True false = Empty

  data Nat : Set where
    zero : Nat
    suc : (n : Nat) → Nat

  module NAT where

    eq : (n m : Nat) → Bool
    eq zero    zero    = true
    eq (suc n) zero    = false
    eq zero    (suc m) = false
    eq (suc n) (suc m) = eq n m

    Eq : (n m : Nat) → Set
    Eq n m = True (eq n m)

  plus : Nat → Nat → Nat
  plus zero    m = m
  plus (suc n) m = suc (plus n m)

  plusZero : (n : Nat) → NAT.Eq (plus n zero) n
  plusZero zero    = unit
  plusZero (suc n) = plusZero n

  max : Nat → Nat → Nat
  max zero    m       = m
  max (suc n) zero    = suc n
  max (suc n) (suc m) = suc (max n m)

  module TREE (A : Set) where

    data Tree : Set where
      leaf : A → Tree
      node : Tree → Tree → Tree

    height : Tree → Nat
    height (leaf a)   = zero
    height (node t u) = max (height t) (height u)

  module TREE' (b : Bool) = TREE (If b Nat Bool)

  wrong : (t : TREE'.Tree true) → NAT.Eq (TREE'.height true (TREE'.node t t)) zero
  wrong t = unit

  -- Agda says:
  -- Unit !=<
  -- True
  -- (NAT.eq
  --  (max (TREE.height (If true Nat Bool) t)
  --   (TREE.height (If true Nat Bool) t))
  --  zero)
  -- when checking that the expression unit has type
  -- NAT.Eq (TREE'.height true (TREE'.node t t)) zero

  -- Expected error:
  -- Unit !=< True (NAT.eq (max (TREE'.height true t) (TREE'.height true t)) zero)

{-
  module TN = TREE' true
  wrong : (t : TN.Tree) → NAT.Eq (TN.height (TN.node t t)) zero
  wrong t = unit
  -- Expected error:
  -- Unit !=< True (NAT.eq (max (TN.height t) (TN.height t)) zero)
  -- when checking that the expression unit has type
  -- NAT.Eq (TN.height (TN.node t t)) zero

-- -}
-- -}
