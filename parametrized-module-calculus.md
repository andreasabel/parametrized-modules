# A core calculus for parametrized modules

# Syntax

- Module names    M,N
- Names           f,g,h,D,c
- Qualified names q ::= Ms.f
- Variables       x,y,z
- Patterns        p ::= c xs
- Universes       Typeᵢ
- Terms           t,u,v,A,B
- Telescopes      Γ,Δ

- Declarations d

    * Parameterized module

        module M Γ where ds

    * Simple datatypes (no parameter nor indices)

        data D where cs Γs

    * Aliases

        f : A = t

    * Pattern matching definitions

        f : (x : A) → B
          = ps → ts

    * Instantiation

        module N Γ = M ts

      This statement can be interpreted in two different ways:

      1. new, deeply copying definitions (even pattern matching defs)
      2. old, like Agda does now, just creating aliases:
         for f in M create N.f Γ = M.f ts (and recursively for nested modules).

# Semantics

Have the usual reduction rules (small-step semantics).
For 1. add equalities

   N.f Γ = M.f ts

that only fire during conversion checking, not weak head evaluation.
For 2. this is the only reduction rule needed to facilitate
instantiation.

Goal: show that the two interpretations yield the same semantics under
a suitable reduction relation and equality relation.
