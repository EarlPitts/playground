import cats._

// The K variant of these algebras differ from the normal ones
// by having higher-kinded types as type parameters

// A simple semigroup
Semigroup[Int].combine(1, 2)

// SemigroupK
SemigroupK[List].combineK(List(1), List(2))

// MonoidK
MonoidK[List].combineK(List(1), MonoidK[List].empty)
