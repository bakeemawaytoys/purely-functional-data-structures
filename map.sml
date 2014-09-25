signature ORDERED = 
sig
  type T
  
  val eq        : T * T -> bool
  val lt        : T * T -> bool
  val leq       : T * T -> bool
end

exception NotFound

signature FINITE_MAP = 
sig
  type Key
  type 'a Map

  val empty     : 'a Map
  val bind      : Key * 'a * 'a Map -> 'a Map
  val lookup    : Key * 'a Map -> 'a
end


functor UnbalancedFiniteMap(Elem : ORDERED): FINITE_MAP = 
struct
  type Key = Elem.T
  datatype 'a Tree = E | T of 'a Tree * Key * 'a * 'a Tree
  type 'a Map = 'a Tree

  val empty = E
  fun bind(key, value, E) = T(E, key, value, E)
    | bind (key, value, map) = E
  fun lookup (_, E) = raise NotFound
    | lookup (key, map) = raise NotFound
end

structure IntOrdered : ORDERED =
struct
  type T = int
  fun eq (l, r) = l = r
  fun lt (l, r) = l < r
  fun leq (l, r) = l <= r
end
