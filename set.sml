signature SET =
sig
  type Elem
  type Set

  val empty     : Set
  val insert    : Elem * Set -> Set
  val insert2   : Elem * Set -> Set
  val insert3   : Elem * Set -> Set
  val member    : Elem * Set -> bool
  val member2   : Elem * Set -> bool
end  


signature ORDERED =
sig
  type T

  val eq        : T * T -> bool
  val lt        : T * T -> bool
  val leq       : T * T -> bool
end

functor UnbalancedSet(Element : ORDERED) : SET = 
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T(a, y, b)) = 
        if Element.lt(x,y) then member(x,a)
        else if Element.lt(y,x) then member(x,b)
        else true

  fun insert (x, E) = T(E, x, E)
    | insert (x, s as T(a, y, b)) =
        if Element.lt(x, y) then T(insert(x, a), y, b)
        else if Element.lt(y, x) then T(a, y, insert(x, b))
        else s

  (* Exercise 2.3 *)
  exception ExistingElement
  fun try_insert(x, E) = T(E, x, E)
    | try_insert(x, s as T(a, y, b)) = 
        if Element.lt(x, y) then 
          let
            val node = insert(x, a)
          in
            T(node, y, b)
          end
        else if Element.lt(y, x) then
          let
            val node = insert(x, b)
          in
            T(a, y, node)
          end
        else raise ExistingElement
  fun insert2 (x, s) = try_insert(x, s) handle ExistingElement => s
  (* End Exercise 2.3  *)

  (* Exercise 2.4  *)
  fun check_candidate_for_insert(x, E) = T(E, x, E)
    | check_candidate_for_insert(x, T(_, c, _)) = 
        if Element.eq(x,c) then raise ExistingElement else T(E, x, E)

  fun check_and_try_insert(x, E, candidate) = check_candidate_for_insert(x,candidate)
    | check_and_try_insert(x, current as T(E, y, E), candidate) = 
        if Element.lt(x, y) then 
          let
            val node = check_candidate_for_insert(x, candidate)
          in
            T(node, y, E)
          end
        else 
          let
            val node = check_candidate_for_insert(x, current) 
          in
            T(E, y, node)
          end
    | check_and_try_insert(x, current as T(E, y, b), candidate) = 
        if Element.lt(x, y) then 
          let
            val node = check_candidate_for_insert(x,candidate)
          in
            T(node, y, b)
          end
        else 
          let
            val node = check_and_try_insert(x,b,current)
          in
            T(E, y, node)
          end
    | check_and_try_insert(x, current as T(a, y, E), candidate) = 
        if Element.lt(x, y) then 
          let
            val node = check_and_try_insert(x, a, candidate)
          in
            T(node, y, E)
          end
        else 
          let
            val node = check_candidate_for_insert(x, current)
          in
            T(a, y, node)
          end
    | check_and_try_insert (x, current as T(a, y, b), candidate) = 
        if Element.lt(x, y) then 
          let
            val node = check_and_try_insert(x,a,candidate) 
          in
            T(node, y, b)
          end
        else 
          let
            val node = check_and_try_insert(x,b,current) 
          in
            T(a, y, node)
          end


  fun insert3 (x, E) = T(E, x, E)
    | insert3 (x, s) = check_and_try_insert(x, s, E) handle ExistingElement => s 
  (* End Exercise 2.4 *)

  (* Exercise 2.2  *)
  fun check_candidate(x, E) = false
    | check_candidate(x, T(_, c, _)) = Element.eq(x,c)

  fun check(x, E, candidate) = check_candidate(x,candidate)
    | check(x, T(E, y, E), candidate) = 
        if Element.eq(x, y) then true
        else check_candidate(x, candidate) 
    | check(x, current as T(E, y, b), candidate) = 
        if Element.lt(x, y) then check_candidate(x,candidate)
        else check(x,b,current)
    | check(x, current as T(a, y, E), candidate) = 
        if Element.lt(x, y) then check(x, a, candidate)
        else check_candidate(x, current)
    | check (x, current as T(a, y, b), candidate) = 
        if Element.lt(x, y) then check(x,a,candidate) 
        else check(x,b,current) 

  fun member2 (x, root) = check(x, root, E) 
  (* End Exercise 2.2 *)

  (* Exercise 2.5 *)
  fun complete(e, 0) = E
    | complete(e, x) = 
    let
      val node = complete(e, x - 1)
    in
     T(node, e, node)
    end 
  (* End Exercise 2.5 *)
end 

structure IntOrdered : ORDERED = 
struct
  type T = int
  fun eq(l, r) = l = r
  fun lt(l, r) = l < r
  fun leq(l, r) = l <= r
end

structure IntSet = UnbalancedSet(IntOrdered)
