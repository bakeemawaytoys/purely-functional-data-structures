(* Exercise 2.1 *)
fun suffixes [] = [[]]
  | suffixes (l as x :: xs)  = l :: suffixes(xs);

signature Stack = 
sig
    type 'a Stack
    val empty		: 'a Stack
    val isEmpty	: 'a Stack -> bool
    val cons		: 'a * 'a Stack -> 'a Stack
    val head		: 'a Stack -> 'a
    val tail		: 'a Stack -> 'a Stack
end

signature Empty =
sig
	val t 	: unit -> bool
end

signature MONAD = 
sig
    type 'a monad
    val fmap : ('a -> 'b) -> ('a monad -> 'b monad)
    val u : 'a -> 'a monad
    val mult : 'a monad monad -> 'a monad
    val bind : 'a monad * ('a -> 'b monad) -> 'b monad    
end

signature LENS =
sig
    type t
    type v
    val get : t -> v
    val set : t * v -> t
end

functor ComposedLens (structure Left : LENS
                     structure Right : LENS
                     sharing type Left.v = Right.t) : LENS = 
struct
    type t = Left.t
    type v = Right.v
    val get = Right.get o Left.get
    fun set (lt,rv) = Left.set(lt,Right.set(Left.get(lt),rv))
end

structure OptionMonad : MONAD =
struct
    type 'a monad = 'a option
    val fmap = fn f => fn ma => case ma of NONE => NONE | SOME(x) => SOME(f(x))
    val u = fn x => SOME(x)
    val mult = fn ma => case ma of NONE => NONE | SOME(x) => (case x of NONE => NONE | SOME(_) => x)
    val bind = fn (ma, f) => mult(fmap(f)(ma))
end

structure List :> Stack = 
struct
    type 'a Stack = 'a list
    val empty = []
    fun isEmpty s = null s
    fun cons (x, s) = x :: s
    fun head s = hd s
    fun tail s = tl s
end

structure List2 :> Stack = 
struct
    type 'a Stack = 'a list
    val empty = []
    fun isEmpty s = null s
    fun cons (x, s) = x :: s
    fun head s = hd s
    fun tail s = tl s
    fun t ()  = true
end
