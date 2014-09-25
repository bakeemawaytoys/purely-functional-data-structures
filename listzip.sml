signature ZIPPER_LIST = 
sig
	type 'a zipper
	val empty : 'a zipper
	val fromList : 'a list -> 'a zipper
	val next : 'a zipper -> 'a zipper
	val prev : 'a zipper -> 'a zipper
	val value : 'a zipper -> 'a option
	val index : 'a zipper -> int
	val null : 'a zipper -> bool
	val insert : ('a zipper * 'a) -> 'a zipper
	val toList : 'a zipper -> 'a list
	val front : 'a zipper -> bool
	val back : 'a zipper -> bool
	val apply : 'a zipper * ('a -> 'a) -> 'a zipper
end

structure ZipperList :> ZIPPER_LIST  =
struct
	type 'a zipper = { index: int, pre : 'a list , post : 'a list }
	val empty  = { index = 0, pre = [] : 'a list, post = [] : 'a list }
	fun fromList l  = { index = 0, pre = [], post = l }
	fun next z = case z
			of { index = i, pre = p , post = []  } => raise Subscript
			| { index = i, pre = p , post = h :: t } => { index = i + 1, pre = h :: p, post = t }
	fun prev z = case z
			of { index = i, pre = [], post = _ } => raise Subscript
			|  { index = i, pre = h ::t , post = p } => { index = i - 1, pre = t, post = h :: p }
	fun value ({post = h :: t, ...} : 'a zipper) = Option.SOME h
	  | value ({post = [], ...} : 'a zipper) = Option.NONE
	fun index ({index = ind, ...} : 'a zipper) : int = ind
	fun null {index = ind, pre = f, post = b } = case (f,b) of ([],[] ) => true
							| _ => false 
	fun toList {index = _, pre = f, post = b } = List.rev(f) @ b
	fun insert ({index = ind, pre = f, post = b}, x) = { index = ind, pre = f, post = x :: b } 
	fun front ({pre = p, ...} : 'a zipper) = List.null p
	fun back ({post = p, ...} : 'a zipper) = List.null p
	fun apply ({index = ind, pre = p, post = h :: t} , f) = {index = ind, pre = p, post = f(h) :: t} 
	  | apply (z, f) = z
end
