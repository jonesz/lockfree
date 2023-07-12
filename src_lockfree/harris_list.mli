type 'a t

val create : unit -> 'a t
val add : 'a t -> 'a -> bool
val remove : 'a t -> 'a -> bool
val contains : 'a t -> 'a -> bool
