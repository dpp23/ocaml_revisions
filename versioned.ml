class type virtual versioned_abstract = object

  method virtual merge : unit -> unit
  method virtual get : unit ->'a
  method virtual set : 'a -> unit
  
end

class type ['a] versioned  (init : 'a) = object
  inherit virtual versioned_abstract

  val map : (int, 'a, Comparator.Poly.comparator) Map.t (** Alternatives to the polymorphic comparator? **)
                                                        (** User supplied compare function?**)

end

class ['a] versioned (init: 'a) = object

  val map = Map.of_alist_exn ~comparator:Comparator.Poly.comparator [1,init]

  method get_map = function () -> map

end;;


class ['a] versioned (init: 'a) = object
  inherit ver_abst 
  val map = Map.of_alist_exn ~comparator:Int.comparator [1,init]
  method check = function () -> 5
  method get_map = function () -> map

end;;

