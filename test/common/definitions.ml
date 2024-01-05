module IntOrderedEq = struct
  type t = int
  let compare a b = compare a b
  let string_of = string_of_int
end


module Five = struct
  let n = 5
end

module Four = struct
  let n = 5
end

module Three = struct
  let n = 5
end

