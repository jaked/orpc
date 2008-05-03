module List =
struct
  include List

  let foldi_right f l b =
    let rec loop l i =
      match l with
        | [] -> b
        | e::l -> f e i (loop l (i + 1)) in
    loop l 0

  let mapi f l =
    foldi_right
      (fun e i l -> f e i :: l)
      l
      []
end
