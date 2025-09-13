module Newton = struct
  
  (* Result of newton iteration scheme *) 
  type result =
    | Ok of float
    | Fail of string * float

  let newton ~x0:x0 ~f:f ~fprime:f' ~tol_f:tol_f ~tol_x:tol_x ~max_iter:max_iter =
    let rec loop i x =
      let fx = f x in
      (* check if fx is close enough to 0.0 *)
      if Float.abs fx < tol_f then Ok x
      else if i >= max_iter then Fail ("max_iter", x)
      else
        let f'x = f' x in
        (* check if f'x is too close to 0.0 *)
        if not (Float.is_finite f'x) || Float.abs f'x < 1e-14
        then Fail ("flat_derivative", x)
        else
          let step = fx /. f'x in
          let x' = (x -. step) in
          (* check |x - x_0| < max(1, |x_0|) * tol_x 
            this checks for convergence by comapring the absolute error
            close to 0 and the relative error otherwise *)
          if Float.abs (x' -. x) < tol_x *. Float.max 1.0 (Float.abs x)
          then Ok x'
          else loop (i + 1) x' in
    loop 0 x0
end

(* uitls *)
let norm_pdf = Owl.Stats.gaussian_pdf ~mu:0.0 ~sigma:1.0 

let norm_cdf = Owl.Stats.gaussian_cdf ~mu:0.0 ~sigma:1.0 
(*TODO: Positive Domain Newton*)