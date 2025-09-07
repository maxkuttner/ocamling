type option_type = Call | Put


let payoff kind ~spot:s ~strike:k = 
  match kind with
  | Call -> max 0.0 (s -. k)
  | Put -> max 0.0 (k -. s)


let black_scholes kind ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t =
  if 
    t <= 0.0 then payoff kind ~spot:s ~strike:k
  else 
    let sqrt_t = sqrt t in
    let d1 = (log (s /. k) +. (r +. sigma *. sigma *. 0.5) *. t) /. (sigma *. sqrt_t) in
    let d2 = d1 -. sigma *. sqrt_t in
    let norm_cdf = Owl.Stats.gaussian_cdf ~mu:0.0 ~sigma:1.0 in
    match kind with 
    | Call -> s *. (norm_cdf d1) -. (norm_cdf d2) *. k *. (exp (-.r *. t))
    | Put  -> k *. (exp (-.r *. t)) *. (norm_cdf (-.d2)) -. s *. (norm_cdf (-.d1))


