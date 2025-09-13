open Utils

(* Option Type *)
type option_type = Call | Put

(* simple payoffs *)
let payoff kind ~spot:s ~strike:k = 
  match kind with
  | Call -> max 0.0 (s -. k)
  | Put -> max 0.0 (k -. s)

(* black scholes *)
let black_scholes kind ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t =
  if 
    t <= 0.0 then payoff kind ~spot:s ~strike:k
  else 
    let sqrt_t = sqrt t in
    let d1 = (log (s /. k) +. (r +. sigma *. sigma *. 0.5) *. t) /. (sigma *. sqrt_t) in
    let d2 = d1 -. sigma *. sqrt_t in
    match kind with 
    | Call -> s *. (norm_cdf d1) -. (norm_cdf d2) *. k *. (exp (-.r *. t))
    | Put  -> k *. (exp (-.r *. t)) *. (norm_cdf (-.d2)) -. s *. (norm_cdf (-.d1))

(* implied volatility *)
let implied_vol
    ?(tol = 1e-8)
    ?(max_iter = 100)
    ?(sigma_init = 0.2)
    kind ~spot:s ~strike:k ~rate:r ~maturity:t ~price:price =
  if t <= 0.0 then None else
  let bs sigma =
    black_scholes kind ~spot:s ~strike:k ~rate:r ~maturity:t ~vol:sigma in  
  let vega sigma =
    let d1 = (log (s /. k) +. (r +. 0.5 *. sigma *. sigma) *. t) /. (sigma *. sqrt t) in
      s *. norm_pdf d1 *. sqrt t in
  (* Newton iteration scheme to solve for iv *)
  match Newton.newton 
    ~x0:sigma_init ~f:(fun x -> bs x -. price) ~fprime:vega ~tol_f:tol ~tol_x:tol ~max_iter:max_iter with
  | Newton.Ok sigma  -> Some sigma
  | Newton.Fail _    -> None