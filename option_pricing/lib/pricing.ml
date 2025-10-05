open Utils
open Types

(* simple payoffs *)
let payoff kind ~spot:s ~strike:k = 
  match kind with
  | Call -> max 0.0 (s -. k)
  | Put -> max 0.0 (k -. s)


(* BS Helpers*)
let d1 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t = 
  (log (s /. k) +. (r +. sigma *. sigma *. 0.5) *. t) /. (sigma *. sqrt t)

let d2 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t = 
    let d1v = d1 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in 
    d1v -. sigma *. sqrt t

(* Greeks *)

(* 1st derivative wrt. spot *)
let delta kind ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t= 
  let d1v = d1 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
  match kind with
  | Call -> norm_cdf d1v
  | Put -> norm_cdf d1v -. 1.0

(* 2st derivatives wrt. spot *)
let gamma ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t = 
  let d1v = d1 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
  norm_pdf d1v /. (s *. sigma *. sqrt t)

(* 1st derivative wrt. sigma *)
let vega ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t = 
  let d1v = d1 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
  s *. norm_pdf d1v *. sqrt t

(* 1st derivative wrt. maturity *)
let theta kind ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t =
  let d1v = d1 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
  let d2v = d2 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
  let term_time = -. (s *. norm_pdf d1v *. sigma) /. (2.0 *. sqrt t) in
  let term_rate = r *. s *. exp (-. r *. t) in
  match kind with
  | Call -> term_time -. term_rate *. norm_cdf d2v
  | Put  -> term_time +. term_rate *. norm_cdf (-. d2v)

(* 1st derivateive wrt. to interest rate*)
let rho kind ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t =
  let d2v = d2 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
  match kind with
    | Call -> k *. t *. exp (-. r *. t) *. norm_cdf d2v
    | Put -> -. k *. t *. exp (-. r *. t) *. norm_cdf (-. d2v)


  (* black scholes *)
let black_scholes kind ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t =
  if 
    t <= 0.0 then payoff kind ~spot:s ~strike:k
  else 
    let d1v = d1 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in 
    let d2v = d2 ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
    match kind with 
    | Call -> s *. (norm_cdf d1v) -. (norm_cdf d2v) *. k *. (exp (-.r *. t))
    | Put  -> k *. (exp (-.r *. t)) *. (norm_cdf (-.d2v)) -. s *. (norm_cdf (-.d1v))

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
