open Owl
open Owl_plplot
open Camlquant.Strategy

let () =
  let open Camlquant.Pricing in
  let s, k, r, t, sigma_true = 100., 100., 0., 1., 0.20 in
  let price = black_scholes Call ~spot:s ~strike:k ~rate:r ~maturity:t ~vol:sigma_true in
  (match implied_vol Call ~spot:s ~strike:k ~rate:r ~maturity:t ~price with
  | Some sigma -> Printf.printf "IV ≈ %.8f (true=%.2f)\n" sigma sigma_true
  | None -> print_endline "IV failed");
  Printf.printf "Option price = %.8f\n" price;
  
  (* create a straddle contract *)
  let strdl = Strategy.straddle ~k:k ~t:t ~qty:1.0 in
  let payoff x = Strategy.payoff_strategy x strdl in
  (* create 200 points between 60 and 140 *)
  let x = Mat.linspace 60. 140. 200 in
  let y = Mat.map payoff x in
  let h = Plot.create "./plots/straddle.png" in   (* file output; no X11 needed *)
  Plot.set_title h "Straddle payoff";
  Plot.set_xlabel h "S_T";
  Plot.set_ylabel h "Payoff";
  Plot.plot ~h x y;
  Plot.output h               (* write the PNG *)