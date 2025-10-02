open Camlquant.Trading

let () =
  (* let open Camlquant.Pricing in
  let s, k, r, t, sigma_true = 100., 100., 0., 1., 0.20 in
  let price = black_scholes Call ~spot:s ~strike:k ~rate:r ~maturity:t ~vol:sigma_true in
  (match implied_vol Call ~spot:s ~strike:k ~rate:r ~maturity:t ~price with
  | Some sigma -> Printf.printf "IV ≈ %.8f (true=%.2f)\n" sigma sigma_true
  | None -> print_endline "IV failed");
  Printf.printf "Option price = %.8f\n" price;
  let strdl = Strategy.straddle ~k:k ~t:t ~qty:1.0 in plot_straddle strdl; *)
  let cboe_data = load_cboe_data "/home/max/workarea/ocamling/option_pricing/data/UnderlyingOptionsEODCalcs_2023-08-25_cgi_or_historical.csv" in 
  List.iteri (fun i r -> if i < 5 then print_endline (string_of_cboe r)) cboe_data

