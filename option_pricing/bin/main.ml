open Camlquant.Types
open Camlquant.Backtest

let () =
  (* let open Camlquant.Pricing in
  let s, k, r, t, sigma_true = 100., 100., 0., 1., 0.20 in
  let price = black_scholes Call ~spot:s ~strike:k ~rate:r ~maturity:t ~vol:sigma_true in
  (match implied_vol Call ~spot:s ~strike:k ~rate:r ~maturity:t ~price with
  | Some sigma -> Printf.printf "IV ≈ %.8f (true=%.2f)\n" sigma sigma_true
  | None -> print_endline "IV failed");
  Printf.printf "Option price = %.8f\n" price;
  let strdl = Strategy.straddle ~k:k ~t:t ~qty:1.0 in plot_straddle strdl; *)
   

  (*TEST: loading cboe option data *)

  (*let core_data = CBOE.load_as_core "./data/UnderlyingOptionsEODCalcs_2023-08-25_cgi_or_historical.csv" in 
  List.iteri (fun i r -> if i < 1000 then print_endline (string_of_core r)) core_data *)
  let stock_pos = stock_position 
    ~symbol:"AAPL" 
    ~date:"2025-01-15" 
    ~price:175.50 
    ~quantity:100.0 
    ~side:Long in
  
  Printf.printf "=== Stock Position ===\n";
  Printf.printf "ID: %s\n" stock_pos.id;
  Printf.printf "Description: %s\n" stock_pos.description;
  Printf.printf "Entry Cost: $%.2f\n\n" (entry_cost stock_pos);

  (* Example 2: Single Call Option - Long 1 SPY Call *)
  let call_opt = {
    underlying = "SPY";
    quote_date = "2025-01-15";
    expiration = "2025-03-21";
    strike = 450.0;
    option_type = Call;
    price = 12.50;
    underlying_price = 445.0;
  } in
  
  let call_pos = single_option_position call_opt ~quantity:1.0 ~side:Long in
  
  Printf.printf "=== Single Call Option ===\n";
  Printf.printf "ID: %s\n" call_pos.id;
  Printf.printf "Description: %s\n" call_pos.description;
  Printf.printf "Entry Cost: $%.2f\n\n" (entry_cost call_pos);

  (* Example 3: Single Put Option - Short 1 SPY Put (selling premium) *)
  let put_opt = {
    underlying = "SPY";
    quote_date = "2025-01-15";
    expiration = "2025-02-21";
    strike = 430.0;
    option_type = Put;
    price = 3.75;
    underlying_price = 445.0;
  } in
  
  let put_pos = single_option_position put_opt ~quantity:1.0 ~side:Short in
  
  Printf.printf "=== Single Put Option (Short) ===\n";
  Printf.printf "ID: %s\n" put_pos.id;
  Printf.printf "Description: %s\n" put_pos.description;
  Printf.printf "Entry Cost: $%.2f (credit received)\n\n" (entry_cost put_pos);

  (* Example 4: Short Strangle - Sell OTM Call and Put *)
  let strangle_call = {
    underlying = "SPY";
    quote_date = "2025-01-15";
    expiration = "2025-02-21";
    strike = 460.0;
    option_type = Call;
    price = 2.80;
    underlying_price = 445.0;
  } in
  
  let strangle_put = {
    underlying = "SPY";
    quote_date = "2025-01-15";
    expiration = "2025-02-21";
    strike = 430.0;
    option_type = Put;
    price = 2.50;
    underlying_price = 445.0;
  } in
  
  let strangle_pos = strangle_position 
    strangle_call 
    strangle_put 
    ~quantity:1.0 
    ~side:Short in
  
  Printf.printf "=== Short Strangle ===\n";
  Printf.printf "ID: %s\n" strangle_pos.id;
  Printf.printf "Description: %s\n" strangle_pos.description;
  Printf.printf "Entry Cost: $%.2f (credit received)\n" (entry_cost strangle_pos);
  Printf.printf "Number of legs: %d\n\n" (List.length strangle_pos.legs);

  (* Example 5: Long Strangle - Buy OTM Call and Put for volatility play *)
  let long_strangle_call = {
    underlying = "TSLA";
    quote_date = "2025-01-15";
    expiration = "2025-03-21";
    strike = 280.0;
    option_type = Call;
    price = 15.40;
    underlying_price = 250.0;
  } in
  
  let long_strangle_put = {
    underlying = "TSLA";
    quote_date = "2025-01-15";
    expiration = "2025-03-21";
    strike = 220.0;
    option_type = Put;
    price = 14.20;
    underlying_price = 250.0;
  } in
  
  let long_strangle_pos = strangle_position 
    long_strangle_call 
    long_strangle_put 
    ~quantity:1.0 
    ~side:Long in
  
  Printf.printf "=== Long Strangle (Volatility Play) ===\n";
  Printf.printf "ID: %s\n" long_strangle_pos.id;
  Printf.printf "Description: %s\n" long_strangle_pos.description;
  Printf.printf "Entry Cost: $%.2f\n\n" (entry_cost long_strangle_pos);

  (* Example 6: Iron Condor - Complex 4-leg strategy *)
  let ic_put_long = {
    underlying = "SPY";
    quote_date = "2025-01-15";
    expiration = "2025-02-21";
    strike = 420.0;
    option_type = Put;
    price = 1.20;
    underlying_price = 445.0;
  } in
  
  let ic_put_short = {
    underlying = "SPY";
    quote_date = "2025-01-15";
    expiration = "2025-02-21";
    strike = 430.0;
    option_type = Put;
    price = 2.50;
    underlying_price = 445.0;
  } in
  
  let ic_call_short = {
    underlying = "SPY";
    quote_date = "2025-01-15";
    expiration = "2025-02-21";
    strike = 460.0;
    option_type = Call;
    price = 2.80;
    underlying_price = 445.0;
  } in
  
  let ic_call_long = {
    underlying = "SPY";
    quote_date = "2025-01-15";
    expiration = "2025-02-21";
    strike = 470.0;
    option_type = Call;
    price = 1.30;
    underlying_price = 445.0;
  } in
  
  let ic_pos = iron_condor_position 
    ic_put_long 
    ic_put_short 
    ic_call_short 
    ic_call_long 
    ~quantity:1.0 in
  
  Printf.printf "=== Iron Condor ===\n";
  Printf.printf "ID: %s\n" ic_pos.id;
  Printf.printf "Description: %s\n" ic_pos.description;
  Printf.printf "Entry Cost: $%.2f (credit received)\n" (entry_cost ic_pos);
  Printf.printf "Number of legs: %d\n" (List.length ic_pos.legs);
  Printf.printf "Max profit: $%.2f\n" (abs_float (entry_cost ic_pos));
  Printf.printf "Max loss: $%.2f\n\n" (10.0 *. 100.0 -. abs_float (entry_cost ic_pos));
  

  (* Summary *)
  Printf.printf "=== Summary ===\n";
  Printf.printf "Total positions created: 6\n";
  Printf.printf "- 1 stock position\n";
  Printf.printf "- 2 single option positions\n";
  Printf.printf "- 2 strangle positions\n";
  Printf.printf "- 1 iron condor position\n"

  





