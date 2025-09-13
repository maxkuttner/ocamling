open OUnit2
open Camlquant.Pricing
open Camlquant.Strategy


let float_eq ?(eps=1e-12) a b = Float.abs (a -. b) < eps
let float_close = float_eq

let payoff_suite =
  "option_pricing payoff" >::: [
    "call_in_the_money" >:: (fun _ ->
      assert_bool "C(105,100) ~ 5"
        (float_eq (payoff Call ~spot:105. ~strike:100.) 5.)
    );

    "call_out_of_the_money" >:: (fun _ ->
      assert_bool "C(80,100) ~ 0"
        (float_eq (payoff Call ~spot:80. ~strike:100.) 0.)
    );

    "put_in_the_money" >:: (fun _ ->
      assert_bool "P(80,100) ~ 20"
        (float_eq (payoff Put ~spot:80. ~strike:100.) 20.)
    );

    "put_out_of_the_money" >:: (fun _ ->
      assert_bool "P(120,100) ~ 0"
        (float_eq (payoff Put ~spot:120. ~strike:100.) 0.)
    );
  ]

let bs_suite =
  "black_scholes" >::: [
    (* Test whether the put-call parity holds *)
    "put_call_parity" >:: (fun _ ->
      let s, k, r, sigma, t = (120., 100., 0.03, 0.25, 0.75) in
      let c = black_scholes Call ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
      let p = black_scholes Put  ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
      let rhs = s -. k *. exp (-. r *. t) in
      assert_bool "C - P ≈ S - K e^{-rT}" (float_close (c -. p) rhs);
    );
  ]

  let iv_suite =
    "implied_volatility" >::: [
      "call" >:: (fun _ -> 
        let s, k, r, sigma, t = (120., 100., 0.03, 0.25, 0.75) in
        let c = black_scholes Call ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
        let iv = (
        match implied_vol Call ~spot:s ~strike:k ~rate:r ~maturity:t ~price:c with
        | Some sigma -> sigma
        | None -> 9999.9999 ) in
        assert_bool "iv = sigma" (float_close iv sigma);
      );
      "put" >:: (fun _ ->
        let s, k, r, sigma, t = (120., 100., 0.03, 0.25, 0.75) in
        let p = black_scholes Put  ~spot:s ~strike:k ~rate:r ~vol:sigma ~maturity:t in
        let iv = (
        match implied_vol Put ~spot:s ~strike:k ~rate:r ~maturity:t ~price:p with
        | Some sigma -> sigma
        | None -> 9999.9999 ) in
        assert_bool "iv = sigma" (float_close iv sigma);
      )
    ]
  
  let test_straddle_at_k _ =
    let k, t, qty = 100., 30. /. 365., 1.0 in
    let strat = Strategy.straddle ~k ~t ~qty in
    let p = Strategy.payoff_strategy k strat in
    assert_bool "straddle payoff at K should be 0" (float_close p 0.)

  let test_straddle_above_k _ =
    let k, t, qty = 100., 30. /. 365., 1.0 in
    let strat = Strategy.straddle ~k ~t ~qty in
    let sT = 110. in
    let p = Strategy.payoff_strategy sT strat in
    assert_bool "straddle payoff above K = |S_T-K|"
      (float_close p 10.)

  let test_straddle_below_k _ =
    let k, t, qty = 100., 30. /. 365., 1.0 in
    let strat = Strategy.straddle ~k ~t ~qty in
    let sT = 85. in
    let p = Strategy.payoff_strategy sT strat in
    assert_bool "straddle payoff below K = |S_T-K|"
      (float_close p 15.)

  let test_straddle_qty_scales _ =
    let k, t = 100., 30. /. 365. in
    let sT = 112. in
    let p1 = Strategy.payoff_strategy sT (Strategy.straddle ~k ~t ~qty:1.5) in
    let p2 = Strategy.payoff_strategy sT (Strategy.straddle ~k ~t ~qty:3.0) in
    assert_bool "doubling qty doubles payoff" (float_close (2.0 *. p1) p2)

  let strategy_suite =
    "strategy" >::: [
      "straddle at K"      >:: test_straddle_at_k;
      "straddle above K"   >:: test_straddle_above_k;
      "straddle below K"   >:: test_straddle_below_k;
      "straddle qty scale" >:: test_straddle_qty_scales;
    ]

let tests = "all" >::: [payoff_suite; bs_suite; iv_suite; strategy_suite]
let () = run_test_tt_main tests
