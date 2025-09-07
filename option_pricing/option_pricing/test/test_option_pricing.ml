open OUnit2
open Option_pricing

let float_eq ?(eps=1e-2) a b = Float.abs (a -. b) < eps
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

let tests = "all" >::: [payoff_suite; bs_suite]
let () = run_test_tt_main tests
