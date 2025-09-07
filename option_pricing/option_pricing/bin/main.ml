let () = 
  let open Option_pricing in 
    let c = payoff Call ~spot:105. ~strike:100. in
    let p = payoff Put ~spot:5. ~strike:100. in 
    Printf.printf "Call payoff: %.4f Put payoff: %.4f" c p