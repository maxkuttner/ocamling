module Strategy = struct
  open Pricing

  type side = Long | Short

  type leg = {
    side: side;
    kind: option_type;
    k: float;
    t: float;
    qty: float; 
  }

  type strategy = leg list

  let sign = function Long -> 1.0 | Short -> -1.0

  (* ======= Predefined Strategies ======= *)

  (* Payoff: \/ *)
  let straddle ~k ~t ~qty : strategy =
  [
    {side=Long; kind=Call; k=k; t=t; qty=qty};
    {side=Long; kind=Put; k=k; t=t; qty=qty};
  ]
  
  (* Payoff: \_/ *)
  let strangle ~k_put ~k_call ~t ~qty : strategy = 
  [
    {side=Long; kind=Put; k=k_put; t=t; qty=qty};
    {side=Long; kind=Call; k=k_call; t=t; qty=qty};
  ]
  
  (* Payoff: _/\_ *)
  let call_butterfly ~k1 ~k2 ~k3 ~t ~qty : strategy = 
  [
    {side=Long; kind=Call; k=k1; t=t; qty=qty};
    {side=Short; kind=Call; k=k2; t=t; qty=2.0 *. qty};
    {side=Long; kind=Call; k=k3; t=t; qty=qty};
  ]

  (* Payoff: _/\_ *)
  let put_butterfly ~k1 ~k2 ~k3 ~t ~qty : strategy =   
  [
    {side=Long; kind=Put; k=k1; t=t; qty=qty};
    {side=Short; kind=Put; k=k2; t=t; qty=2.0 *. qty};
    {side=Long; kind=Put; k=k3; t=t; qty=qty};
  ]
 
  (* payoff of a single leg *)
  let payoff_leg sT (l : leg) = sign l.side *. l.qty *. payoff l.kind ~spot:sT ~strike:l.k
  
  (* payoff of a strategy *)
  let payoff_strategy sT (legs: strategy) = 
    List.fold_left (fun acc l -> acc +. payoff_leg sT l) 0.0 legs

end


