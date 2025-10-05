open Trading

(* This shall replace the option specific strategy module*)


(* Core instrument types *)
type option_type = Call | Put

type option_core = {
  underlying: string;
  quote_date: string;
  expiration: string;
  strike: float;
  option_type: option_type;
  price: float;
  underlying_price: float;
}

type stock = {
  symbol: string;
  date: string;
  price: float;
}

type instrument = 
  | Stock of stock
  | Option of option_core


type side = Long | Short

(* Generic instrument leg *)
type leg = {
  instrument: instrument;
  side: side;
  quantity: float;
  entry_price: float
}

(* Position status *)
type position_status = 
  | Open 
  | Closed of close_reason and
  close_reason = 
    | TakeProfit 
    | StopLoss 
    | Expiration
    | DTE_threshold
    | Manual


(* A position is one or more legs *)
type position = {
  id: string;  (* Unique identifier *)
  entry_date: string;
  description: string;  (* "Long 100 AAPL", "Short Straddle SPY 440", etc *)
  legs: leg list;
  status: position_status;
}


(* Helper functions *)

let sign = function Long -> 1.0 | Short -> -1.0

(* Get the underlying symbol of an instrument *)
let underlying_of_instrument = function
  | Stock s -> s.symbol
  | Option o -> o.underlying


(* Total entry cost of a position (negative = credit received) *)
let entry_cost (pos: position) : float =
  List.fold_left (fun acc leg ->
    acc +. (sign leg.side *. leg.quantity *. leg.entry_price)
  ) 0.0 pos.legs


let stock_position ~symbol ~date ~price ~quantity ~side : position =
  let stock = { symbol; date; price } in
    let desc = Printf.sprintf "%s %.0f %s" 
      (match side with Long -> "Long" | Short -> "Short")
      quantity symbol in
    {
      id = Printf.sprintf "%s_%s" symbol date;
      entry_date = date;
      description = desc;
      legs = [{
        instrument = Stock stock;
        side;
        quantity;
        entry_price = price;
      }];
      status = Open;
    }


(* Single option position *)
let single_option_position (opt: option_core) ~quantity ~side : position =
  let opt_type_str = match opt.option_type with Call -> "Call" | Put -> "Put" in
  let desc = Printf.sprintf "%s %.0f %s %s %.2f %s" 
    (match side with Long -> "Long" | Short -> "Short")
    quantity opt.underlying opt_type_str opt.strike opt.expiration in
  {
    id = Printf.sprintf "%s_%s_%.2f_%s_%s" 
      opt.underlying opt.expiration opt.strike opt_type_str opt.quote_date;
    entry_date = opt.quote_date;
    description = desc;
    legs = [{
      instrument = Option opt;
      side;
      quantity;
      entry_price = opt.price;
    }];
    status = Open;
  }

(* Strangle: long call + long put at different strikes *)
let strangle_position (call_opt: option_core) (put_opt: option_core) 
                      ~quantity ~side : position =
  let side_str = match side with Long -> "Long" | Short -> "Short" in
  let desc = Printf.sprintf "%s Strangle %s %.2f/%.2f %s" 
    side_str call_opt.underlying put_opt.strike call_opt.strike call_opt.expiration in
  let legs = [
      {
        instrument = Option call_opt;
        side;
        quantity;
        entry_price = call_opt.price;
      };
      {
        instrument = Option put_opt;
        side;
        quantity;
        entry_price = put_opt.price;
      };
    ] in
  let id = Printf.sprintf "strangle_%s_%.2f_%.2f_%s_%s" 
      call_opt.underlying put_opt.strike call_opt.strike 
      call_opt.expiration call_opt.quote_date in
  { 
    id = id;
    entry_date = call_opt.quote_date;
    description = desc;
    legs = legs;   
    status = Open;
  }

(* Iron Condor: sell OTM put spread + sell OTM call spread *)
let iron_condor_position (put_long: option_core) (put_short: option_core)
                         (call_short: option_core) (call_long: option_core)
                         ~quantity : position =
  let desc = Printf.sprintf "Iron Condor %s %.2f/%.2f/%.2f/%.2f %s" 
    put_long.underlying put_long.strike put_short.strike 
    call_short.strike call_long.strike put_long.expiration in
  {
    id = Printf.sprintf "ic_%s_%s_%s" 
      put_long.underlying put_long.expiration put_long.quote_date;
    entry_date = put_long.quote_date;
    description = desc;
    legs = [
      { instrument = Option put_long; side = Long; quantity; entry_price = put_long.price };
      { instrument = Option put_short; side = Short; quantity; entry_price = put_short.price };
      { instrument = Option call_short; side = Short; quantity; entry_price = call_short.price };
      { instrument = Option call_long; side = Long; quantity; entry_price = call_long.price };
    ];
    status = Open;
  }
