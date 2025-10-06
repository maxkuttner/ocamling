(* Instrument types *)


type stock = {
  symbol: string;
  date: string;
  price: float;
}

(* Option (derivative) types *)
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


type instrument = 
  | Stock of stock
  | Option of option_core




(* Position-related types *)
type side = Long | Short

type leg = {
  instrument: instrument;
  side: side;
  quantity: float;
  entry_price: float
}

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
  legs: leg list; (* This can also be a recursive type -> position*)
  status: position_status;
}

