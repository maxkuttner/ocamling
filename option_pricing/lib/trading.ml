(* domain.ml *)
(* This file holds light-weight input types for adapters and data 
loaders whos job it will be to attach to an external data source like 
.cvs, http, db, etc. and provide raw input these types 
Not sure, if I really need all of this ... but for now I need to get my 
hands on some realistic data such that I can test it out and refactor if needed.
*)



(* Symbol of underlying *)
type symbol = string 
type option_right = Call | Put

type option_symbol = {
  underlying : symbol;      (* "AAPL" *)
  right      : option_right;(* Call|Put *)
  strike     : float;       (* 100.0 *)
  expiry     : string;      (* "2025-11-21" ISO date *)
}

(* OHLCV data point *)
type bar = { t: float; o: float; h: float; l: float; c: float; v: int }

(* Tick data point *)
type quote = { t: float; bid: float; ask: float; bid_size: int; ask_size: int }


(* Market snapshot in time *)
type snapshot = {
  opt     : option_symbol;
  quote   : quote option;
  iv      : float option;  
  greeks  : (float * float * float * float * float) option; 
}

(* Spot Market Data *)
type mkt = { s: float; r: float; q: float; sigma: float }  (* spot, rate, dividend, vol *)