
(* Core domain types *)
type option_type = Call | Put

(* Minimal required data - available from ANY provider *)
type option_core = {
  (* Identifiers *)
  underlying: string;
  quote_date: string;
  expiration: string;
  strike: float;
  option_type: option_type;
  
  (* Essential pricing - use mid or last *)
  price: float;  (* close, last, or (bid+ask)/2 *)
  
  (* Context *)
  underlying_price: float;
}

(* Helper functions *)
let float_opt_of_string s = if s = "" then None else Some (float_of_string s) 
let int_opt_of_string s = if s = "" then Some (int_of_string s) else None

(* String conversion for debugging *)
let string_of_option_type = function
  | Call -> "C"
  | Put -> "P"

let string_of_core (opt: option_core) : string =
  Printf.sprintf "t: %s\t{underlying_ticker: %s, expiration: %s, strike: %.2f, option_type: %s, price: %.4f, underlying_price: %.2f}"
    opt.quote_date
    opt.underlying
    opt.expiration
    opt.strike
    (string_of_option_type opt.option_type)
    opt.price
    opt.underlying_price


(* Domain: CBOE *)
module CBOE = struct

  type cboe_record_raw = {
    underlying           : string;
    quote_date           : string;
    root                 : string;
    expiration           : string;
    strike               : float;
    option_type          : string;
    open_                : float option;
    high                 : float option;
    low                  : float option;
    close                : float option;
    trade_volume         : int option;
    (* 15:45 snapshot *)
    bid_size_1545        : int option;
    bid_1545             : float option;
    ask_size_1545        : int option;
    ask_1545             : float option;
    underlying_bid_1545  : float option;
    underlying_ask_1545  : float option;
    implied_under_1545   : float option;
    active_under_1545    : float option;
    iv_1545              : float option;
    delta_1545           : float option;
    gamma_1545           : float option;
    theta_1545           : float option;
    vega_1545            : float option;
    rho_1545             : float option;
    (* EOD snapshot *)
    bid_size_eod         : int option;
    bid_eod              : float option;
    ask_size_eod         : int option;
    ask_eod              : float option;
    underlying_bid_eod   : float option;
    underlying_ask_eod   : float option;
    vwap                 : float option;
    open_interest        : int option;
    delivery_code        : string option;
  }


  let cboe_record_of_row (row : string list) : cboe_record_raw =
    match row with
    | [ underlying; quote_date; root; expiration; strike; opt_type;
        open_; high; low; close; trade_volume;
        bid_sz_1545; bid_1545; ask_sz_1545; ask_1545;
        ubid_1545; uask_1545; impl_u_1545; active_u_1545;
        iv_1545; d_1545; g_1545; th_1545; v_1545; r_1545;
        bid_sz_eod; bid_eod; ask_sz_eod; ask_eod;
        ubid_eod; uask_eod; vwap; oi; delivery_code ] ->
      {
        underlying;
        quote_date;
        root;
        expiration;
        strike = float_of_string strike;
        option_type = opt_type;
        open_ = float_opt_of_string open_;
        high  = float_opt_of_string high;
        low   = float_opt_of_string low;
        close = float_opt_of_string close;
        trade_volume = int_opt_of_string trade_volume;
        
        (* 15:45 snapshot *)
        bid_size_1545 = int_opt_of_string bid_sz_1545;
        bid_1545      = float_opt_of_string bid_1545;
        ask_size_1545 = int_opt_of_string ask_sz_1545;
        ask_1545      = float_opt_of_string ask_1545;
        underlying_bid_1545 = float_opt_of_string ubid_1545;
        underlying_ask_1545 = float_opt_of_string uask_1545;
        implied_under_1545  = float_opt_of_string impl_u_1545;
        active_under_1545   = float_opt_of_string active_u_1545;
        iv_1545   = float_opt_of_string iv_1545;
        delta_1545 = float_opt_of_string d_1545;
        gamma_1545 = float_opt_of_string g_1545;
        theta_1545 = float_opt_of_string th_1545;
        vega_1545 = float_opt_of_string v_1545;
        rho_1545  = float_opt_of_string r_1545;
        
        (* EOD snapshot *)
        bid_size_eod = int_opt_of_string bid_sz_eod;
        bid_eod      = float_opt_of_string bid_eod;
        ask_size_eod = int_opt_of_string ask_sz_eod;
        ask_eod      = float_opt_of_string ask_eod;
        underlying_bid_eod = float_opt_of_string ubid_eod;
        underlying_ask_eod = float_opt_of_string uask_eod;
        vwap          = float_opt_of_string vwap;
        open_interest = int_opt_of_string oi;
        delivery_code = if delivery_code = "" then None else Some delivery_code;
      }
    | _ -> failwith "Unexpected CSV row length for CBOE EOD"


  (* Load raw cboe data from .csv file - Example: https://datashop.cboe.com/option-eod-summary*)
  let load_cboe_data filename =
    let rows = Csv.load filename in
    match rows with 
    | [] -> []
    | _header :: data -> List.map cboe_record_of_row data 

  (* Create a string to print raw cboe record *)
  let string_of_cboe r =
    let r_close = 
      match r.close with 
        | Some x -> Printf.sprintf "%.4f" x
        | None -> "None"  in
    Printf.sprintf "t: %s \t{underlying: %s, expiration: %s, strike: %.2f, option_type: %s, close: %s}" 
      r.quote_date r.underlying r.expiration r.strike r.option_type r_close

  (* Convert CBOE record to option_core *)
  let to_core (cboe: cboe_record_raw) : option_core option =
    (* Parse option type *)
    let opt_type = match cboe.option_type with
      | "C" -> Some Call
      | "P" -> Some Put
      | _ -> None
    in
    
    (* Determine price: prefer close, fallback to mid of bid/ask EOD *)
    let price = match cboe.close with
      | Some p when p > 0.0 -> Some p
      | _ -> 
          match cboe.bid_eod, cboe.ask_eod with
          | Some b, Some a when b > 0.0 && a > 0.0 -> Some ((b +. a) /. 2.0)
          | _ -> None
    in
    
    (* Determine underlying price: prefer EOD, fallback to 1545 *)
    let underlying_price = match cboe.underlying_bid_eod, cboe.underlying_ask_eod with
      | Some b, Some a -> Some ((b +. a) /. 2.0)
      | _ ->
          match cboe.underlying_bid_1545, cboe.underlying_ask_1545 with
          | Some b, Some a -> Some ((b +. a) /. 2.0)
          | _ -> cboe.active_under_1545
    in
    
    (* Build option_core if we have all required fields *)
    match opt_type, price, underlying_price with
    | Some ot, Some p, Some up when p > 0.0 && up > 0.0 ->
        Some {
          underlying = cboe.underlying;
          quote_date = cboe.quote_date;
          expiration = cboe.expiration;
          strike = cboe.strike;
          option_type = ot;
          price = p;
          underlying_price = up;
        }
    | _ -> None  (* Skip records with missing data *)

  (* Load and convert to option_core list *)
  let load_as_core filename : option_core list =
    (* TODO: Think about whether to add a transforamtion or mapping function 
       to load_cboe_data o.w. we iterate twice over all loaded records? *)
    let raw_data = load_cboe_data filename in
    List.filter_map to_core raw_data

end 



