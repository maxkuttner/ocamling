(* domain.ml *)
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

let float_opt_of_string s = if s = "" then Some (float_of_string s) else None 
let int_opt_of_string s = if s = "" then Some (int_of_string s) else None

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


let string_of_cboe r =
  Printf.sprintf "underlying: %s, expiration: %s, strike: %.2f" 
    r.underlying r.expiration r.strike

let load_cboe_data filename =
  let rows = Csv.load filename in
  match rows with 
  | [] -> []
  | _header :: data -> List.map cboe_record_of_row data 
