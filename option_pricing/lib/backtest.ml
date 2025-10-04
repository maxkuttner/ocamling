open Trading

type position = {
  entry_date: string;
  expiration: string;
  strike: float;  (* for straddle, same strike; for strangle, ATM reference *)
  call: option_core;
  put: option_core;
  entry_cost: float;  (* net debit/credit *)
  max_loss: float option;
  max_profit: float option;
}

type position_status = Open | Closed of close_reason
and close_reason = 
  | TakeProfit 
  | StopLoss 
  | Expiration
  | DTE_threshold


type trade_result = {
  position: position;
  exit_date: string;
  exit_price: float;
  pnl: float;
  reason: close_reason;
}

type portfolio = {
  cash: float;
  positions: (position * position_status) list;
  closed_trades: trade_result list;
}
