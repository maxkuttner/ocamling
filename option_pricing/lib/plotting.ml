open Strategy
open Owl
open Owl_plplot


let plot_straddle strdl = 
  let payoff x = Strategy.payoff_strategy x strdl in
  (* create 200 points between 60 and 140 *)
  let x = Mat.linspace 60. 140. 200 in
  let y = Mat.map payoff x in
  
  (* file output; no X11 needed *)
  let h = Plot.create "./plots/straddle.png" in   
  Plot.set_title h "Straddle payoff";
  Plot.set_xlabel h "S_T";
  Plot.set_ylabel h "Payoff";
  Plot.plot ~h x y;
  (* write the PNG *)
  Plot.output h               