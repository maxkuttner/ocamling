(* Define the type *)
type word = {
  english : string;
  german : string;
  russian : string;
}

(* File path for storing vocab *)
let vocab_file = "vocab.json"

let languages = ["english"; "german"; "russian"]


(* Converts a word to Yojson *)
let word_to_yojson (w : word) =
  `Assoc [
    ("english", `String w.english);
    ("german", `String w.german);
    ("russian", `String w.russian)
  ]



(* Converts Yojson to a word *)
let word_of_yojson = function
  | `Assoc [("english", `String e); ("german", `String g); ("russian", `String r)] ->
      {english = e; german = g; russian = r}
  | _ -> failwith "Invalid word format in JSON"




(* Load vocab from file *)
let load_vocab () =
  if Sys.file_exists vocab_file then
    let json = Yojson.Basic.from_file vocab_file in
    match json with
    | `List lst -> List.map word_of_yojson lst
    | _ -> []
  else 
     []



(* Save vocab to file *)
let save_vocab (words : word list) =
  let json = `List (List.map word_to_yojson words) in
  Yojson.Basic.to_file vocab_file json




(* Mutable list holding vocab *)
let vocab : word list ref = ref (load_vocab ())

let ask lang =
  Printf.printf "Enter word in %s: " lang;
  read_line ()

let add_word () =
  let answers = List.map ask languages in
  match answers with
  | [english; german; russian] ->
      let new_word = {english; german; russian} in
      vocab := new_word :: !vocab;
      save_vocab !vocab
  | _ -> failwith "Please provide all translations!"

let () = add_word ()

