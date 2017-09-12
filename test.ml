
(* compare speed of BatString.starts_with with the one from containers *)

open Printf

let rng =
  Cryptokit.(Random.pseudo_rng (Random.string Random.secure_rng 32))

let () =
  let input = Array.init 100_000
      (fun _ ->
         let str_len = Random.int 80 in
         let str = Cryptokit.Random.string rng str_len in
         let prfx_len = Random.int (str_len + 1) in
         let prfx =
           if Random.bool () then
             String.sub str 0 prfx_len
           else
             String.sub str (str_len - prfx_len) prfx_len
         in
         (prfx, str)
      ) in

  let output = Array.map (fun (prfx, str) ->
      BatString.starts_with str prfx
    ) input in

  let test f nb_iter =
    Array.iteri (fun i (x, y) ->
        assert (f x y = output.(i));
        for i = 1 to nb_iter do
          ignore (f x y);
        done)
      input in

  let bat_starts_with prfx s =
    BatString.starts_with s prfx in

  let cont_starts_with prfx s =
    Containers.String.prefix ~pre:prfx s in

  let tests = ["batteries", test bat_starts_with;
               "containers", test cont_starts_with] in

  Bench.summarize 0.95 (Bench.bench_n tests)
