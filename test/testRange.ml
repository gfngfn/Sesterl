
let test_pp_dummy () =
  let rng = Range.dummy "foo" in
  Alcotest.(check string) "same string" "(foo)" (Format.asprintf "%a" Range.pp rng)


let () =
  let open Alcotest in
  run "Range" [
    ("dummy", [
      test_case "pp dummy" `Quick test_pp_dummy;
    ]);
  ]
