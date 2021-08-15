
type test_case = {
  before  : string;
  after   : string;
  expects : bool;
}


let test_is_compatible (r : test_case) () =
  match (LanguageVersion.parse r.before, LanguageVersion.parse r.after) with
  | (Some(before), Some(after)) ->
      let message = Printf.sprintf "(%s, %s)" r.before r.after in
      Alcotest.(check bool) message r.expects (LanguageVersion.is_compatible ~before ~after)

  | _ ->
      Alcotest.fail "parse failed"


let () =
  let open Alcotest in
  run "LanguageVersion" [
    ("is_compatible", List.map (fun r ->
      test_case "check" `Quick (test_is_compatible r))
      [
        { before  = "v0.1.3";
          after   = "v0.1.4";
          expects = true;
        };
        { before  = "v0.1.4";
          after   = "v0.1.4";
          expects = true;
        };
        { before  = "v0.1.5";
          after   = "v0.1.4";
          expects = false;
        };
        { before  = "v0.1.3";
          after   = "v0.2.4";
          expects = false;
        };
        { before  = "v0.1.5";
          after   = "v0.2.4";
          expects = false;
        };
        { before  = "v0.1.3";
          after   = "v1.2.4";
          expects = false;
        };
        { before  = "v1.1.5";
          after   = "v1.2.4";
          expects = true;
        };
      ]
    );
  ]
