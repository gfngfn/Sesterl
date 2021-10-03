module SnakeCase = struct
  type t = { message : string; input : string; expects : string list option }

  let test (r : t) () =
    let actual =
      IdentifierScheme.from_snake_case r.input
      |> Option.map (fun x -> x.IdentifierScheme.fragments)
    in
    Alcotest.(check (option (list string))) r.message r.expects actual
end

module CamelCase = struct
  type t = { message : string; input : string; expects : string list option }

  let test (r : t) () =
    let actual =
      IdentifierScheme.from_upper_camel_case r.input
      |> Option.map (fun x -> x.IdentifierScheme.fragments)
    in
    Alcotest.(check (option (list string))) r.message r.expects actual
end

let () =
  let open Alcotest in
  run "IdentifierScheme"
    [
      ( "from_snake_case",
        List.map
          (fun tuple -> test_case "equal" `Quick (SnakeCase.test tuple))
          SnakeCase.
            [
              { message = "single"; input = "foo"; expects = Some [ "foo" ] };
              {
                message = "double";
                input = "foo_bar";
                expects = Some [ "foo"; "bar" ];
              };
              {
                message = "triple";
                input = "foo_bar_baz";
                expects = Some [ "foo"; "bar"; "baz" ];
              };
              {
                message = "allow words to start with a digit";
                input = "x86_64";
                expects = Some [ "x86"; "64" ];
              };
              {
                message = "cannot use the empty string";
                input = "";
                expects = None;
              };
              {
                message = "cannot include adjacent underscores";
                input = "foo__bar";
                expects = None;
              };
              {
                message = "cannot begin with an underscore";
                input = "_foo";
                expects = None;
              };
              {
                message = "cannot end with an underscore";
                input = "foo_";
                expects = None;
              };
              {
                message = "cannot include uppercase letters (1)";
                input = "Foo";
                expects = None;
              };
              {
                message = "cannot include uppercase letters (2)";
                input = "fOo";
                expects = None;
              };
              {
                message = "cannot include uppercase letters (1)";
                input = "foo_Bar";
                expects = None;
              };
              {
                message = "cannot include uppercase letters (2)";
                input = "foo_bAr";
                expects = None;
              };
            ] );
      ( "from_upper_camel_case",
        List.map
          (fun tuple -> test_case "equal" `Quick (CamelCase.test tuple))
          CamelCase.
            [
              { message = "single"; input = "Foo"; expects = Some [ "foo" ] };
              {
                message = "double";
                input = "FooBar";
                expects = Some [ "foo"; "bar" ];
              };
              {
                message = "triple";
                input = "FooBarBaz";
                expects = Some [ "foo"; "bar"; "baz" ];
              };
              {
                message = "includes number (1)";
                input = "Foo3Bar";
                expects = Some [ "foo3"; "bar" ];
              };
              {
                message = "includes number (2)";
                input = "Fo3oBar";
                expects = Some [ "fo3o"; "bar" ];
              };
              {
                message = "includes number (3)";
                input = "F3ooBar";
                expects = Some [ "f3oo"; "bar" ];
              };
              {
                message = "includes number (4)";
                input = "Fo42oBar";
                expects = Some [ "fo42o"; "bar" ];
              };
              {
                message = "includes number (5)";
                input = "Foo42Bar";
                expects = Some [ "foo42"; "bar" ];
              };
              {
                message = "includes number (6)";
                input = "FooBar3";
                expects = Some [ "foo"; "bar3" ];
              };
              {
                message = "includes number (7)";
                input = "FooB3ar";
                expects = Some [ "foo"; "b3ar" ];
              };
              {
                message = "underscore + digit";
                input = "X86_64";
                expects = Some [ "x86"; "64" ];
              };
              {
                message =
                  "cannot include underscores that are not followed by digits";
                input = "Foo_Bar";
                expects = None;
              };
              {
                message = "cannot use double underscore";
                input = "X86__64";
                expects = None;
              };
              {
                message = "cannot end with underscores";
                input = "Foo_";
                expects = None;
              };
            ] );
    ]
