# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Add a new field `language` to the config file format ([PR\#36](https://github.com/gfngfn/Sesterl/pull/36)).

### Changed
- Change the typing rules for records from a record polymorphism similar to that of SML\# to a kind of row polymorphism ([PR\#39](https://github.com/gfngfn/Sesterl/pull/39); **breaking change**).
- Change the type for the hole `~s` in patterns from `list<char>` to `binary` ([PR\#33](https://github.com/gfngfn/Sesterl/pull/33); **breaking change**).

## [0.1.5] - 2021-08-14
### Fixed
- Fix an unsound type-checking behavior about record kinds ([PR\#35](https://github.com/gfngfn/Sesterl/pull/35)).

## [0.1.4] - 2021-07-15
### Changed
- Rename configuration files from `package.yaml` to `sesterl.yaml` while providing a fallback mechanism ([PR\#32](https://github.com/gfngfn/Sesterl/pull/32) by @michallepicki).

## [0.1.3] - 2021-07-11
### Fixed
- Fix how to load test dependencies.
- Fix how to output string/binary literals about non-ASCII characters ([PR\#22](https://github.com/gfngfn/Sesterl/pull/22) by @michallepicki).
- Update GitHub Actions workflow ([PR\#12](https://github.com/gfngfn/Sesterl/pull/12) by @smorimoto).
- Fix how to check type definitions ([PR\#30](https://github.com/gfngfn/Sesterl/pull/30)).
- Fix how to perform the universal quantification ([PR\#31](https://github.com/gfngfn/Sesterl/pull/31)).

### Added
- Add binary literal patterns ([PR\#28](https://github.com/gfngfn/Sesterl/pull/28)).
- Support fully-annotated polymorphic recursion ([PR\#31](https://github.com/gfngfn/Sesterl/pull/31)).

## [0.1.2] - 2021-05-29
### Added
- Introduce the notion of attributes of the form `#[foo(…)]`.
- Introduce attributes `#[test]`, `#[behavior(…)]`, and `#[atom(…)]`.
- Add the syntax `assert e` for tracking code positions in unit tests.
- Separate test dependencies from dependencies.
- Collaborate with EUnit.
- Add the syntax `open M`.

### Changed
- Change how to compile `None` and `Some` (**breaking change for FFIs**).

### Fixed
- Largely fix the type-checking algorithm (mainly about how to track type synonyms).
- Fix how to treat relative paths given via command lines.

## [0.1.1] - 2021-05-16
### Added
- Add the syntax sugar of list patterns.
- Add patterns of the form `Module.Constructor`.
- Add the variant type `result`.
- Add first-class modules based on the formalization of F-ing modules.
- Add option `-p` for specifying paths of external packages, which will be used mainly for the collaboration with Rebar3.

### Changed
- Change output module names from `foo_bar_baz.erl` to `Foo.Bar.Baz.erl` (**breaking change for FFIs**).

### Fixed
- Fix the parser about unit patterns and Boolean patterns.
- Quote global names in order to avoid clashes with keywords.

## [0.1.0] - 2021-05-02
### Added
- Develop the collabration with Rebar3.
- Add the command line `sesterl config <input>` for generating `rebar.config`.

### Changed
- Change the command line spec from `sesterl <input> -o <output>` to `sesterl build <input> -o <output>`.
- Change the syntax of effect types from `[τ]τ` to `fun(τ, …, τ) -> [τ]τ` (**breaking change**).
- Separate the syntax of expressions and that of computations by using the newly introduced keyword `act` (**breaking change**).

## 0.0.1 - 2020-10-29

The initial release


  [Unreleased]: https://github.com/gfngfn/Sesterl/compare/v0.1.5...HEAD
  [0.1.5]: https://github.com/gfngfn/Sesterl/compare/v0.1.4...v0.1.5
  [0.1.4]: https://github.com/gfngfn/Sesterl/compare/v0.1.3...v0.1.4
  [0.1.3]: https://github.com/gfngfn/Sesterl/compare/v0.1.2...v0.1.3
  [0.1.2]: https://github.com/gfngfn/Sesterl/compare/v0.1.1...v0.1.2
  [0.1.1]: https://github.com/gfngfn/Sesterl/compare/v0.1.0...v0.1.1
  [0.1.0]: https://github.com/gfngfn/Sesterl/compare/v0.0.1...v0.1.0
