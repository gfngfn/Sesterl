# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Fix how to load test dependencies.
- Fix how to output string/binary literals about non-ASCII characters ([PR\#22](https://github.com/gfngfn/Sesterl/pull/22) by @michallepicki).
- Update GitHub Actions workflow ([PR\#12](https://github.com/gfngfn/Sesterl/pull/12) by @smorimoto).
- Fix how to check type definitions ([PR\#30](https://github.com/gfngfn/Sesterl/pull/30)).

### Added
- Add binary literal patterns ([PR\#28](https://github.com/gfngfn/Sesterl/pull/28)).

## [0.1.2] - 2021-05-29
### Added
- Introduce the notion of attributes of the form `#[foo(…)]`.
- Introduce attributes `#[test]`, `#[behavior(…)]`, and `#[atom(…)]`.
- Add the syntax `assert e` for tracking code positions in unit tests.
- Separate test dependencies from dependencies.
- Collaborate with EUnit.
- Add the syntax `open M`.

### Changed
- Change how to compile `None` and `Some`.

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
- Change output module names from `foo_bar_baz.erl` to `Foo.Bar.Baz.erl`.

### Fixed
- Fix the parser about unit patterns and Boolean patterns.
- Quote global names in order to avoid clashes with keywords.

## [0.1.0] - 2021-05-02
### Added
- Develop the collabration with Rebar3.
- Add the command line `sesterl config <input>` for generating `rebar.config`.

### Changed
- Change the command line spec from `sesterl <input> -o <output>` to `sesterl build <input> -o <output>`.
- Change the syntax of effect types from `[τ]τ` to `fun(τ, …, τ) -> [τ]τ`.
- Separate the syntax of expressions and that of computations by using the newly introduced keyword `act`.

## 0.0.1 - 2020-10-29

The initial release


  [Unreleased]: https://github.com/gfngfn/Sesterl/compare/v0.1.2...HEAD
  [0.1.2]: https://github.com/gfngfn/Sesterl/compare/v0.1.1...v0.1.2
  [0.1.1]: https://github.com/gfngfn/Sesterl/compare/v0.1.0...v0.1.1
  [0.1.0]: https://github.com/gfngfn/Sesterl/compare/v0.0.1...v0.1.0
