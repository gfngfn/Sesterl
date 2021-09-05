
open MyUtil
open Syntax
open Env

type config_error =
  | CyclicFileDependencyFound of absolute_path cycle
  | ConfigFileError           of YamlDecoder.error
  | MultipleModuleOfTheSameName of module_name * absolute_path * absolute_path
  | ModuleNotFound              of Range.t * module_name
  | InvalidPackageName          of string
  | CannotSpecifyDependency
  | MainModuleNotFound          of package_name * module_name
  | UnrecognizableExtension     of string
  | ConfigFileNotFound          of absolute_dir
  | SourceFileDependsOnTestFile of module_name * module_name
  | NoOutputSpecForSingleSource
  | UnsupportedLanguageVersion  of string

exception ConfigError of config_error

type package_error =
  | DuplicatedPackageName of package_name * absolute_path * absolute_path
  | PackageDirNotFound    of absolute_dir
  | NotFoundInExternalMap of package_name * external_map

type lexer_error =
  | UnidentifiedToken                of Range.t * string
  | SeeEndOfFileInComment            of Range.t
  | SeeEndOfFileInStringLiteral      of Range.t
  | BlockClosedWithTooManyBackQuotes of Range.t
  | SeeBreakInStringLiteral          of Range.t
  | NotASingleCodePoint              of Range.t
  | UnknownEscapeSequence            of Range.t

type syntax_error =
  | LexerError of lexer_error
  | ParseError of Range.t

type unification_error =
  | Contradiction
  | Inclusion                 of FreeID.t
  | InclusionRow              of FreeRowID.t
  | InsufficientRowConstraint of { id : MustBeBoundRowID.t; given : LabelSet.t; required : LabelSet.t; }

type type_error =
  | UnboundVariable                     of Range.t * identifier
  | UnificationError                    of { actual : mono_type; expected : mono_type; detail : unification_error; }
  | BadArityOfOrderedArguments          of { range : Range.t; got : int; expected : int; }
  | BoundMoreThanOnceInPattern          of Range.t * identifier
  | UnboundTypeParameter                of Range.t * type_variable_name
  | UnboundRowParameter                 of Range.t * row_variable_name
  | UndefinedConstructor                of Range.t * constructor_name
  | InvalidNumberOfConstructorArguments of Range.t * constructor_name * int * int
  | UndefinedTypeName                   of Range.t * type_name
  | UndefinedKindName                   of Range.t * kind_name
  | InvalidNumberOfTypeArguments        of Range.t * type_name * int * int
  | KindContradiction                   of Range.t * type_name * kind * kind
  | TypeParameterBoundMoreThanOnce      of Range.t * type_variable_name
  | RowParameterBoundMoreThanOnce       of Range.t * row_variable_name
  | InvalidByte                         of Range.t
  | CyclicSynonymTypeDefinition         of (type_name ranged) cycle
  | CyclicTypeParameter                 of Range.t * BoundBothID.t cycle * poly_type
  | UnboundModuleName                   of Range.t * module_name
  | NotOfStructureType                  of Range.t * module_signature
  | NotOfFunctorType                    of Range.t * module_signature
  | NotAFunctorSignature                of Range.t * module_signature
  | NotAStructureSignature              of Range.t * module_signature
  | UnboundSignatureName                of Range.t * signature_name
  | CannotRestrictTransparentType       of Range.t * type_name * type_entry
  | PolymorphicContradiction            of Range.t * identifier * poly_type * poly_type
  | PolymorphicInclusion                of Range.t * FreeID.t * poly_type * poly_type
  | MissingRequiredValName              of Range.t * identifier * poly_type
  | MissingRequiredConstructorName      of Range.t * constructor_name * constructor_entry
  | MissingRequiredTypeName             of Range.t * type_name * type_entry
  | MissingRequiredModuleName           of Range.t * module_name * module_signature
  | MissingRequiredSignatureName        of Range.t * signature_name * module_signature abstracted
  | NotASubtype                         of Range.t * module_signature * module_signature
  | NotASubtypeTypeDefinition           of Range.t * type_name * type_entry * type_entry
  | NotASubtypeConstructorDefinition    of Range.t * constructor_name * constructor_entry * constructor_entry
  | NotASubtypeVariant                  of Range.t * TypeID.t * TypeID.t * constructor_name
  | OpaqueIDExtrudesScopeViaValue       of Range.t * poly_type
  | OpaqueIDExtrudesScopeViaType        of Range.t * type_entry
  | OpaqueIDExtrudesScopeViaSignature   of Range.t * module_signature abstracted
  | SupportOnlyFirstOrderFunctor        of Range.t
  | RootModuleMustBeStructure           of Range.t
  | InvalidIdentifier                   of Range.t * string
  | ConflictInSignature                 of Range.t * string
  | DuplicatedLabel                     of Range.t * label
  | UnexpectedMandatoryLabel            of { range : Range.t; label : label; }
  | MissingMandatoryLabel               of { range : Range.t; label : label; typ : mono_type; }
  | UnexpectedOptionalLabel             of { range : Range.t; label : label; }
  | NullaryFormatString                 of Range.t
  | CannotFreezeNonGlobalName           of Range.t * identifier
