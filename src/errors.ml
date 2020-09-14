
open Syntax
open Env

type lexer_error =
  | UnidentifiedToken                of Range.t * string
  | SeeEndOfFileInComment            of Range.t
  | SeeEndOfFileInStringLiteral      of Range.t
  | BlockClosedWithTooManyBackQuotes of Range.t
  | SeeBreakInStringLiteral          of Range.t
  | NotASingleCodePoint              of Range.t

type type_error =
  | UnboundVariable                     of Range.t * identifier
  | ContradictionError                  of mono_type * mono_type
  | InclusionError                      of FreeID.t * mono_type * mono_type
  | InclusionRowError                   of FreeRowID.t * mono_type * mono_type
  | BoundMoreThanOnceInPattern          of Range.t * identifier
  | UnboundTypeParameter                of Range.t * type_variable_name
  | UnboundRowParameter                 of Range.t * row_variable_name
  | UndefinedConstructor                of Range.t * constructor_name
  | InvalidNumberOfConstructorArguments of Range.t * constructor_name * int * int
  | UndefinedTypeName                   of Range.t * type_name
  | UndefinedKindName                   of Range.t * kind_name
  | InvalidNumberOfTypeArguments        of Range.t * type_name * int * int
  | TypeParameterBoundMoreThanOnce      of Range.t * type_variable_name
  | RowParameterBoundMoreThanOnce       of Range.t * row_variable_name
  | InvalidByte                         of Range.t
  | CyclicSynonymTypeDefinition         of (type_name ranged) TupleList.t
  | UnboundModuleName                   of Range.t * module_name
  | NotOfStructureType                  of Range.t * module_signature
  | NotOfFunctorType                    of Range.t * module_signature
  | NotAFunctorSignature                of Range.t * module_signature
  | NotAStructureSignature              of Range.t * module_signature
  | UnboundSignatureName                of Range.t * signature_name
  | CannotRestrictTransparentType       of Range.t * type_opacity
  | PolymorphicContradiction            of Range.t * identifier * poly_type * poly_type
  | PolymorphicInclusion                of Range.t * FreeID.t * poly_type * poly_type
  | MissingRequiredValName              of Range.t * identifier * poly_type
  | MissingRequiredTypeName             of Range.t * type_name * type_opacity
  | MissingRequiredModuleName           of Range.t * module_name * module_signature
  | MissingRequiredSignatureName        of Range.t * signature_name * module_signature abstracted
  | NotASubtype                         of Range.t * module_signature * module_signature
  | NotASubtypeTypeOpacity              of Range.t * type_name * type_opacity * type_opacity
  | NotASubtypeVariant                  of Range.t * TypeID.Variant.t * TypeID.Variant.t * constructor_name
  | NotASubtypeSynonym                  of Range.t * TypeID.Synonym.t * TypeID.Synonym.t
  | OpaqueIDExtrudesScopeViaValue       of Range.t * poly_type
  | OpaqueIDExtrudesScopeViaType        of Range.t * type_opacity
  | OpaqueIDExtrudesScopeViaSignature   of Range.t * module_signature abstracted
  | SupportOnlyFirstOrderFunctor        of Range.t
  | RootModuleMustBeStructure           of Range.t
  | InvalidIdentifier                   of Range.t * string
  | ConflictInSignature                 of Range.t * string
  | DuplicatedLabel                     of Range.t * label
