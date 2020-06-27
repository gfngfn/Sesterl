
open Syntax
open Env

exception UnboundVariable                     of Range.t * identifier
exception ContradictionError                  of mono_type * mono_type
exception InclusionError                      of FreeID.t * mono_type * mono_type
exception BoundMoreThanOnceInPattern          of Range.t * identifier
exception UnboundTypeParameter                of Range.t * type_variable_name
exception UndefinedConstructor                of Range.t * constructor_name
exception InvalidNumberOfConstructorArguments of Range.t * constructor_name * int * int
exception UndefinedTypeName                   of Range.t * type_name
exception InvalidNumberOfTypeArguments        of Range.t * type_name * int * int
exception TypeParameterBoundMoreThanOnce      of Range.t * type_variable_name
exception InvalidByte                         of Range.t
exception CyclicSynonymTypeDefinition         of (type_name ranged) list
exception UnboundModuleName                   of Range.t * module_name
exception NotOfStructureType                  of Range.t * concrete_module_signature
exception NotOfFunctorType                    of Range.t * concrete_module_signature
exception NotAFunctorSignature                of Range.t * concrete_module_signature
exception NotAStructureSignature              of Range.t * concrete_module_signature
exception UnboundSignatureName                of Range.t * signature_name
exception CannotRestrictTransparentType       of Range.t * type_opacity
exception PolymorphicContradiction            of Range.t * identifier * poly_type * poly_type
exception PolymorphicInclusion                of Range.t * FreeID.t * poly_type * poly_type
exception MissingRequiredValName              of Range.t * identifier * poly_type
exception MissingRequiredTypeName             of Range.t * type_name * type_opacity
exception MissingRequiredModuleName           of Range.t * module_name * concrete_module_signature
exception MissingRequiredSignatureName        of Range.t * signature_name * abstract_module_signature
exception NotASubtype                         of Range.t * concrete_module_signature * concrete_module_signature
exception NotASubtypeTypeOpacity              of Range.t * type_name * type_opacity * type_opacity
exception NotASubtypeVariant                  of Range.t * TypeID.Variant.t * TypeID.Variant.t * constructor_name
exception NotASubtypeSynonym                  of Range.t * TypeID.Synonym.t * TypeID.Synonym.t
exception OpaqueIDExtrudesScopeViaValue       of Range.t * poly_type
exception OpaqueIDExtrudesScopeViaType        of Range.t * type_opacity
exception OpaqueIDExtrudesScopeViaSignature   of Range.t * abstract_module_signature
exception SupportOnlyFirstOrderFunctor        of Range.t
exception InvalidIdentifier                   of Range.t * identifier

val main : untyped_binding list -> NamedSigRecord.t abstracted * binding list
