
open Syntax

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
exception NotOfStructureType                  of Range.t * module_signature
exception NotOfFunctorType                    of Range.t * module_signature
exception NotAFunctorSignature                of Range.t * module_signature
exception NotAStructureSignature              of Range.t * module_signature
exception UnboundSignatureName                of Range.t * signature_name
exception CannotRestrictTransparentType       of Range.t * type_opacity
exception PolymorphicContradiction            of Range.t * identifier * poly_type * poly_type
exception PolymorphicInclusion                of Range.t * FreeID.t * poly_type * poly_type
exception MissingRequiredValName              of Range.t * identifier * poly_type
exception MissingRequiredTypeName             of Range.t * type_name * type_opacity
exception MissingRequiredModuleName           of Range.t * module_name * module_signature
exception MissingRequiredSignatureName        of Range.t * signature_name * module_signature abstracted
exception MissingRequiredConstructor          of Range.t * constructor_name * constructor_entry
exception NotASubtype                         of Range.t * module_signature * module_signature
exception NotASubtypeTypeOpacity              of Range.t * type_name * type_opacity * type_opacity
exception NotASubtypeVariant                  of Range.t * TypeID.Variant.t * TypeID.Variant.t
exception MismatchedNumberOfConstructorParameters of Range.t * constructor_name * constructor_entry * constructor_entry

val main : untyped_binding list -> SigRecord.t abstracted * binding list
