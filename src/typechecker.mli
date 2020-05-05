
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
exception CannotRestrictTransparentType       of Range.t * single_type_binding

val main : untyped_binding list -> SigRecord.t abstracted * binding list
