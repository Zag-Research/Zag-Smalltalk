"
I clean up the stack and possibly replace the self value with TOS.

I will precede a branch-return or a branch for an inlined tail send
"
Class {
	#name : 'ASCRestructureStack',
	#superclass : 'ASCStackReferencingOperations',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#instVars : [
		'restructure'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}
