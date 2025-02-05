"
I am used in inlining because the return should simply branch to the normal return point, but the stack operations are the same as the corresponding return.
"
Class {
	#name : 'ASCBranchReturn',
	#superclass : 'ASCBranch',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}
