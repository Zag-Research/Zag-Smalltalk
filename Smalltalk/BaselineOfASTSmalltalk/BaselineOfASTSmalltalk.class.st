"
I am the baseline for ASTSmalltalk aka ZagSmalltalk
"
Class {
	#name : 'BaselineOfASTSmalltalk',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfASTSmalltalk-Baseline',
	#package : 'BaselineOfASTSmalltalk',
	#tag : 'Baseline'
}

{ #category : 'baselines' }
BaselineOfASTSmalltalk >> baseline: spec [

	<baseline>
	spec
		for: #common
		do: [ "Packages"
			spec
				package: 'ASTKernel-Objects';
				package: 'ASTKernel-Methods' with: [ spec requires: #('ASTKernel-Objects') ];
				package: 'ASTKernel-Classes' with: [ spec requires: #('ASTKernel-Collections-Ordered') ];
				package: 'ASTKernel-Collections-Abstract' with: [ spec requires: #('ASTKernel-Objects' ) ];
				package: 'ASTKernel-Collections-Ordered' with: [ spec requires: #('ASTKernel-Numeric' 'ASTKernel-Collections-Abstract') ];
				package: 'ASTKernel-Collections-Unordered' with: [ spec requires: #('ASTKernel-Numeric' 'ASTKernel-Collections-Abstract') ];
				package: 'ASTKernel-Numeric' with: [ spec requires: #('ASTKernel-Objects') ];
				package: 'ASTKernel-Optional' with: [ spec requires: #('ASTKernel-Numeric' 'ASTKernel-Collections-Ordered') ];
				package: 'ASTKernel-Optional-Graphics' with: [ spec requires: #('ASTKernel-Optional') ];
				package: 'ASTKernel-Processes' with: [ spec requires: #('ASTKernel-Collections-Ordered') ];
				package: 'ASTKernel-Streams' with: [ spec requires: #('ASTKernel-Objects') ];
				package: 'ASTKernel-System' with: [ spec requires: #('ASTKernel-Objects') ];
				package: 'ASTSmalltalk' with: [ spec requires: #('ASTKernel-Optional') ];
				package: 'ASTSmalltalk-Tests' with: [ spec requires: #('ASTSmalltalk') ];
				package: 'Benchmarks' with: [ spec requires: #('ASTSmalltalk') ]
				]
]

{ #category : 'accessing' }
BaselineOfASTSmalltalk >> projectClass [
	^ MetacelloCypressBaselineProject
]
