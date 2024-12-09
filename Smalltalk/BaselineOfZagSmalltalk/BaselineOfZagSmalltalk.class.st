"
I am the baseline for ASTSmalltalk aka ZagSmalltalk
"
Class {
	#name : 'BaselineOfZagSmalltalk',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfZagSmalltalk',
	#package : 'BaselineOfZagSmalltalk'
}

{ #category : 'baselines' }
BaselineOfZagSmalltalk >> baseline: spec [

	<baseline>
	spec for: #common do: [ "Packages"
		spec
			package: 'ASTSmalltalk';
			package: 'ASTSmalltalk-Tests'
			with: [ spec requires: #( 'ASTSmalltalk' ) ];
			package: 'ASTSmalltalk-LLVM'
			with: [ spec requires: #( 'ASTSmalltalk' ) ];
			package: 'ASTSmalltalk-LLVM-Tests'
			with: [
				spec requires: #( 'ASTSmalltalk-LLVM' 'ASTSmalltalk-Tests' ) ];
			package: 'Benchmarks' with: [ spec requires: #( 'ASTSmalltalk' ) ];
			package: 'Zag-LLVM';
			package: 'Zag-LLVM-Tests' with: [ spec requires: #( 'Zag-LLVM' ) ];
			package: 'Zag-Exporter';
			package: 'Zag-Exporter-Tests'
			with: [ spec requires: #( 'Zag-Exporter' ) ];
			package: 'Zag-Library' ]
]

{ #category : 'accessing' }
BaselineOfZagSmalltalk >> projectClass [
	^ MetacelloCypressBaselineProject
]
