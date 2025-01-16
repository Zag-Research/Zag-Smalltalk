"
I am the baseline for ZagSmalltalk
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
			package: 'Zag-Substitutions';
			package: 'Zag-Core' with: [ spec requires: #( 'Zag-Substitutions' ) ];
			package: 'Zag-Core-Tests' with: [
				spec requires:
						#( 'Zag-Core' 'Zag-Compiler-Tests' 'Zag-Inlining' 'Zag-Output' ) ];
			package: 'Zag-Compiler' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Compiler-Tests'
			with: [ spec requires: #( 'Zag-Compiler' ) ];
			package: 'Zag-Exporter' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Exporter-Tests' with: [
				spec requires:
						#( 'Zag-Exporter' 'Zag-Core-Tests' 'Zag-Compiler-Tests'
						   'Zag-Output' ) ];
			package: 'Zag-Inlining'
			with: [ spec requires: #( 'Zag-Core' 'Zag-Compiler' ) ];
			package: 'Zag-Inlining-Tests' with: [
				spec requires:
						#( 'Zag-Inlining' 'Zag-Compiler-Tests' 'Zag-Core-Tests' ) ];
			package: 'Zag-Inspector'
			with: [ spec requires: #( 'Zag-Core' 'Zag-Compiler' 'Zag-Output' ) ];
			package: 'Zag-Inspector-Tests' with: [
				spec requires:
						#( 'Zag-Inspector' 'Zag-Compiler-Tests' 'Zag-Core' 'Zag-Output-Tests' ) ];
			package: 'Zag-LLVM'
			with: [ spec requires: #( 'Zag-Core' 'Zag-Output' ) ];
			package: 'Zag-LLVM-Tests' with: [
				spec requires:
						#( 'Zag-LLVM' 'Zag-Core-Tests' 'Zag-Compiler-Tests'
						   'Zag-Output-Tests' 'Zag-Inlining-Tests' ) ];
			package: 'Zag-Output'
			with: [ spec requires: #( 'Zag-Core' 'Zag-Exporter' ) ];
			package: 'Zag-Output-Tests' with: [
				spec requires: #( 'Zag-Output' 'Zag-Compiler-Tests'
					   'Zag-Core' ) ];
			package: 'Benchmarks' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Library' ]
]

{ #category : 'accessing' }
BaselineOfZagSmalltalk >> projectClass [
	^ MetacelloCypressBaselineProject
]
