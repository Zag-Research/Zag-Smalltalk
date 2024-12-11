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
			package: 'Zag-Core';
			package: 'Zag-Core-Tests' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Exporter' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Exporter-Tests'
			with: [ spec requires: #( 'Zag-Exporter' 'Zag-Core-Tests' ) ];
			package: 'Zag-Inlining' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Inlining-Tests'
			with: [ spec requires: #( 'Zag-Inlining' ) ];
			package: 'Zag-Inspector' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Inspector-Tests'
			with: [ spec requires: #( 'Zag-Inspector' ) ];
			package: 'Zag-LLVM' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-LLVM-Tests' with: [ spec requires: #( 'Zag-LLVM' ) ];
			package: 'Zag-OpalCompiler'
			with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-OpalCompiler-Tests'
			with: [ spec requires: #( 'Zag-OpalCompiler' ) ];
			package: 'Zag-Output' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Output-Tests'
			with: [ spec requires: #( 'Zag-Output' ) ];
			package: 'Zag-TextCompiler'
			with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-TextCompiler-Tests'
			with: [ spec requires: #( 'Zag-TextCompiler' ) ];
			package: 'Benchmarks' with: [ spec requires: #( 'Zag-Core' ) ];
			package: 'Zag-Library' ]
]

{ #category : 'accessing' }
BaselineOfZagSmalltalk >> projectClass [
	^ MetacelloCypressBaselineProject
]
