"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZagBenchmarks',
	#superclass : 'PackageManifest',
	#category : 'Benchmarks-Manifest',
	#package : 'Benchmarks',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZagBenchmarks class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#BeMandelbrot #mandelbrot: #false)) #'2024-05-09T18:36:41.508712-04:00') )
]

{ #category : 'code-critics' }
ManifestZagBenchmarks class >> ruleWhileTrueRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#BeMandelbrot #mandelbrot: #false)) #'2024-05-09T18:36:48.777029-04:00') )
]
