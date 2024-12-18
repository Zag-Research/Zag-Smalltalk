"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZagLLVMTests',
	#superclass : 'PackageManifest',
	#category : 'Zag-LLVM-Tests-Manifest',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZagLLVMTests class >> ruleCodeCruftLeftInMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ZagOutputLLVMTest)) #'2024-12-17T12:17:10.726734-05:00') )
]
