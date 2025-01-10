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
ManifestZagLLVMTests class >> ruleBadMessageRule2V1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ZagOutputLLVMForTest #iterate #false)) #'2025-01-09T19:06:50.440792-05:00') )
]

{ #category : 'code-critics' }
ManifestZagLLVMTests class >> ruleCodeCruftLeftInMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ZagOutputLLVMTest)) #'2024-12-17T12:17:10.726734-05:00') )
]

{ #category : 'code-critics' }
ManifestZagLLVMTests class >> rulePrecedenceRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #( #( #RGClassDefinition #( #ZagOutputLLVMForTest ) )
	        #'2025-01-09T19:10:32.346021-05:00' ) )
]
