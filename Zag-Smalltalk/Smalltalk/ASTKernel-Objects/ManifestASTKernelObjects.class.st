"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestASTKernelObjects',
	#superclass : 'PackageManifest',
	#category : 'ASTKernel-Objects-Manifest',
	#package : 'ASTKernel-Objects',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestASTKernelObjects class >> ruleNotEliminationRuleV1FalsePositive [

	^ #( #( #( #RGMethodDefinition #( #ASTObject #~~ false ) )
	        #'2023-06-24T14:15:20.396425-04:00' ) )
]

{ #category : 'code-critics' }
ManifestASTKernelObjects class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASTObject #error: #false)) #'2023-06-24T15:05:14.743216-04:00') )
]
