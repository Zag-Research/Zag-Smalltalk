"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestASTKernelNumeric,
	#superclass : #PackageManifest,
	#category : 'ASTKernel-Numeric-Manifest'
}

{ #category : #'code-critics' }
ManifestASTKernelNumeric class >> ruleLongMethodsRuleV1FalsePositive [

	^ #( #( #( #RGMethodDefinition #( ASTInteger printOn:base:
	              false ) )
	        #'2023-06-24T14:40:15.59323-04:00' ) )
]

{ #category : #'code-critics' }
ManifestASTKernelNumeric class >> ruleRefersToClassRuleV1FalsePositive [

	^ #( #( #( #RGMethodDefinition #( ASTSmallInteger byteAt: false ) )
	        #'2023-06-24T15:07:07.276064-04:00' ) )
]

{ #category : #'code-critics' }
ManifestASTKernelNumeric class >> ruleUtilityMethodsRuleV1FalsePositive [

	^ #( #( #( #RGMethodDefinition #( ASTNumber to:by:do: #alse ) )
	        #'2023-06-24T14:28:28.803778-04:00' )
	     #( #( #RGMethodDefinition #( ASTNumber to:by: false ) )
	        #'2023-06-24T14:32:32.563207-04:00' ) )
]
