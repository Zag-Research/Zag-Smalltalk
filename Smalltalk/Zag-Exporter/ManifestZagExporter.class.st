"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZagExporter',
	#superclass : 'PackageManifest',
	#category : 'Zag-Exporter-Manifest',
	#package : 'Zag-Exporter',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleBadMessageRule2V1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASAllocationInfo #put:into: #false)) #'2024-12-05T13:54:10.100834-05:00') #(#(#RGMethodDefinition #(#ASAllocationInfo #put:into:indexBlock: #false)) #'2024-12-22T09:16:23.685897-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleBooleanPrecedenceRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagImageEncodeZag class' #double:for: #true)) #'2024-12-24T09:07:17.412955-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleExcessiveArgumentsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #(#(#RGMethodDefinition #(#'ASAllocationInfo class' #headerIVars:indexed:element:weak: #true)) #'2024-12-08T22:22:53.410073-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleInconsistentMethodClassificationRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagImageEncoder class' #comment #true)) #'2024-12-08T22:15:45.265696-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ASAllocationInfo class' #headerIVars:indexed:element:weak: #true)) #'2024-12-08T22:22:57.079135-05:00') #(#(#RGMethodDefinition #(#'ZagConstants class' #initialize #true)) #'2024-12-11T07:17:16.189773-05:00') #(#(#RGMethodDefinition #(#'ZagConstants class' #initializeThreadedConstants #true)) #'2024-12-14T11:46:40.896487-05:00') #(#(#RGMethodDefinition #(#'ZagConstants class' #generateInitializeThreadedConstants #true)) #'2025-01-10T13:21:02.602135-05:00') #(#(#RGMethodDefinition #(#'ZagConstants class' #loadThreadedConstants: #true)) #'2025-01-25T09:26:57.975429-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleMinMaxRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagConstants class' #primitive: #true)) #'2024-12-14T11:47:36.704175-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> rulePrecedenceRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASImageExporter #objectSymbol: #false)) #'2024-12-18T22:13:44.901327-05:00') #(#(#RGMethodDefinition #(#ASStorage #header:size: #false)) #'2024-12-24T10:03:43.095798-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleReCollectionAtCollectionSizeRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagImageEncodeZag class' #double:for: #true)) #'2024-12-24T09:07:29.02622-05:00') #(#(#RGMethodDefinition #(#'ZagConstants class' #loadThreadedConstants: #true)) #'2025-01-25T09:27:06.594071-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleReCyclomaticComplexityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagImageEncoder class' #handleComments: #true)) #'2024-12-07T22:25:26.2562-05:00') #(#(#RGMethodDefinition #(#'ASAllocationInfo class' #headerIVars:indexed:element:weak: #true)) #'2024-12-08T22:23:04.238647-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleStringConcatenationRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagConstants class' #generateInitializeThreadedConstants #true)) #'2025-01-10T12:32:07.826947-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleUsesTrueRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASImageExporter #initialize #false)) #'2024-12-14T12:15:02.427121-05:00') #(#(#RGMethodDefinition #(#ASImageExporter #initClasses #false)) #'2024-12-20T08:34:15.162734-05:00') )
]
