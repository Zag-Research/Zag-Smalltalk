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
	^ #(#(#(#RGMethodDefinition #(#ASAllocationInfo #put:into: #false)) #'2024-12-05T13:54:10.100834-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleExcessiveArgumentsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASStorage #register:ivBlock:arrayBlock:ivSize:arraySize:class: #false)) #'2024-12-05T13:16:07.550458-05:00') #(#(#RGMethodDefinition #(#'ASAllocationInfo class' #headerIVars:indexed:element:weak: #true)) #'2024-12-08T22:22:53.410073-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleInconsistentMethodClassificationRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagImageEncoder class' #comment #true)) #'2024-12-08T22:15:45.265696-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ASAllocationInfo class' #headerIVars:indexed:element:weak: #true)) #'2024-12-08T22:22:57.079135-05:00') #(#(#RGMethodDefinition #(#'ZagConstants class' #initialize #true)) #'2024-12-11T07:17:16.189773-05:00') #(#(#RGMethodDefinition #(#'ZagConstants class' #initializeThreadedConstants #true)) #'2024-12-14T11:46:40.896487-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleMinMaxRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagConstants class' #primitive: #true)) #'2024-12-14T11:47:36.704175-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> rulePrecedenceRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASStorage #register:ivBlock:arrayBlock:ivSize:arraySize:class: #false)) #'2024-12-05T13:15:57.925386-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleReCyclomaticComplexityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagImageEncoder class' #handleComments: #true)) #'2024-12-07T22:25:26.2562-05:00') #(#(#RGMethodDefinition #(#'ASAllocationInfo class' #headerIVars:indexed:element:weak: #true)) #'2024-12-08T22:23:04.238647-05:00') )
]
