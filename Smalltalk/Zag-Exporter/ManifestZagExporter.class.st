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
	^ #(#(#(#RGMethodDefinition #(#ASStorage #register:ivBlock:arrayBlock:ivSize:arraySize:class: #false)) #'2024-12-05T13:16:07.550458-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> rulePrecedenceRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASStorage #register:ivBlock:arrayBlock:ivSize:arraySize:class: #false)) #'2024-12-05T13:15:57.925386-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporter class >> ruleSendsDifferentSuperRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ZagImageEncodeZag class' #saveConstants #true)) #'2024-12-06T00:25:02.133734-05:00') #(#(#RGMethodDefinition #(#'ZagImageEncodeNan class' #saveConstants #true)) #'2024-12-06T00:25:12.848567-05:00') )
]
