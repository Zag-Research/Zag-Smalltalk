Extension { #name : 'ASCodeGenerator' }

{ #category : '*Zag-Core-Tests' }
ASCodeGenerator >> compiledUnits [
	
	^ compiledUnits
]

{ #category : '*Zag-Core-Tests' }
ASCodeGenerator >> pushNil [

	^ code pushNil
]

{ #category : '*Zag-Core-Tests' }
ASCodeGenerator class >> targetClass: aClass selector: aSymbol [

	^ self new
		  targetClass: aClass;
		  generateFrom: (ASPharoCompiler new astFor: (ASInliner lookup: aSymbol class: aClass) method)
]
