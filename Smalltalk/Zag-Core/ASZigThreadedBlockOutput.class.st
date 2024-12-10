Class {
	#name : 'ASZigThreadedBlockOutput',
	#superclass : 'ASZigThreadedOutput',
	#instVars : [
		'methodGenerator',
		'block'
	],
	#category : 'Zag-Core-Output',
	#package : 'Zag-Core',
	#tag : 'Output'
}

{ #category : 'code generation' }
ASZigThreadedBlockOutput >> codeGeneratorClass [

	^ ASCodeGeneratorBlock
]

{ #category : 'compiling' }
ASZigThreadedBlockOutput >> methodGenerator [

	^ methodGenerator
]

{ #category : 'accessing' }
ASZigThreadedBlockOutput >> methodGenerator: anOutput block: anInteger [

	methodGenerator := anOutput.
	block := anInteger
]

{ #category : 'visiting' }
ASZigThreadedBlockOutput >> visitPopLocalData: aASCIndirectVariable [

	currentBasicBlock
		offsetOf: aASCIndirectVariable name
		context: nil
		stack: [ :offset |
			self
				putEmbedded: #popFromContaxt; "or popStack?"
				print: currentBasicBlock offsetOfSelf;
				nextPutAll: ' + ( ';
				print: offset;
				nextPutAll: ' << 12 )';
				comma; 
				lf ]
		other: [ :bcOffset :methodOffset :objectOffset |
			self
				putEmbedded: #popIndiect;
				print: bcOffset;
				"offset of block closure"nextPutAll: ' + ( ';
				print: methodOffset;
				nextPutAll: ' << 12 )';
				nextPutAll: ' + ( ';
				print: objectOffset;
				nextPutAll: ' << 12 )';
				comma;
				lf ]
]

{ #category : 'visiting' }
ASZigThreadedBlockOutput >> visitPushLocalData: aASCIndirectVariable [
	currentBasicBlock
		offsetOf: aASCIndirectVariable name
		context: nil
		stack: [ :offset |
			self
				putEmbedded: #pushFromContext;
				print: offset;
				nextPutAll: ' + ( ';
				print: (currentBasicBlock offsetTo: nil);
				nextPutAll: ' << 12 )';
				comma;
				lf ]
		other: [ :bcOffset :methodOffset :objectOffset |
			self
				putEmbedded: #pushIndiect;
				print: bcOffset;
				"offset of block closure"nextPutAll: ' + ( ';
				print: methodOffset;
				nextPutAll: ' << 12 )';
				nextPutAll: ' + ( ';
				print: objectOffset;
				nextPutAll: ' << 12 )';
				comma;
				lf ]
]

{ #category : 'API' }
ASZigThreadedBlockOutput >> visitPushLocalOrBlock: anASVariable [ 
	"The same logic as our super but for the other case.
	The other case will be triggered when the variable was not found.
	In the case the variable is not found we need to go through the blocks clousre.
	The context will have references to other closures and shared data, 
	see Execution.md for more details."
	currentBasicBlock
		offsetOf: anASVariable name
		context: [ :local |
			self
				putEmbedded: #pushLocal;
				print: local;
				comma;
				lf ]
		stack: [ :offset |
			self
				putEmbedded: #pushStack;
				print: offset;
				comma;
				lf ]
		other: "The variable is either in our context, or in the closure data" 
			[ :offsets |
				self
				putEmbedded: #pushLocalData;
				print: (offsets collectWithIndex: [:offset :index | offset withIndex: index ]);
				comma;
				lf 
			 ]
]

{ #category : 'visiting' }
ASZigThreadedBlockOutput >> visitReturnNonLocalTop [

	self putEmbedded: #returnNonLocal;
	lf
]

{ #category : 'visiting' }
ASZigThreadedBlockOutput >> visitStoreLocal: anASLocal [ 
	^ self subclassResponsibility
]
