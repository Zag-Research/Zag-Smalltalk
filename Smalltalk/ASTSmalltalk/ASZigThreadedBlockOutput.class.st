"
I output BlockClosures
"
Class {
	#name : 'ASZigThreadedBlockOutput',
	#superclass : 'ASZigThreadedOutput',
	#instVars : [
		'methodGenerator',
		'block'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
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
	"&e.popLocalData, i1 + (i2 << s1)"

	"self
		putEmbedded: #popLocalData;
		print:
			(aASCIndirectVariable offsets collectWithIndex: [ :offset :index |
					 offset withIndex: index ]);
		comma;
		lf"
	currentBasicBlock
		offsetOf: aASCIndirectVariable name
		local: nil
		stack: [ :offset |
			self
				putEmbedded: #popStack;
				print: (currentBasicBlock offsetToSelf);
				print: offset;
				comma; 
				lf ]
		other:  [ :offsets |
			self
				putEmbedded: #popLocalData;
				print: (offsets collectWithIndex: [ :offset :index |
					 offset withIndex: index ]);
				comma;
				lf ]
]

{ #category : 'visiting' }
ASZigThreadedBlockOutput >> visitPushLocalData: aASCIndirectVariable [
	"&e.pushLocalData, i1 + (i2 << s1)"

	"self
		putEmbedded: #pushLocalData;
		print:
			(aASCIndirectVariable offsets collectWithIndex: [ :offset :index |
					 offset withIndex: index ]);
		comma;
		lf"
	currentBasicBlock
		offsetOf: aASCIndirectVariable name
		local: nil
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
		local: [ :local |
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
