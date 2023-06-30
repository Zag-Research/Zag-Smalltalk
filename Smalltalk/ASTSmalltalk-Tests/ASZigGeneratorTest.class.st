Class {
	#name : #ASZigGeneratorTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigGeneratorTest >> testASAssign [

	| result stackVars |
	stackVars := OrderedCollection new.
	stackVars add: 'x'.

	result := (ASAssign
		           variable: (ASLocal name: 'x')
		           expression: (ASLiteral literal: 3))
		          zigString: Dictionary new
		          and: stackVars.
	self
		assert: result
		equals: '&e.pushLiteral, Object.from(3), &e.popLocal, 1'
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlock [

	| result stackVars body args |
	args := Array new: 1.
	args at: 1 put: (ASSend
			 target: (ASSelf method: #basicAt:put:)
			 selector: #size
			 args: Array new).
	body := OrderedCollection new.
	body add: (ASSend
			 target: (ASRef variable: (ASArg name: #index))
			 selector: #'<='
			 args: args).
	stackVars := OrderedCollection new.
	stackVars add: 'x'.

	result := ((ASBlock arguments: Array new locals: Array new body: body) selector: '@foo:bar:1' )
		          zigString: Dictionary new
		          and: stackVars.
	self assert: result equals: '&e.blockClosure,"@foo:bar:1",1+(3<<16),'.
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlockWithFalse [
	| result body |
	"pushNonlocalBlock_false"
	body := OrderedCollection new.
	body add: (ASReturn expression: (ASLiteral literal: false)).
	result := (ASBlock arguments: Array new locals: Array new body: body)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushNonlocalBlock_false,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlockWithMinusOne [

	| result body |
	"pushNonlocalBlock_minusone"
	body := OrderedCollection new.
	body add: (ASReturn expression: (ASLiteral literal: -1)).
	result := (ASBlock arguments: Array new locals: Array new body: body)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushNonlocalBlock_minusOne,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlockWithNil [

	| result body |
	"pushNonlocalBlock_nil"
	body := OrderedCollection new.
	body add: (ASReturn expression: (ASLiteral literal: nil)).
	result := (ASBlock arguments: Array new locals: Array new body: body)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushNonlocalBlock_nil,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlockWithOne [

	| result body |
	"pushNonlocalBlock_one"
	body := OrderedCollection new.
	body add: (ASReturn expression: (ASLiteral literal: 1)).
	result := (ASBlock arguments: Array new locals: Array new body: body)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushNonlocalBlock_one,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlockWithSelf [

	| result body stackVariables |
	"pushNonlocalBlock_self"
	stackVariables := OrderedCollection new.
	stackVariables add: #self.
	body := OrderedCollection new.
	body add: (ASReturn expression: ASSelf new).
	result := (ASBlock arguments: Array new locals: Array new body: body)
		          zigString: Dictionary new
		          and: stackVariables.
	self assert: result equals: '&e.pushNonlocalBlock_self,1,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlockWithTrue [

	| result body |
	"pushNonlocalBlock_true"
	body := OrderedCollection new.
	body add: (ASReturn expression: (ASLiteral literal: true)).
	result := (ASBlock arguments: Array new locals: Array new body: body)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushNonlocalBlock_true,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlockWithTwo [

	| result body |
	"pushNonlocalBlock_two"
	body := OrderedCollection new.
	body add: (ASReturn expression: (ASLiteral literal: 2)).
	result := (ASBlock arguments: Array new locals: Array new body: body)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushNonlocalBlock_two,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASBlockWithZero [

	| result body |
	"pushNonlocalBlock_zero"
	body := OrderedCollection new.
	body add: (ASReturn expression: (ASLiteral literal: 0)).
	result := (ASBlock arguments: Array new locals: Array new body: body)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushNonlocalBlock_zero,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASLiteral [

	| result |
	result := (ASLiteral literal: 1)
		          zigString: Dictionary  new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushLiteral, Object.from(1),'
]

{ #category : #tests }
ASZigGeneratorTest >> testASMethod [

	| result selector arguments locals string bytes stream value body |
	string := '
var size =
	compileMethod(sym.size,0,0+0,.{
	&e.verifySelector,
	&e.pushContext,"^",
	&e.returnTop,
});
'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterWriteStream on: ByteArray new writeStream.
	"stream nextPutAll: string."

	selector := #size.

	arguments := OrderedCollection new.
	locals := OrderedCollection new.
	body := OrderedCollection new.
	result := (ASMethodNode
		           class: self
		           selector: selector
		           arguments: arguments
		           locals: locals
		           body: body)
		          writeMethod: stream
		          with: Dictionary new.
	value := stream wrappedStream contents asString.
	self assert: stream wrappedStream contents equals: bytes
]

{ #category : #tests }
ASZigGeneratorTest >> testASMethodNoBody [

	| result selector arguments locals string bytes stream value |
	string := '
var size =
	compileMethod(sym.size,0,0+0,.{
	&e.verifySelector,
	&e.pushContext,"^",
	&e.returnTop,
});
'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterWriteStream on: ByteArray new writeStream.
	"stream nextPutAll: string."

	selector := #size.

	arguments := OrderedCollection new.
	locals := OrderedCollection new.
	result := (ASMethodNode
		           class: self
		           selector: selector
		           arguments: arguments
		           locals: locals
		           body: OrderedCollection new)
		          writeMethod: stream
		          with: Dictionary new.
	value := stream wrappedStream contents asString.
	self assert: stream wrappedStream contents equals: bytes
]

{ #category : #tests }
ASZigGeneratorTest >> testASMethodTrivialReturn [

	| result selector arguments locals string bytes stream value body |
	string := '
var size =
	compileMethod(sym.size,0,0+0,.{
	&e.verifySelector,
	&e.pushContext,"^",
	&e.pushLiteral, Object.from(5),
	&e.returnTop,
});
'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterWriteStream on: ByteArray new writeStream.
	"stream nextPutAll: string."

	selector := #size.

	arguments := OrderedCollection new.
	locals := OrderedCollection new.
	body := OrderedCollection new.
	body add: (ASReturn expression: (ASLiteral literal: 5)).
	result := (ASMethodNode
		           class: self
		           selector: selector
		           arguments: arguments
		           locals: locals
		           body: body) writeMethod: stream with: Dictionary new.
	value := stream wrappedStream contents asString.
	self assert: stream wrappedStream contents equals: bytes
]

{ #category : #tests }
ASZigGeneratorTest >> testASRef [

	| result stackVars |
	stackVars := OrderedCollection new.
	stackVars add: 'p'.

	result := (ASRef variable: (ASArg name: 'p'))
		          zigString: Dictionary new
		          and: stackVars.
	self assert: result equals: '&e.pushLocal, 1,'.

"what happens when the stackVars doesn't have the name? exception?"
	result := (ASRef variable: (ASArg name: 'x'))
		          zigString: Dictionary new
		          and: stackVars.
	self assert: result equals: '&e.pushLocal, 0,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASReturn [

	| result stackVars |
	stackVars := OrderedCollection new.
	stackVars add: 'x'.

	result := (ASReturn
		           expression: (ASLiteral literal: 3))
		          zigString: Dictionary new
		          and: stackVars.
	self
		assert: result
		equals: '&e.pushLiteral, Object.from(3),'
]

{ #category : #tests }
ASZigGeneratorTest >> testASSelf [

	| result stackVars |
	stackVars := OrderedCollection new.
	stackVars add: 'p'.

	result := (ASSelf
		           method: #methodName)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self
		assert: result
		equals: '&e.pushSelf,'
]

{ #category : #tests }
ASZigGeneratorTest >> testASSend [

	| result stackVars |
	stackVars := OrderedCollection new.
	stackVars add: 'p'.

	result := (ASSend
		           target: (ASLiteral literal: 'p')
		           selector: #methodName
		           args: Dictionary new)
		          zigString: Dictionary new
		          and: OrderedCollection new.
	self assert: result equals: '&e.pushLiteral, Object.from(p),&e.send, methodName,'
]
