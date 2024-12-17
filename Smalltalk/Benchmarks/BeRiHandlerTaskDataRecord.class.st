Class {
	#name : 'BeRiHandlerTaskDataRecord',
	#superclass : 'BeRiObject',
	#instVars : [
		'workIn',
		'deviceIn'
	],
	#category : 'Benchmarks-Richards',
	#package : 'Benchmarks',
	#tag : 'Richards'
}

{ #category : 'instance creation' }
BeRiHandlerTaskDataRecord class >> create [
    ^super new create
]

{ #category : 'initialize' }
BeRiHandlerTaskDataRecord >> create [
    workIn := deviceIn := nil
]

{ #category : 'accessing' }
BeRiHandlerTaskDataRecord >> deviceIn [
    ^deviceIn
]

{ #category : 'accessing' }
BeRiHandlerTaskDataRecord >> deviceIn: aPacket [
    deviceIn := aPacket
]

{ #category : 'accessing' }
BeRiHandlerTaskDataRecord >> deviceInAdd: packet [
    deviceIn := self append: packet head: deviceIn
]

{ #category : 'accessing' }
BeRiHandlerTaskDataRecord >> workIn [
    ^workIn
]

{ #category : 'accessing' }
BeRiHandlerTaskDataRecord >> workIn: aWorkQueue [ 
    workIn := aWorkQueue
]

{ #category : 'accessing' }
BeRiHandlerTaskDataRecord >> workInAdd: packet [
    workIn := self append: packet head: workIn
]
