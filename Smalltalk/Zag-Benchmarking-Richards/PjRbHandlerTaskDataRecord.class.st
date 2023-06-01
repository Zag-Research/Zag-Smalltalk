Class {
	#name : #PjRbHandlerTaskDataRecord,
	#superclass : #PjRbObject,
	#instVars : [
		'workIn',
		'deviceIn'
	],
	#category : #'Zag-Benchmarking-Richards'
}

{ #category : #'instance creation' }
PjRbHandlerTaskDataRecord class >> create [
    ^super new create
]

{ #category : #initialization }
PjRbHandlerTaskDataRecord >> create [
    workIn := deviceIn := nil
]

{ #category : #accessing }
PjRbHandlerTaskDataRecord >> deviceIn [
    ^deviceIn
]

{ #category : #accessing }
PjRbHandlerTaskDataRecord >> deviceIn: aPacket [
    deviceIn := aPacket
]

{ #category : #accessing }
PjRbHandlerTaskDataRecord >> deviceInAdd: packet [
    deviceIn := self append: packet head: deviceIn
]

{ #category : #accessing }
PjRbHandlerTaskDataRecord >> workIn [
    ^workIn
]

{ #category : #accessing }
PjRbHandlerTaskDataRecord >> workIn: aWorkQueue [ 
    workIn := aWorkQueue
]

{ #category : #accessing }
PjRbHandlerTaskDataRecord >> workInAdd: packet [
    workIn := self append: packet head: workIn
]
