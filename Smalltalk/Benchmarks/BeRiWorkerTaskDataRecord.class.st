Class {
	#name : 'BeRiWorkerTaskDataRecord',
	#superclass : 'BeRiObject',
	#instVars : [
		'destination',
		'count'
	],
	#category : 'Benchmarks-Richards',
	#package : 'Benchmarks',
	#tag : 'Richards'
}

{ #category : 'instance creation' }
BeRiWorkerTaskDataRecord class >> create [
    ^super new create
]

{ #category : 'accessing' }
BeRiWorkerTaskDataRecord >> count [
    ^count
]

{ #category : 'accessing' }
BeRiWorkerTaskDataRecord >> count: aCount [
    count := aCount
]

{ #category : 'initialize' }
BeRiWorkerTaskDataRecord >> create [
    destination := HandlerA.
    count := 0
]

{ #category : 'accessing' }
BeRiWorkerTaskDataRecord >> destination [
    ^destination
]

{ #category : 'accessing' }
BeRiWorkerTaskDataRecord >> destination: aHandler [
    destination := aHandler
]
