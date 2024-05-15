Class {
	#name : 'BeRiIdleTaskDataRecord',
	#superclass : 'BeRiObject',
	#instVars : [
		'control',
		'count'
	],
	#category : 'Benchmarks-Richards',
	#package : 'Benchmarks',
	#tag : 'Richards'
}

{ #category : 'instance creation' }
BeRiIdleTaskDataRecord class >> create [
    ^super new create
]

{ #category : 'accessing' }
BeRiIdleTaskDataRecord >> control [
    ^control
]

{ #category : 'accessing' }
BeRiIdleTaskDataRecord >> control: aNumber [
    control := aNumber
]

{ #category : 'accessing' }
BeRiIdleTaskDataRecord >> count [
    ^count
]

{ #category : 'accessing' }
BeRiIdleTaskDataRecord >> count: aCount [
    count := aCount
]

{ #category : 'initialize' }
BeRiIdleTaskDataRecord >> create [
    control := 1.
    count := 10000
]
