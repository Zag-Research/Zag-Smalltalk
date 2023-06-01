Class {
	#name : #PjRbIdleTaskDataRecord,
	#superclass : #PjRbObject,
	#instVars : [
		'control',
		'count'
	],
	#category : #'Zag-Benchmarking-Richards'
}

{ #category : #'instance creation' }
PjRbIdleTaskDataRecord class >> create [
    ^super new create
]

{ #category : #accessing }
PjRbIdleTaskDataRecord >> control [
    ^control
]

{ #category : #accessing }
PjRbIdleTaskDataRecord >> control: aNumber [
    control := aNumber
]

{ #category : #accessing }
PjRbIdleTaskDataRecord >> count [
    ^count
]

{ #category : #accessing }
PjRbIdleTaskDataRecord >> count: aCount [
    count := aCount
]

{ #category : #initialization }
PjRbIdleTaskDataRecord >> create [
    control := 1.
    count := 10000
]
