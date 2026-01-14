"
I represent the stdlib structure
"
Class {
	#name : 'Zag_struct_timeval',
	#superclass : 'Zag_CStruct',
	#instVars : [
		'tv_sec',
		'tv_use'
	],
	#category : 'Zag-Library-Structures',
	#package : 'Zag-Library',
	#tag : 'Structures'
}

{ #category : 'slot types' }
Zag_struct_timeval class >> tm_sec [
	^ C_long
]

{ #category : 'slot types' }
Zag_struct_timeval class >> tm_usec [
	^ C_int
]
