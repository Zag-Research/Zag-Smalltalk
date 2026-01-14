"
I represent the stdlib structure

My slots should be extended to be `#z_minuteswest->C_int . #tz_dsttime->C_int
"
Class {
	#name : 'Zag_struct_timezone',
	#superclass : 'Zag_CStruct',
	#instVars : [
		'tz_minuteswest',
		'tz_dsttime'
	],
	#category : 'Zag-Library-Structures',
	#package : 'Zag-Library',
	#tag : 'Structures'
}

{ #category : 'slot types' }
Zag_struct_timezone class >> tz_dsttime [
	^ C_int
]

{ #category : 'slot types' }
Zag_struct_timezone class >> tz_minuteswest [

	^ C_int
]
