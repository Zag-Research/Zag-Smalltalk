"
I contain class-size methods that are defined in libc
"
Class {
	#name : 'Zag_Libc',
	#superclass : 'Object',
	#category : 'Zag-Library-LLibraries',
	#package : 'Zag-Library',
	#tag : 'LLibraries'
}

{ #category : 'system-api' }
Zag_Libc class >> getenv: vname [

	<ffi: 'getenv' with: '*C_string' returns: 'C_string'>
	self primitiveFailed
]

{ #category : 'system-api' }
Zag_Libc class >> setenv: vname to: value [

	<ffi: 'setenv' with: '*C_string,*C_string' error: 'ec'>
	self primitiveFailed
]
