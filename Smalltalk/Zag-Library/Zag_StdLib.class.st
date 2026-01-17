"
I contain class-size methods for stdlib methods that are not marked as in libc
"
Class {
	#name : 'Zag_StdLib',
	#superclass : 'Object',
	#category : 'Zag-Library-LLibraries',
	#package : 'Zag-Library',
	#tag : 'LLibraries'
}

{ #category : 'system-api' }
Zag_StdLib class >> getrusage: who into: struct [

	<ffi: 'getrusage' with: 'C_int,*struct_rusage' error: 'ec'>
	self primitiveFailed
]

{ #category : 'system-api' }
Zag_StdLib class >> gettimeofday: time zone: zone [

	<ffi: 'gettimeofday' with: '*Zag_struct_timeval,*Zag_struct_timezone' error: 'ec'>
	self primitiveFailed
]
