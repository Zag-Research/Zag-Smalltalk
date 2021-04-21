Class {
	#name : #PCFile,
	#superclass : #PCObject,
	#instVars : [
		'name',
		'fileID'
	],
	#category : #'ASTKernel-Optional'
}

{ #category : #'as yet unclassified' }
PCFile >> close [
	"Close this file."

	fileID
		ifNotNil: [ self primClose: fileID.
			fileID := nil ]
]

{ #category : #'as yet unclassified' }
PCFile >> cr [
	self nextPutAll: (PCString with: PCCharacter cr)
]

{ #category : #'as yet unclassified' }
PCFile >> localFolderPath [
	"Answer the path for the folder containing the image file."

	"MFile new localFolderPath"

	| imagePath delimiter i |
	imagePath := self primImageName.
	delimiter := $/.
	(imagePath includes: delimiter)
		ifFalse: [ delimiter := $\ ].
	i := imagePath size.
	[ i > 0 and: [ (imagePath at: i) ~= delimiter ] ]
		whileTrue: [ i := i - 1 ].
	i = 0
		ifTrue: [ ^ '' ].
	^ imagePath copyFrom: 1 to: i
]

{ #category : #'as yet unclassified' }
PCFile >> name [
	"Answer the name of this file."

	^ name
]

{ #category : #'as yet unclassified' }
PCFile >> next: count [
	"Answer a String containing the next count bytes of the file. If there are not count bytes left in the file, answer a String with as many bytes as available."

	| buffer n |
	buffer := '' class new: count.
	n := self
		primRead: fileID
		into: buffer
		startingAt: 1
		count: count.
	n < count
		ifTrue: [ buffer := buffer copyFrom: 1 to: n ].
	^ buffer
]

{ #category : #'as yet unclassified' }
PCFile >> nextPutAll: buffer [
	"Write the contents of the given bytes or words object to this file."

	^ self
		primWrite: fileID
		from: buffer
		startingAt: 1
		count: buffer basicSize
]

{ #category : #'as yet unclassified' }
PCFile >> openReadOnly: fileName [
	"Open the file with the given name for reading and writing."

	name := nil.
	fileID := self primOpen: fileName writable: false.
	name := fileName
]

{ #category : #'as yet unclassified' }
PCFile >> openReadWrite: fileName [
	"Open the file with the given name for reading only."

	name := nil.
	fileID := self primOpen: fileName writable: true.
	name := fileName
]

{ #category : #'as yet unclassified' }
PCFile >> position [
	"Answer the current file position in bytes."

	^ self primGetPosition: fileID
]

{ #category : #'as yet unclassified' }
PCFile >> position: newPosition [
	"Seek to the given file position in bytes."

	^ self primSetPosition: fileID to: newPosition
]

{ #category : #'as yet unclassified' }
PCFile >> primClose: id [
	"Close this file. Don't raise an error if the primitive fails."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	
]

{ #category : #'as yet unclassified' }
PCFile >> primGetPosition: id [
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCFile >> primImageName [
	"Answer the full path name for the current image."

	<primitive: 121>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCFile >> primOpen: fileName writable: writableFlag [
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCFile >> primRead: id into: byteArray startingAt: startIndex count: count [
	"Read up to count elements into the given buffer and answer the number of elements actually read. The buffer may either a byte- or word-indexable object."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCFile >> primSetPosition: id to: anInteger [
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCFile >> primSize: id [
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCFile >> primWrite: id from: buffer startingAt: startIndex count: count [
	"Write up to count elements from the given buffer and answer the number of elements actually written. The buffer may either a byte- or word-indexable object."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCFile >> readInto: buffer startingAt: startIndex count: count [
	"Read up to count elements into the given array and answer the number of elements actually read. The buffer may either a byte- or word-indexable object."

	^ self
		primRead: fileID
		into: buffer
		startingAt: startIndex
		count: count
]

{ #category : #'as yet unclassified' }
PCFile >> size [
	"Answer the size of this file in bytes."

	^ self primSize: fileID
]
