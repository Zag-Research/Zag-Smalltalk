Extension { #name : 'ByteArray' }

{ #category : '*Zag-Core' }
ByteArray >> asU64Array [

	^ ((self groupsOf: 8) collect: [ :group |
		   group reversed
			   inject: 0
			   into: [ :t :each | (t bitShift: 8) + each ] ]) asArray
]

{ #category : '*Zag-Core' }
ByteArray >> at64: anInteger [
	| result |
	result := 0.
	8 to: 1 by: -1 do: [: i |
		result := (result << 8)+(self at: anInteger + i)
	].
	^ result
]
