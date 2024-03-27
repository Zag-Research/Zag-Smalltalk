Extension { #name : #ByteArray }

{ #category : #'*ASTSmalltalk-Tests' }
ByteArray >> at64: anInteger [
	| result |
	result := 0.
	8 to: 1 by: -1 do: [: i |
		result := (result << 8)+(self at: anInteger + i)
	].
	^ result
]
