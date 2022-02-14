Class {
	#name : #ASMessageStats,
	#superclass : #Object,
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #accessing }
ASMessageStats class >> array1: size [
	^ self gen: size types: { ASMessageT1 }
]

{ #category : #accessing }
ASMessageStats class >> array2: size [
	^ self gen: size types: { ASMessageT1. ASMessageT2 }
]

{ #category : #accessing }
ASMessageStats class >> array3: size [
	^ self gen: size types: { ASMessageT1. ASMessageT2. ASMessageT3 }
]

{ #category : #accessing }
ASMessageStats class >> array4: size [
	^ self gen: size types: { ASMessageT1. ASMessageT2. ASMessageT3. ASMessageT4 }
]

{ #category : #accessing }
ASMessageStats class >> array5: size [
	^ self gen: size types: { ASMessageT1. ASMessageT2. ASMessageT3. ASMessageT4. ASMessageT5 }
]

{ #category : #accessing }
ASMessageStats class >> array6: size [
	^ self gen: size types: { ASMessageT1. ASMessageT2. ASMessageT3. ASMessageT4. ASMessageT5. ASMessageT6 }
]

{ #category : #accessing }
ASMessageStats class >> array7: size [
	^ self gen: size types: { ASMessageT1. ASMessageT2. ASMessageT3. ASMessageT4. ASMessageT5. ASMessageT6. ASMessageT7 }
]

{ #category : #accessing }
ASMessageStats class >> array8: size [
	^ self gen: size types: { ASMessageT1. ASMessageT2. ASMessageT3. ASMessageT4. ASMessageT5. ASMessageT6. ASMessageT7. ASMessageT8 }
]

{ #category : #accessing }
ASMessageStats class >> array9: size [
	^ self gen: size types: { ASMessageT1. ASMessageT2. ASMessageT3. ASMessageT4. ASMessageT5. ASMessageT6. ASMessageT7. ASMessageT8. ASMessageT9 }
]

{ #category : #generation }
ASMessageStats class >> gen: anInteger types: aCollection [ 
	| array |
	array := Array new: anInteger.
	1 to: anInteger do: [: i |
		array at: i put: (aCollection at: (i % aCollection size)+1) new
	].
	^ array
]

{ #category : #'as yet unclassified' }
ASMessageStats class >> runP1 [
	| size |
	" (1 to: 2)  collect: [:x| self runP1] "
	size := 50000000.
	(Array >> #doP1) recompile.
	^ { 
		  #runP1.
		  DateAndTime now.
		  (self array1: size) doP1.
		  (self array2: size) doP1.
		  (self array3: size) doP1.
		  (self array4: size) doP1.
		  (self array5: size) doP1.
		  (self array6: size) doP1.
		  (self array7: size) doP1.
		  (self array8: size) doP1.
		  (self array9: size) doP1 }
]
