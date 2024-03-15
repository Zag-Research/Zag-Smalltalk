Class {
	#name : 'ASExploreHashing',
	#superclass : 'TestCase',
	#instVars : [
		'universe',
		'primes'
	],
	#classVars : [
		'Something'
	],
	#category : 'ASTSmalltalk-Tests-Explore',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Explore'
}

{ #category : 'exploring' }
ASExploreHashing >> crtTable: n [
" build perfect hash from prime number keys: Chang 1984 - https://doi.org/10.1145/358027.358051 "
" unfortunately, if the universe of keys is more than about the first 15 primes, and the table includes more than a very few keys, you quickly move to BigInt calculations "
" another e.g. if the keys are the first 1000 primes and more than 5 keys, it's BigInt "
" ASExploreHashing new crtTable: 6 "
	| m M b C p |
	m := self keys: n from: primes.
	M := Array new: n withAll: 1.
	p := 1.
	m withIndexDo: [: mi : j | p:=p*mi. 1 to: n do: [: i | i~=j ifTrue: [ M at: i put: (M at: i)*mi ]]].
	b := m with: M collect: [: mi : Mi | | DEND DSR Q RMD Bp B |
			DSR := mi.
			RMD := Mi\\mi.
			Q := OrderedCollection new.
			[
				DEND := DSR.
				DSR := RMD.
				Q add: DEND//DSR.
				RMD := DEND - (Q last * DSR).
				RMD > 1 ] whileTrue.
			Bp := 1.
			B := Q last negated.
			1 to: Q size - 1 do: [: j | | t |
				t := B*(Q at: Q size - j) negated + Bp.
				Bp := B.
				B := t
			].
			B ].
	C := (1 to: n) inject: 0 into: [: sum : i | sum+((b at: i)*(M at: i)*i)].
	C := C\\p.
	^ C "->(m collect: [: mi | C\\mi ])"
]

{ #category : 'exploring' }
ASExploreHashing >> directPower2Table: n [
	" Returns an array of n, followed by the size table to have no collisions,
      the size for worst-case 2 collisions, 3, etc. "
	" #(4 8 16 32 33 34 35 36 37 38 39 64 81 100 121 144 169 256) collect: [:e| ASExploreHashing new directPower2Table: e ] "
	| table size keys overflow stored extra |
	extra := 3.
	keys := self keys: n from: universe.
	overflow := OrderedCollection new.
	size := (n*5//3) asLargerPowerOfTwo.
	table := Array new: size+extra.
	keys do: [: r |
		(table at: r\\size+1) ifNil: [
			table at: r\\size+1 put: r.
	]].
	keys do: [: r |
		stored := false.
		r\\size+1 to: r\\size+extra+1 do: [: index |
			stored ifFalse: [
				(table at: index) = r ifTrue: [ stored := true ] ifFalse: [
					(table at: index) ifNil: [
						table at: index put: r.
						stored := true
		]]]].
		stored ifFalse: [ overflow add: r ]
	].
	^ n->size->overflow size
]

{ #category : 'initialization' }
ASExploreHashing >> initialize [
	super initialize.
	" 62816 distinct method names of 90813 different symbols "
	primes := Integer primesUpTo: 800000.
	universe := 1 to: 90813.

]

{ #category : 'exploring' }
ASExploreHashing >> keys: n from: aCollection [
	| keys |
	keys := Set new.
	[ keys add: aCollection atRandom. keys size < n] whileTrue.
	^ keys asArray.

]

{ #category : 'exploring' }
ASExploreHashing >> min2Table: n [
" (2 to: 35) collect: [:e| ASExploreHashing new min2Table: e] "
	| table filled size index keys size2 |
	keys := self keys: n from: universe.
	size := n.
	[
		size := size + (size//3).
		filled := 0.
		table := Array new: size.
		size2 := size//2.
		keys do: [: r |
			index := r\\size2*2+1.
			(table at: index) ifNil: [
				filled := filled+1 .
				table at: index put: r
			] ifNotNil: [
				(table at: index+1) ifNil: [
					filled := filled+1 .
					table at: index+1 put: r
			]].
		].
		filled=n
	] whileFalse.
	^ n->(size/n) asFloat
]

{ #category : 'exploring' }
ASExploreHashing >> min4Table: n [
" (4 to: 35) collect: [:e| ASExploreHashing new min4Table: e] "
	| table filled size index keys size4 |
	keys := self keys: n from: universe.
	size := n.
	[
		size := size + (size//3).
		filled := 0.
		table := Array new: size.
		size4 := size//4.
		keys do: [: r |
			index := r\\size4*4+1.
			(table at: index) ifNil: [
				filled := filled+1 .
				table at: index put: r
			] ifNotNil: [
				(table at: index+1) ifNil: [
					filled := filled+1 .
					table at: index+1 put: r
			] ifNotNil: [
				(table at: index+2) ifNil: [
					filled := filled+1 .
					table at: index+2 put: r
			] ifNotNil: [
				(table at: index+3) ifNil: [
					filled := filled+1 .
					table at: index+3 put: r
			]]]].
		].
		filled=n
	] whileFalse.
	^ n->(size/n) asFloat
]

{ #category : 'exploring' }
ASExploreHashing >> minTable: n [
" #(4 8 16 32 64 128 256) collect: [:e| ASExploreHashing new minTable: e] "
	| table filled size index keys |
	keys := self keys: n from: universe.
	size := n - 1.
	[
		size := size + 1.
		filled := 0.
		table := Array new: size.
		keys do: [: r |
			index := r\\size+1.
			(table at: index) ifNil: [ filled := filled+1 ].
			table at: index put: r.
		].
		filled=n
	] whileFalse.
	^ n->(size/n) asFloat
]

{ #category : 'exploring' }
ASExploreHashing >> twoLevelPower2Counts: n [
	" Returns an array of n, followed by the size table to have no collisions,
      the size for worst-case 2 collisions, 3, etc. "
	" #(4 8 16 32 33 34 35 36 37 38 39 64 81 100 121 144 169 256) collect: [:e| ASExploreHashing new twoLevelCounts: e ] "
	| table size index keys count result bag |
	keys := self keys: n from: universe.
	result := OrderedCollection with: n.
	size := (n*5/3) asLargerPowerOfTwo.
	table := Array new: size withAll: 0.
	keys do: [: r |
		index := r\\size+1.
		count := table at: index.
		table at: index put: count+1
	].
	bag := Bag new.
	(table reject: #isZero) do: [: c | bag add: c].
	^ n->size->bag valuesAndCounts
]

{ #category : 'exploring' }
ASExploreHashing >> twoLevelPower2Table: n [
	" Returns an array of n, followed by the size table to have no collisions,
      the size for worst-case 2 collisions, 3, etc. "
" #(4 8 16 32 33 34 35 36 37 38 39 64 81 100 121 144 169 256) collect: [:e| ASExploreHashing new twoLevelPower2Table: e ] "
	^ self twoLevelTable: n base: n asLargerPowerOfTwo scale: 2
]

{ #category : 'exploring' }
ASExploreHashing >> twoLevelTable: n [
	" Returns an array of n, followed by the size table to have no collisions,
      the size for worst-case 2 collisions, 3, etc. "
" #(4 8 16 32 33 34 35 36 37 38 39 64 81 100 121 144 169 256) collect: [:e| ASExploreHashing new twoLevelTable: e ] "
	^ self twoLevelTable: n base: n scale: 4/3
]

{ #category : 'exploring' }
ASExploreHashing >> twoLevelTable: n base: base scale: scale [
	" Returns an array of n, followed by the size table to have no collisions,
      the size for worst-case 2 collisions, 3, etc. "
" #(4 8 16 32 33 34 35 36 37 38 39 64 81 100 121 144 169 256) collect: [:e| ASExploreHashing new twoLevelTable: e ] "
	| table size index keys count result max |
	keys := self keys: n from: universe.
	result := OrderedCollection with: n.
	size := base.
	[
		table := Array new: size withAll: 0.
		keys do: [: r |
			index := r\\size+1.
			count := table at: index.
			table at: index put: count+1
		].
		max := table max + 1.
		max-result size timesRepeat: [ result add: 0 ].
		(result at: max) = 0 ifTrue: [ result at: max put: size].
		max > 2
	] whileTrue: [ size := (size*scale) asInteger ].
	^ result asArray
]
