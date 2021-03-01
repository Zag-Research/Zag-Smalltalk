Class {
	#name : #ASExploreHashing,
	#superclass : #TestCase,
	#instVars : [
		'universe',
		'primes'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #exploring }
ASExploreHashing >> crtTable: n [
" build perfect hash from prime number keys: Chang 1984 - https://doi.org/10.1145/358027.358051 "
" ASExploreHashing new crtTable: 42 "
	| keys m M b C p |
	keys := Set new.
	[ keys add: primes atRandom. keys size < n] whileTrue.
	m := keys asSortedCollection asArray.
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
				RMD ~= 1 ] whileTrue.
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
	^ C->(m collect: [: mi | C\\mi ])
]

{ #category : #initialization }
ASExploreHashing >> initialize [
	super initialize.
	" 62816 distinct method names of 90813 different symbols "
	primes := Integer primesUpTo: 800000.
	universe := 1999 to: 65000.

]

{ #category : #exploring }
ASExploreHashing >> minTable: n [
" ASExploreHashing new minTable: 64 "
	| table filled size index keys |
	keys := Set new.
	[ keys add: universe atRandom. keys size < n] whileTrue.
	keys := keys asArray.
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
	^ n->size
]

{ #category : #exploring }
ASExploreHashing >> tryTable: n [
" ASExploreHashing new tryTable: 32 "
	| table r |
	table := Array new: n.
	1 to: n do: [: i |
		r := universe atRandom.
		(table at: r\\n) ifNotNil: [ ^ r->table ].
		table at: r\\n put: r.
	].
	^ 1
]
