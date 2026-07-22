"
I manage the values to go into a zag symbols table. I have predefined symbols and runtime symbols.
"
Class {
	#name : 'ZagSymbols',
	#superclass : 'Object',
	#instVars : [
		'nextSymbol',
		'symbols'
	],
	#classVars : [
		'Predefined'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'instance creation' }
ZagSymbols class >> newWithEncoder: encoder [

	^ self basicNew encodePredefined: encoder
]

{ #category : 'class initialization' }
ZagSymbols class >> symbols [
	"in future better to load sysmbols directly from symbol.zig, may need a config file so we know where symbol.zig is"

	^ #( value #= value: cull: doesNotUnderstand: at: new: valueWithArguments:
	     ifTrue: ifFalse: ifNil: ifNotNil: perform: #+ #- #* #'~=' #'=='
	     #'~~' #< #'<=' #'>=' #> at:put: value:value: cull:cull:
	     ifTrue:ifFalse ifFalse:ifTrue: ifNil:ifNotNil ifNotNil:ifNil:
	     perform:with: perform:withArguments: value:value:value:
	     cull:cull:cull: perform:with:with: perform:withArguments:inSuperclass:
	     value:value:value:value: cull:cull:cull:cull:
	     perform:with:with:with: perform:with:with:with:with:
	     yourself size negated new self name class Class Behavior
	     ClassDescription Metaclass SmallInteger noFallback fibonacci
	     Object )
	"inspect"
]

{ #category : 'accessing' }
ZagSymbols >> at: symbol orGenerateWith: generator [

	^ symbols at: symbol ifAbsentPut: [
		| hash |
		hash := nextSymbol * 10368889 bitAnd: 16rffffff.
		nextSymbol := nextSymbol + 1.
		generator value: hash
	]
	
]

{ #category : 'initialization' }
ZagSymbols >> encodePredefined: encoder [

	symbols := (self class symbols collectWithIndex: [ :key :index |
		               key -> (encoder symbolArity: key numArgs hash: index) ])
		              asDictionary.
	nextSymbol := self class symbols size + 1.

]

{ #category : 'testing' }
ZagSymbols >> predefinedSymbol: aSymbol [

	^ (Predefined includes: aSymbol) ifTrue: [ 'Sym.' , aSymbol zigName ]
]

{ #category : 'testing' }
ZagSymbols >> printable: aSymbol [

	^ (self predefinedSymbol: aSymbol) ifNotNil: [ :s | s ] ifNil: [
		  self add: aSymbol.
		  'sym.' , aSymbol zigName ]
]

{ #category : 'writing' }
ZagSymbols >> writeDefinitionOn: anASLLFileContext [ 

]

{ #category : 'writing' }
ZagSymbols >> writeDefinitionWith: stream [

	| syms n |
	"syms := runtimeDefined." "asSortedCollection"
	stream nextPutAll: 'const Sym = struct {
'.
	syms do: [ :sym |
			stream
				tab;
				nextPutAll: sym zigName;
				nextPutAll: ': Object,
' ].
	"    @""foo:bar:"": Object,"
	stream nextPutAll: '    const ss = heap.compileStrings(.{
'.
	syms do: [ :sym |
			stream
				tab;
				nextPut: $";
				nextPutAll: (sym ifNil: [ '*Nil*' ]);
				nextPutAll: '",
' ].
	"     ""foo:bar:"","
	stream nextPutAll: '    });
    usingnamespace symbol.symbols;
    fn init() Sym {
        return .{
'.
	n := 0.
	syms do: [ :sym |
			stream
				tab;
				nextPut: $.;
				nextPutAll: sym zigName;
				nextPutAll: ' = symbol.intern(ss[';
				nextPutAll: n asString;
				nextPutAll: '].asObject()),
'.
			n := n + 1 ].
	"            .@""foo:bar:"" = symbol.intern(ss[0].asObject()),"
	stream nextPutAll: '        };
    }
};
'
]

{ #category : 'writing' }
ZagSymbols >> writeInitializationOn: anASLLFileContext [ 

]

{ #category : 'writing' }
ZagSymbols >> writeInitializationWith: stream [

	stream nextPutAll: '    sym = Sym.init();
'
]
