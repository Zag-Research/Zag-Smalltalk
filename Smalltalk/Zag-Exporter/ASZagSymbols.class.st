"
I manage the values to go into a zag symbols table. I have predefined symbols and runtime symbols.
"
Class {
	#name : 'ASZagSymbols',
	#superclass : 'Object',
	#instVars : [
		'runtimeDefined'
	],
	#classVars : [
		'Predefined'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'class initialization' }
ASZagSymbols class >> initialize [
	"in future better to load sysmbols directly from symbol.zig, may need a config file so we know where symbol.zig is"

	Predefined := #( #= value value: cull: yourself doesNotUnderstand: #+ #-#*
size at: at:put: #~= #== #~~ value:value: negated new new: value:value:value: 
value:value:value:value: valueWithArguments: cull:cull: cull:cull:cull: cull:cull:cull:cull:
self name #< #<= #>= #> class Class Behavior ClassDescription Metaclass
SmallInteger noFallback ifTrue: ifTrue:ifFalse ifFalse: ifFalse:ifTrue: ifNil: 
ifNil:ifNotNil ifNotNil: ifNotNil:ifNil: perform: perform:with: perform:with:with: 
perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: fibonacci ) asSet
]

{ #category : 'adding' }
ASZagSymbols >> add: aSymbol [

	runtimeDefined add: aSymbol
]

{ #category : 'initialization' }
ASZagSymbols >> initialize [ 
	super initialize .
	runtimeDefined := Set new
]

{ #category : 'testing' }
ASZagSymbols >> predefinedSymbol: aSymbol [

	^ (Predefined includes: aSymbol) ifTrue: [ 'Sym.' , aSymbol zigName ]
]

{ #category : 'testing' }
ASZagSymbols >> printable: aSymbol [

	^ (self predefinedSymbol: aSymbol) ifNotNil: [ :s | s ] ifNil: [
		  self add: aSymbol.
		  'sym.' , aSymbol zigName ]
]

{ #category : 'writing' }
ASZagSymbols >> writeDefinitionOn: anASLLFileContext [ 

]

{ #category : 'writing' }
ASZagSymbols >> writeDefinitionWith: stream [

	| syms n |
	syms := runtimeDefined "asSortedCollection".
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
			nextPutAll: (sym ifNil: ['*Nil*']);
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
ASZagSymbols >> writeInitializationOn: anASLLFileContext [ 

]

{ #category : 'writing' }
ASZagSymbols >> writeInitializationWith: stream [

	stream nextPutAll: '    sym = Sym.init();
'
]
