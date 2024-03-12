"
I manage the values to go into a zag symbols table. I have predefined symbols and runtime symbols.
"
Class {
	#name : 'ASZigSymbols',
	#superclass : 'ASZagSymbols',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'as yet unclassified' }
ASZigSymbols >> predefinedSymbol: aSymbol [

	^ (Predefined includes: aSymbol) ifTrue: [ 'Sym.' , aSymbol zigName ]
]

{ #category : 'as yet unclassified' }
ASZigSymbols >> printable: aSymbol [

	^ (self predefinedSymbol: aSymbol) ifNotNil: [ :s | s ] ifNil: [
		  self add: aSymbol.
		  'sym.' , aSymbol zigName ]
]

{ #category : 'as yet unclassified' }
ASZigSymbols >> writeDefinitionOn: stream [

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

{ #category : 'as yet unclassified' }
ASZigSymbols >> writeInitializationOn: stream [

	stream nextPutAll: '    sym = Sym.init();
'
]
