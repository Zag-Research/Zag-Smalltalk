Class {
	#name : #ASZigSymbols,
	#superclass : #Object,
	#instVars : [
		'newlyDefined'
	],
	#classVars : [
		'Predefined'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #compiling }
ASZigSymbols class >> initialize [
	"in future better to load sysmbols directly from symbol.zig, may need a config file so we know where symbol.zig is"

	Predefined := #( yourself doesNotUnderstand: #= #+ #- #* size at: at:put: #'~='
	   #'==' #'~~' value value: value:value: negated new new: cull: value:value:value:
	   value:value:value:value: valueWithArguments: cull:cull: cull:cull:cull:
	   cull:cull:cull:cull: self name #< #'<=' #'>=' #> class Class Behavior
	   ClassDescription Metaclass SmallInteger noFallback ifTrue: ifTrue:ifFalse
	   ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil ifNotNil: ifNotNil:ifNil:
	   perform: perform:with: perform:with:with: perform:with:with:with:perform:withArguments:
	   perform:withArguments:inSuperclass:
	   #'\\' whileTrue: Object ) asSet
]

{ #category : #adding }
ASZigSymbols >> add: aSymbol [

aSymbol = #footer ifTrue: [ self halt].
	newlyDefined add: aSymbol
]

{ #category : #'as yet unclassified' }
ASZigSymbols >> emitOn: stream [
	stream << 'const Sym = struct {'.
	stream cr.
	selectors do: [ :selector |
		stream tab.
		stream << selector asZigString << ': Object,'.
		stream cr ].
	stream tab.
	stream << 'const ss = heap.compileStrings(.{'.
	stream cr.
	selectors do: [ :aSelector |
		stream tab.
		stream tab.
		stream << '"' << aSelector << '",'.
		stream cr ].
	stream tab.
	stream << '});'.
	stream cr.
	stream tab.
	stream << 'usingnamespace symbol.symbols;'.
	stream cr.
	stream tab.
	stream << 'fn init() Sym {'.
	stream cr.
	stream tab.
	stream tab.
	stream << 'return .{'.
	stream cr.
	selectors withIndexDo: [ :aSelector :index |
		stream tab.
		stream tab.
		stream tab.
		stream << '.' << aSelector asZigString << ' = symbol.intern(ss['
		<< (index - 1) asString << '].asObject()),'.
		stream cr ].
	stream tab.
	stream tab.
	stream << '};'.
	stream cr.
	stream tab.
	stream << '}'.
	stream cr.
	stream << '};'.
	stream cr.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigSymbols >> emitZigSymbols: syms selectors: selectors [ 
	selectors removeAllFoundIn: syms keys.
	stream << 'const Sym = struct {'.
	stream cr.
	selectors do: [ :selector |
		stream tab.
		stream << selector asZigString << ': Object,'.
		stream cr ].
	stream tab.
	stream << 'const ss = heap.compileStrings(.{'.
	stream cr.
	selectors do: [ :aSelector |
		stream tab.
		stream tab.
		stream << '"' << aSelector << '",'.
		stream cr ].
	stream tab.
	stream << '});'.
	stream cr.
	stream tab.
	stream << 'usingnamespace symbol.symbols;'.
	stream cr.
	stream tab.
	stream << 'fn init() Sym {'.
	stream cr.
	stream tab.
	stream tab.
	stream << 'return .{'.
	stream cr.
	selectors withIndexDo: [ :aSelector :index |
		stream tab.
		stream tab.
		stream tab.
		stream << '.' << aSelector asZigString << ' = symbol.intern(ss['
		<< (index - 1) asString << '].asObject()),'.
		stream cr ].
	stream tab.
	stream tab.
	stream << '};'.
	stream cr.
	stream tab.
	stream << '}'.
	stream cr.
	stream << '};'.
	stream cr.
	stream cr
]

{ #category : #initialization }
ASZigSymbols >> initialize [ 
	super initialize .
	newlyDefined := Set new
]

{ #category : #testing }
ASZigSymbols >> predefinedSymbol: aSymbol [

	^ (Predefined includes: aSymbol) ifTrue: [ 'Sym.' , aSymbol zigName ]
]

{ #category : #testing }
ASZigSymbols >> printable: aSymbol [

	^ (self predefinedSymbol: aSymbol) ifNotNil: [ :s | s ] ifNil: [
		  self add: aSymbol.
		  'sym.' , aSymbol zigName ]
]

{ #category : #writing }
ASZigSymbols >> writeDefinitionOn: stream [

	| syms n |
	syms := newlyDefined asSortedCollection.
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
			nextPutAll: sym;
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

{ #category : #writing }
ASZigSymbols >> writeInitializationOn: stream [

	stream nextPutAll: '    sym = Sym.init();
'
]
