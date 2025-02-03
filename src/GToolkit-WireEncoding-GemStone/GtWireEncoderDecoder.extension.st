Extension { #name : 'GtWireEncoderDecoder' }

{ #category : '*GToolkit-WireEncoding-GemStone' }
GtWireEncoderDecoder class >> defaultMapping [
	"The default mapping only encodes directly supported classes"
	| mapping |

	mapping := IdentityDictionary new.
	self map: ExecBlock withSubclassesTo: GtWireBlockClosureEncoder new in: mapping.
	self map: RsrService withSubclassesTo: GtWireGemStoneRsrEncoder new in: mapping.
	mapping
		at: Association put: GtWireAssociationEncoder new;
		at: Boolean put: GtWireBooleanEncoder new;
		at: ByteArray put: GtWireByteArrayEncoder new;
		at: String  put: GtWireStringEncoder new;
		at: DoubleByteString put: GtWireStringEncoder new;
		at: Unicode7 put: GtWireStringEncoder new;
		at: Unicode16 put: GtWireStringEncoder new;
		at: Unicode32 put: GtWireStringEncoder new;
		at: Symbol put: GtWireSymbolEncoder new;
		at: DoubleByteSymbol put: GtWireSymbolEncoder new;
		at: Character put: GtWireCharacterEncoder new;
		at: Array put: GtWireArrayEncoder new;
		at: Dictionary put: GtWireDictionaryEncoder new;
		at: OrderedCollection put: GtWireOrderedCollectionEncoder new;
		at: Set put: GtWireSetEncoder new;
		at: SmallInteger put: GtWireIntegerEncoder new;
		at: LargeInteger put: GtWireIntegerEncoder new;
		at: Float put: GtWireFloatEncoder new;
		at: SmallDouble put: GtWireFloatEncoder new;
		at: UndefinedObject put: GtWireNilEncoder new;
		at: DateAndTime put: GtWireDateAndTimeEncoder new;
		at: SmallDateAndTime put: GtWireDateAndTimeEncoder new;
		at: ExecBlock put: GtWireBlockClosureEncoder new;
		at: ExecBlock0 put: GtWireBlockClosureEncoder new;
		at: ExecBlock1 put: GtWireBlockClosureEncoder new;
		at: ExecBlock2 put: GtWireBlockClosureEncoder new;
		at: ExecBlock3 put: GtWireBlockClosureEncoder new;
		at: ExecBlock4 put: GtWireBlockClosureEncoder new;
		at: ExecBlock5 put: GtWireBlockClosureEncoder new;
		at: ExecBlockN put: GtWireBlockClosureEncoder new.
	^ mapping
]
