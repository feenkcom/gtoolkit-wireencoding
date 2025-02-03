Extension { #name : #GtWireEncoderDecoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoderDecoder class >> defaultMapping [
	"The default mapping only encodes directly supported classes"
	| mapping |

	mapping := IdentityDictionary new.
	self map: RsrService withSubclassesTo: GtWireGemStoneRsrEncoder new in: mapping.
	mapping
		at: Association put: GtWireAssociationEncoder new;
		at: False put: GtWireFalseEncoder new;
		at: True put: GtWireTrueEncoder new;
		at: ByteArray put: GtWireByteArrayEncoder new;
		at: ByteString put: GtWireStringEncoder new;
		at: WideString put: GtWireStringEncoder new;
		at: ByteSymbol put: GtWireSymbolEncoder new;
		at: WideSymbol put: GtWireSymbolEncoder new;
		at: Character put: GtWireCharacterEncoder new;
		at: Array put: GtWireArrayEncoder new;
		at: Dictionary put: GtWireDictionaryEncoder new;
		at: OrderedCollection put: GtWireOrderedCollectionEncoder new;
		at: Set put: GtWireSetEncoder new;
		at: SmallInteger put: GtWireIntegerEncoder new;
		at: LargePositiveInteger put: GtWirePositiveIntegerEncoder new;
		at: LargeNegativeInteger put: GtWireNegativeIntegerEncoder new;
		at: SmallFloat64 put: GtWireFloatEncoder new;
		at: BoxedFloat64 put: GtWireFloatEncoder new;
		at: UndefinedObject put: GtWireNilEncoder new;
		at: DateAndTime put: GtWireDateAndTimeEncoder new;
		at: FullBlockClosure put: GtWireBlockClosureEncoder new;
		at: ConstantBlockClosure put: GtWireBlockClosureEncoder new.
	^ mapping
]
