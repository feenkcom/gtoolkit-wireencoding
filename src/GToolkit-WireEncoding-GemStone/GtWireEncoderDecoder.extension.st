Extension { #name : #GtWireEncoderDecoder }

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireEncoderDecoder class >> defaultMapping [
	"The default mapping only encodes directly supported classes"

	^ IdentityDictionary new
		at: Association put: GtWireAssociationEncoder new;
		at: Boolean put: GtWireBooleanEncoder new;
		at: ByteArray put: GtWireByteArrayEncoder new;
		at: Symbol put: GtWireSymbolEncoder new;
		at: Character put: GtWireCharacterEncoder new;
		at: Array put: GtWireArrayEncoder new;
		at: Dictionary put: GtWireDictionaryEncoder new;
		at: OrderedCollection put: GtWireOrderedCollectionEncoder new;
		at: Set put: GtWireSetEncoder new;
		at: SmallInteger put: GtWireIntegerEncoder new;
		at: UndefinedObject put: GtWireNilEncoder new;
		at: DateAndTime put: GtWireDateAndTimeEncoder new;
		yourself
]
