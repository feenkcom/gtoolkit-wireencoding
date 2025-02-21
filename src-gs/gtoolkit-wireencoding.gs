! Class Declarations
! Generated file, do not Edit

doit
(Error
	subclass: 'GtWireUnsupportedObject'
	instVarNames: #(object)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireUnsupportedObject
removeallclassmethods GtWireUnsupportedObject

doit
(Object
	subclass: 'GtWireEncoderDecoder'
	instVarNames: #(stream map reverseMap)
	classVars: #(GtDefaultMap GtDefaultReverseMap)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireEncoderDecoder
removeallclassmethods GtWireEncoderDecoder

doit
(GtWireEncoderDecoder
	subclass: 'GtWireDecoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireDecoder
removeallclassmethods GtWireDecoder

doit
(GtWireDecoder
	subclass: 'GtWireInspectionDecoder'
	instVarNames: #(stack root byteArray)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireInspectionDecoder
removeallclassmethods GtWireInspectionDecoder

doit
(GtWireEncoderDecoder
	subclass: 'GtWireEncoder'
	instVarNames: #(defaultEncoder maxObjects objectCount remainingDepth)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireEncoder
removeallclassmethods GtWireEncoder

doit
(Object
	subclass: 'GtWireEncodingExampleInstVarObject'
	instVarNames: #(var1 var2 var3 var4)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireEncodingExampleInstVarObject
removeallclassmethods GtWireEncodingExampleInstVarObject

doit
(Object
	subclass: 'GtWireEncodingExamples'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireEncodingExamples
removeallclassmethods GtWireEncodingExamples

doit
(Object
	subclass: 'GtWireEncodingInspectionObject'
	instVarNames: #(startIndex object decoder parent children components endIndex)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireEncodingInspectionObject
removeallclassmethods GtWireEncodingInspectionObject

doit
(Object
	subclass: 'GtWireNestedEncodingExamples'
	instVarNames: #(signals)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireNestedEncodingExamples
removeallclassmethods GtWireNestedEncodingExamples

doit
(Object
	subclass: 'GtWireObjectEncoder'
	instVarNames: #()
	classVars: #(DefaultMap)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireObjectEncoder
removeallclassmethods GtWireObjectEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireAssociationEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireAssociationEncoder
removeallclassmethods GtWireAssociationEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireBlockClosureEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireBlockClosureEncoder
removeallclassmethods GtWireBlockClosureEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireBooleanEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireBooleanEncoder
removeallclassmethods GtWireBooleanEncoder

doit
(GtWireBooleanEncoder
	subclass: 'GtWireFalseEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireFalseEncoder
removeallclassmethods GtWireFalseEncoder

doit
(GtWireBooleanEncoder
	subclass: 'GtWireTrueEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireTrueEncoder
removeallclassmethods GtWireTrueEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireByteArrayEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireByteArrayEncoder
removeallclassmethods GtWireByteArrayEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireCharacterArrayEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireCharacterArrayEncoder
removeallclassmethods GtWireCharacterArrayEncoder

doit
(GtWireCharacterArrayEncoder
	subclass: 'GtWireStringEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireStringEncoder
removeallclassmethods GtWireStringEncoder

doit
(GtWireCharacterArrayEncoder
	subclass: 'GtWireSymbolEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireSymbolEncoder
removeallclassmethods GtWireSymbolEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireCharacterEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireCharacterEncoder
removeallclassmethods GtWireCharacterEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireCollectionEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireCollectionEncoder
removeallclassmethods GtWireCollectionEncoder

doit
(GtWireCollectionEncoder
	subclass: 'GtWireArrayEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireArrayEncoder
removeallclassmethods GtWireArrayEncoder

doit
(GtWireCollectionEncoder
	subclass: 'GtWireDictionaryEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireDictionaryEncoder
removeallclassmethods GtWireDictionaryEncoder

doit
(GtWireCollectionEncoder
	subclass: 'GtWireOrderedCollectionEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireOrderedCollectionEncoder
removeallclassmethods GtWireOrderedCollectionEncoder

doit
(GtWireCollectionEncoder
	subclass: 'GtWireSetEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireSetEncoder
removeallclassmethods GtWireSetEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireDateAndTimeEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireDateAndTimeEncoder
removeallclassmethods GtWireDateAndTimeEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireFloatEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireFloatEncoder
removeallclassmethods GtWireFloatEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireGemStoneOopEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireGemStoneOopEncoder
removeallclassmethods GtWireGemStoneOopEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireGemStoneRsrEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireGemStoneRsrEncoder
removeallclassmethods GtWireGemStoneRsrEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireInstVarEncoder'
	instVarNames: #(instVarMap)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		comment: 'GtWireInstVarEncoder uses the specified mapping to encode the supplied objects.  No mapping is required for decoding.';
		immediateInvariant.
true.
%

removeallmethods GtWireInstVarEncoder
removeallclassmethods GtWireInstVarEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireIntegerEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireIntegerEncoder
removeallclassmethods GtWireIntegerEncoder

doit
(GtWireIntegerEncoder
	subclass: 'GtWireNegativeIntegerEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireNegativeIntegerEncoder
removeallclassmethods GtWireNegativeIntegerEncoder

doit
(GtWireIntegerEncoder
	subclass: 'GtWirePositiveIntegerEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWirePositiveIntegerEncoder
removeallclassmethods GtWirePositiveIntegerEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireMaxDepthEncoder'
	instVarNames: #(depth encoder)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireMaxDepthEncoder
removeallclassmethods GtWireMaxDepthEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireMinDepthEncoder'
	instVarNames: #(depth encoder)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireMinDepthEncoder
removeallclassmethods GtWireMinDepthEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireNilEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireNilEncoder
removeallclassmethods GtWireNilEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireObjectByNameEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireObjectByNameEncoder
removeallclassmethods GtWireObjectByNameEncoder

doit
(GtWireObjectEncoder
	subclass: 'GtWireStonEncoder'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireStonEncoder
removeallclassmethods GtWireStonEncoder

doit
(Stream
	subclass: 'GtWireStream'
	instVarNames: #(wrappedStream)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'GToolkit-WireEncoding';
		immediateInvariant.
true.
%

removeallmethods GtWireStream
removeallclassmethods GtWireStream

! Class implementation for 'GtWireUnsupportedObject'

!		Class methods for 'GtWireUnsupportedObject'

category: 'signaling'
classmethod: GtWireUnsupportedObject
signal: anObject

	self new
		object: anObject;
		messageText: 'Unable to serialize', anObject class name;
		signal
%

!		Instance methods for 'GtWireUnsupportedObject'

category: 'accessing'
method: GtWireUnsupportedObject
object
	^ object
%

category: 'accessing'
method: GtWireUnsupportedObject
object: anObject
	object := anObject
%

! Class implementation for 'GtWireEncoderDecoder'

!		Class methods for 'GtWireEncoderDecoder'

category: 'maintenance'
classmethod: GtWireEncoderDecoder
generateDefaultGsMapMethod
	"Generate and save the defaultMap method for GS.
	Traversing a class hierarchy is very expensive, especially on GemStone.
	Generate a method to create the map by traversing the required class hierarchies."
	| mapping source |
	
	mapping := self defaultMapping.
	source := self generateDefaultGsMapMethodFrom: mapping.
	"How to compile on GemStone when under Rowan control?"
	"self class
		compileMethod: source
		dictionaries: GsCurrentSession currentSession symbolList
		category: '*GToolkit-WireEncoding-GemStone'
		environmentId: 0."
	^ source
%

category: 'maintenance'
classmethod: GtWireEncoderDecoder
generateDefaultGsMapMethodFrom: aDictionary
	"Answer the source code for the #defaultMap method from the supplied map dictionary.
	.gs files don't allow undeclared classes to be referenced... 
	hack around it by deferring class lookup."
	| source |

	source := String streamContents: [ :stream |
		stream
			<< 'getDefaultMap'; lf;
			tab; << '"Generated by #generateDefaultMapMethodFrom:.'; lf;
			tab; << 'Original source is #defaultMapping, changes should be made there and the code regenerated."'; lf;
			lf;
			tab; << '^ IdentityDictionary new'; lf.
		(aDictionary keys asSortedCollection: [ :a :b | a name < b name ]) do: [ :key |
				stream
					tab; tab; << 'at: ((self lookupClass: #';
					<< key name;
					<< (') ifNil: [ self error: ''Unable to find: ', key name asString, ''' ]) put: ');
					<< (aDictionary at: key) class name;
					<< ' new' ]
			separatedBy: [ stream << ';'; lf ].
		stream
			<< ';'; lf;
			tab; tab; << 'yourself.'; lf ].
	^ source
%

category: 'maintenance'
classmethod: GtWireEncoderDecoder
generateDefaultGsReverseMapMethod
	"Generate and save the defaultMap method for GS.
	Traversing a class hierarchy is very expensive, especially on GemStone.
	Generate a method to create the map by traversing the required class hierarchies."
	| reverse source |
	
	reverse := self reverseMapFrom: self defaultMapping.
	source := self generateDefaultReverseMapMethodFrom: reverse.
	"How to compile on GemStone when under Rowan control?"
	"self class
		compileMethod: source
		dictionaries: GsCurrentSession currentSession symbolList
		category: '*GToolkit-WireEncoding-GemStone'
		environmentId: 0."
	^ source
%

category: 'maintenance'
classmethod: GtWireEncoderDecoder
generateDefaultGtMapMethod
	"Generate and save the defaultMap method for Gt.
	Traversing a class hierarchy is very expensive, especially on GemStone.
	Generate a method to create the map by traversing the required class hierarchies."
	| mapping source reverse |

	mapping := self defaultMapping.
	source := self generateDefaultMapMethodFrom: mapping.
	self class
		compile: source
		classified: '*GToolkit-WireEncoding-GT'.

	reverse := self reverseMapFrom: mapping.
	source := self generateDefaultReverseMapMethodFrom: reverse.
	self class
		compile: source
		classified: '*GToolkit-WireEncoding-GT'.
%

category: 'maintenance'
classmethod: GtWireEncoderDecoder
generateDefaultMapMethodFrom: aDictionary
	"Answer the source code for the #defaultMap method from the supplied map dictionary"
	| source |

	source := String streamContents: [ :stream |
		stream
			<< 'getDefaultMap'; lf;
			tab; << '"Generated by #generateDefaultMapMethodFrom:.'; lf;
			tab; << 'Original source is #defaultMapping, changes should be made there and the code regenerated."'; lf;
			lf;
			tab; << '^ IdentityDictionary new'; lf.
		(aDictionary keys asSortedCollection: [ :a :b | a name < b name ]) do: [ :key |
				stream
					tab; tab; << 'at: ';
					<< key name;
					<< ' put: ';
					<< (aDictionary at: key) class name;
					<< ' new' ]
			separatedBy: [ stream << ';'; lf ].
		stream
			<< ';'; lf;
			tab; tab; << 'yourself.'; lf ].
	^ source
%

category: 'maintenance'
classmethod: GtWireEncoderDecoder
generateDefaultReverseMapMethodFrom: aDictionary
	"Answer the source code for the #defaultMap method from the supplied map dictionary"
	| source |

	source := String streamContents: [ :stream |
		stream
			<< 'getDefaultReverseMap'; lf;
			tab; << '"Generated by #generateDefaultReverseMapMethodFrom:.'; lf;
			tab; << 'Original source is #defaultMapping, changes should be made there and the code regenerated."'; lf;
			lf;
			tab; << '^ IdentityDictionary new'; lf.
		aDictionary keys asSortedCollection do: [ :key |
				stream
					tab; tab; << 'at: ';
					<< key asString;
					<< ' put: ';
					<< (aDictionary at: key) class name;
					<< ' new' ]
			separatedBy: [ stream << ';'; lf ].
		stream
			<< ';'; lf;
			tab; tab; << 'yourself.'; lf ].
	^ source
%

category: 'initialization'
classmethod: GtWireEncoderDecoder
initialize
	"NOTE: GtDefaultMap and GtDefaultReverseMap should only be used on GT.
	On GemStone they should always be nil."

	GtDefaultMap := GtDefaultReverseMap := nil
%

category: 'private'
classmethod: GtWireEncoderDecoder
lookupClass: className
	"Answer the class with the supplied name or nil if not found.
	For GemStone, see STONReader>>lookupClass: for inspiration."

	^ self
		gtDo: [ self class environment classOrTraitNamed: className ]
		gemstoneDo: [ System myUserProfile objectNamed: className asSymbol ]
%

category: 'private'
classmethod: GtWireEncoderDecoder
map: aClass withSubclassesTo: anEncoder in: mapping
	"Add the mapping for aClass and all its subclasses"
	
	"GemStone doesn't have #withAllSubclasses"
	mapping at: aClass put: anEncoder.
	aClass allSubclasses do: [ :cls |
		mapping at: cls put: anEncoder ].
%

category: 'accessing'
classmethod: GtWireEncoderDecoder
reverseMapFrom: aDictionary
	"Construct the reverse map.
	Build a default reverse map and then  overwrite with the supplied configured encoders."
	| reverseMap |

	reverseMap := IdentityDictionary new.
	GtWireObjectEncoder allSubclasses do: [ :cls |
		cls typeIdentifierOrNil ifNotNil:
			[ :id | reverseMap at: id put: cls new ] ].
	aDictionary associationsDo: [ :assoc |
		assoc value class typeIdentifierOrNil ifNotNil: [ :id |
			reverseMap at: id put: assoc value ] ].
	^ reverseMap
%

!		Instance methods for 'GtWireEncoderDecoder'

category: 'accessing'
method: GtWireEncoderDecoder
addMapping: aClass to: anEncoder
	"Add/Overwrite the supplied encoder.
	Be careful not to accidentally overwrite the default map unintentionally."
	| typeIdentifier |
	
	self map at: aClass put: anEncoder.
	typeIdentifier := anEncoder class typeIdentifierOrNil.
	typeIdentifier ifNotNil:
		[ self reverseMap at: typeIdentifier put: anEncoder ]
%

category: 'accessing'
method: GtWireEncoderDecoder
contents

	^ stream contents
%

category: 'accessing'
method: GtWireEncoderDecoder
map

	^ map ifNil: [ map := self class defaultMap ]
%

category: 'accessing'
method: GtWireEncoderDecoder
map: aDictionary

	map := aDictionary.
	reverseMap := nil.
%

category: 'initialization'
method: GtWireEncoderDecoder
reset

	stream reset
%

category: 'accessing'
method: GtWireEncoderDecoder
reverseMap

	^ reverseMap ifNil: [ reverseMap := self class reverseMapFrom: self map ]
%

category: 'accessing'
method: GtWireEncoderDecoder
reverseMap: anObject
	reverseMap := anObject
%

category: 'accessing'
method: GtWireEncoderDecoder
stream

	^ stream ifNil: [ stream := GtWireStream on:
		(ByteArray new: 64 * 1024) ]
%

category: 'accessing'
method: GtWireEncoderDecoder
stream: aGtWireReadWriteStream

	stream := aGtWireReadWriteStream
%

! Class implementation for 'GtWireDecoder'

!		Class methods for 'GtWireDecoder'

category: 'instance creation'
classmethod: GtWireDecoder
on: aReadStream

	^ self basicNew initialize stream:
		(GtWireStream on: aReadStream)
%

!		Instance methods for 'GtWireDecoder'

category: 'accessing'
method: GtWireDecoder
next
	| type |

	type := self nextTypeIdentifier.
	^ (self reverseMap at: type) decodeWith: self.
%

category: 'as yet unclassified'
method: GtWireDecoder
nextByteArray

	^ stream next: self nextSize
%

category: 'as yet unclassified'
method: GtWireDecoder
nextFloat64

	^ stream float64
%

category: 'as yet unclassified'
method: GtWireDecoder
nextInt64

	^ stream int64
%

category: 'as yet unclassified'
method: GtWireDecoder
nextPackedInteger

	^ stream packedInteger
%

category: 'as yet unclassified'
method: GtWireDecoder
nextSize

	^ stream packedInteger
%

category: 'as yet unclassified'
method: GtWireDecoder
nextString
	"Answer the next string.
	GemStone requires conversion from a Unicode object to a String (asString)"

	^ (stream next: self nextSize) utf8Decoded asString
%

category: 'as yet unclassified'
method: GtWireDecoder
nextTypeIdentifier

	^ stream packedInteger
%

! Class implementation for 'GtWireInspectionDecoder'

!		Class methods for 'GtWireInspectionDecoder'

category: 'instance creation'
classmethod: GtWireInspectionDecoder
byteArray: aByteArray

	^ (self on: aByteArray readStream)
		byteArray: aByteArray;
		next;
		root
%

!		Instance methods for 'GtWireInspectionDecoder'

category: 'accessing'
method: GtWireInspectionDecoder
byteArray
	^ byteArray
%

category: 'accessing'
method: GtWireInspectionDecoder
byteArray: anObject
	byteArray := anObject
%

category: 'initialization'
method: GtWireInspectionDecoder
initialize

	super initialize.
	"Use an OrderedCollection for the stack since GemStone doesn't have a Stack"
	stack := OrderedCollection new.
%

category: 'accessing'
method: GtWireInspectionDecoder
next
	| inspectionObject object parent |

	inspectionObject := GtWireEncodingInspectionObject new.
	root ifNil: [ root := inspectionObject ].
	parent := stack
			ifEmpty: [ nil ]
			ifNotEmpty: [ stack last ].
	inspectionObject 
		parent: parent;
		startIndex: stream position + 1;
		decoder: self.
	stack addLast: inspectionObject.
	object := super next.
	inspectionObject 
		object: object;
		endIndex: stream position.
	parent ifNotNil: [ parent addComponent: #object ->inspectionObject ].
	stack removeLast.
	^ object
%

category: 'accessing'
method: GtWireInspectionDecoder
nextByteArray
	| ba |
	ba := super nextByteArray.
	stack last addComponent: #byteArray -> ba.
	^ ba.
%

category: 'accessing'
method: GtWireInspectionDecoder
nextFloat64
	| float64 |

	float64 := super nextFloat64.
	stack last addComponent: #float64 -> float64.
	^ float64.
%

category: 'accessing'
method: GtWireInspectionDecoder
nextInt64
	| int64 |

	int64 := super nextInt64.
	stack last addComponent: #int64 -> int64.
	^ int64.
%

category: 'accessing'
method: GtWireInspectionDecoder
nextPackedInteger
	| packedInteger |

	packedInteger := super nextPackedInteger.
	stack last addComponent: #packedInteger -> packedInteger.
	^ packedInteger.
%

category: 'accessing'
method: GtWireInspectionDecoder
nextSize
	| size |

	size := super nextSize.
	stack last addComponent: #size -> size.
	^ size.
%

category: 'accessing'
method: GtWireInspectionDecoder
nextString
	| string |

	string := super nextString.
	stack last addComponent: #string -> string.
	^ string.
%

category: 'accessing'
method: GtWireInspectionDecoder
nextTypeIdentifier
	| typeIdentifier |

	typeIdentifier := super nextTypeIdentifier.
	stack last addComponent: #typeIdentifier -> typeIdentifier.
	^ typeIdentifier.
%

category: 'accessing'
method: GtWireInspectionDecoder
root
	^ root
%

! Class implementation for 'GtWireEncoder'

!		Class methods for 'GtWireEncoder'

category: 'instance creation'
classmethod: GtWireEncoder
on: aWriteStream

	^ self basicNew initialize stream:
		(GtWireStream on: aWriteStream)
%

category: 'instance creation'
classmethod: GtWireEncoder
onByteArray

	^ self on: (WriteStream on: (ByteArray new: 100))
%

!		Instance methods for 'GtWireEncoder'

category: 'accessing'
method: GtWireEncoder
decoderOn: aReadStream
	| decoder |
	
	decoder := GtWireDecoder on: aReadStream.
	decoder 
		map: self map;
		reverseMap: self reverseMap.
	^ decoder
%

category: 'initialization'
method: GtWireEncoder
initialize

	super initialize.
	defaultEncoder := [ :anObject | GtWireObjectByNameEncoder new ].
	maxObjects := 500000.
	objectCount := 0.
	remainingDepth := maxObjects.
%

category: 'accessing'
method: GtWireEncoder
maxObjects
	^ maxObjects
%

category: 'accessing'
method: GtWireEncoder
maxObjects: anObject
	maxObjects := anObject
%

category: 'accessing'
method: GtWireEncoder
nextPut: anObject

	objectCount > maxObjects ifTrue:
		[ self error: 'Exceeded maximum object count' ].
	remainingDepth := remainingDepth - 1.
	remainingDepth < 0 ifTrue:
		[ self putNil ]
	ifFalse:
		[ (self map at: anObject class
			ifAbsent: [ defaultEncoder value: anObject ])
				encode: anObject with: self ].
	objectCount := objectCount + 1.
	remainingDepth := remainingDepth + 1.
%

category: 'private - encoding'
method: GtWireEncoder
putByteArray: aByteArray

	self putSize: aByteArray size.
	stream nextPutAll: aByteArray.
%

category: 'private - encoding'
method: GtWireEncoder
putFloat64: aFloat

	"GtWireEncodingFloat64Signal new
		float: aFloat;
		emit."
	stream float64: aFloat.
%

category: 'private - encoding'
method: GtWireEncoder
putInt64: anInteger

	"GtWireEncodingInt64Signal new
		integer: anInteger;
		emit."
	stream int64: anInteger.
%

category: 'accessing'
method: GtWireEncoder
putNil

	self putTypeIdentifier: GtWireNilEncoder typeIdentifier.
%

category: 'private - encoding'
method: GtWireEncoder
putPackedInteger: aPositiveInteger

	"GtWireEncodingPositiveIntegerSignal new
		integer: aPositiveInteger;
		emit."
	stream packedInteger: aPositiveInteger.
%

category: 'private - encoding'
method: GtWireEncoder
putSize: anInteger

	"GtWireEncodingSizeSignal new
		size: anInteger;
		emit."
	stream packedInteger: anInteger.
%

category: 'private - encoding'
method: GtWireEncoder
putString: aString
	| encoded |

	encoded := aString utf8Encoded.
	self putSize: encoded size.
	stream nextPutAll: encoded.
%

category: 'private - encoding'
method: GtWireEncoder
putTypeIdentifier: anInteger

	"GtWireEncodingTypeIdentifierSignal new
		typeIdentifier: anInteger;
		emit."
	stream packedInteger: anInteger.
%

category: 'accessing'
method: GtWireEncoder
remainingDepth
	^ remainingDepth
%

category: 'accessing'
method: GtWireEncoder
remainingDepth: anObject
	remainingDepth := anObject
%

category: 'initialization'
method: GtWireEncoder
reset

	super reset.
	objectCount := 0.
%

! Class implementation for 'GtWireEncodingExampleInstVarObject'

!		Class methods for 'GtWireEncodingExampleInstVarObject'

category: 'accessing'
classmethod: GtWireEncodingExampleInstVarObject
leJsonV4Name

	^  #gtWireEncodingExampleInstVarObject
%

!		Instance methods for 'GtWireEncodingExampleInstVarObject'

category: 'as yet unclassified'
method: GtWireEncodingExampleInstVarObject
= anObject

	self == anObject ifTrue: [ ^ true ].
	anObject class = self class ifFalse: [ ^ false ].
	^ anObject var1 = var1 and:
		[ anObject var2 = var2 and:
		[ anObject var3 = var3 and:
		[ anObject var4 = var4 ] ] ]
%

category: 'as yet unclassified'
method: GtWireEncodingExampleInstVarObject
hash

	^ var1 hash bitXor:
		(var2 hash bitXor:
		(var3 hash bitXor:
		var4 hash))
%

category: 'accessing'
method: GtWireEncodingExampleInstVarObject
var1
	^ var1
%

category: 'accessing'
method: GtWireEncodingExampleInstVarObject
var1: anObject
	var1 := anObject
%

category: 'accessing'
method: GtWireEncodingExampleInstVarObject
var2
	^ var2
%

category: 'accessing'
method: GtWireEncodingExampleInstVarObject
var2: anObject
	var2 := anObject
%

category: 'accessing'
method: GtWireEncodingExampleInstVarObject
var3
	^ var3
%

category: 'accessing'
method: GtWireEncodingExampleInstVarObject
var3: anObject
	var3 := anObject
%

category: 'accessing'
method: GtWireEncodingExampleInstVarObject
var4
	^ var4
%

category: 'accessing'
method: GtWireEncodingExampleInstVarObject
var4: anObject
	var4 := anObject
%

! Class implementation for 'GtWireEncodingExamples'

!		Instance methods for 'GtWireEncodingExamples'

category: 'examples'
method: GtWireEncodingExamples
array
	<gtExample>
	| array encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	array := { 1. 'hello'. #hello. }.
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 18.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next = array.
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
association
	<gtExample>
	| association encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	association := 1 -> 'one'.
	encoder nextPut: association.
	byteArray := encoder contents.
	self assert: byteArray size equals: 8.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Association.
	self assert: next = association.
%

category: 'examples'
method: GtWireEncodingExamples
blockClosure
	<gtExample>
	| blockClosure encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	blockClosure := [ :a :b | a + b ].
	encoder nextPut: blockClosure.
	byteArray := encoder contents.
	self assert: byteArray size equals: 20.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(FullBlockClosure ExecBlock) includes: next class name).
	self assert: (next value: 4 value: 3) equals: 7.
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
boolean
	<gtExample>
	| encoder byteArray decoder |

	encoder := GtWireEncoder onByteArray.
	encoder nextPut: true.
	encoder nextPut: false.
	byteArray := encoder contents.
	self assert: byteArray size equals: 2.
	decoder := GtWireDecoder on: byteArray readStream.
	self assert: decoder next.
	self assert: decoder next not.
%

category: 'examples'
method: GtWireEncodingExamples
byteArray
	<gtExample>
	| source encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	source := #[3 1 4 1 5].
	encoder nextPut: source.
	byteArray := encoder contents.
	self assert: byteArray size equals: source size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: ByteArray.
	self assert: next equals: source.
%

category: 'examples'
method: GtWireEncodingExamples
byteString
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := 'Hello, World'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(ByteString String) includes: next class name).
	self assert: next equals: string.
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
byteStringWithNull
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := 'abc', (String with: Character null), 'def'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(ByteString String) includes: next class name).
	self assert: next equals: string.
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
byteSymbol
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := #'Hello, World'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	"Allow for differences in GT & GS class hierarchy"
	self assert: (#(ByteSymbol Symbol) includes: next class name).
	self assert: next equals: string.
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
character
	<gtExample>
	| encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	encoder nextPut: $§.
	byteArray := encoder contents.
	self assert: byteArray size equals: 3.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Character.
	self assert: next == $§.
%

category: 'examples'
method: GtWireEncodingExamples
dateAndTime
	<gtExample>
	| dateAndTime encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	dateAndTime := DateAndTime now.
	encoder nextPut: dateAndTime.
	byteArray := encoder contents.
	self assert: (byteArray size between: 10 and: 25).
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (next isKindOf: DateAndTime).
	self assert: next = dateAndTime.
%

category: 'examples'
method: GtWireEncodingExamples
deepArray
	<gtExample>
	| array currentArray encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	array := Array new: 2.
	currentArray := array.
	1 to: 5 do: [ :i |
		currentArray
			at: 1 put: i;
			at: 2 put: (Array new: 2).
		currentArray := currentArray second ].
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 24.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next = array.
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
dictionary
	<gtExample>
	| dictionary encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	dictionary := {
		 1 -> 'one'.
		 2 -> 'two' } asDictionary.
	encoder nextPut: dictionary.
	byteArray := encoder contents.
	self assert: byteArray size equals: 16.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Dictionary.
	self assert: next = dictionary.
%

category: 'examples'
method: GtWireEncodingExamples
float
	<gtExample>
	| encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	{ Float fmin. Float fmax. 1.25. } doWithIndex: [ :f :i |
		encoder reset.
		encoder nextPut: f.
		byteArray := encoder contents.
		self assert: byteArray size equals: 9.
		next := (GtWireDecoder on: byteArray readStream) next.
		self assert: next equals: f ].
%

category: 'examples'
method: GtWireEncodingExamples
generalObject
	<gtExample>
	| object encoder byteArray next root |

	encoder := GtWireEncoder onByteArray.
	object := self gtDo:[ (self class environment classOrTraitNamed: #AdditionalMethodState) new: 3]
		gemstoneDo: [ ^ self ].
	object
		selector: #one;
		method: 'fake'.
	1 to: 3 do: [ :i |
		object basicAt: i put: 2 ** i ].
	encoder nextPut: object.
	byteArray := encoder contents.
	self assert: byteArray size equals: 60.
	root := GtWireInspectionDecoder byteArray: byteArray.
	next := root object.
	self assert: (#(AdditionalMethodState) includes: next class name).
	self assert: next basicSize equals: 3.
	1 to: 3 do: [ :i |
		self assert: (next basicAt: i) equals: 2 ** i ].
	self assert: next selector equals: #one.
	self assert: next method equals: 'fake'.
%

category: 'examples'
method: GtWireEncodingExamples
maxDepth
	<gtExample>
	| array currentArray encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	array := Array new: 2.
	currentArray := array.
	1 to: 5 do: [ :i |
		currentArray
			at: 1 put: i;
			at: 2 put: (Array new: 2).
		currentArray := currentArray second ].
	encoder 
		remainingDepth: 2;
		nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 8.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next equals: (Array new: 2).
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
nil
	<gtExample>
	| encoder byteArray |

	encoder := GtWireEncoder onByteArray.
	encoder nextPut: nil.
	byteArray := encoder contents.
	self assert: byteArray size equals: 1.
	self assert: (GtWireDecoder on: byteArray readStream) next isNil.
%

category: 'examples'
method: GtWireEncodingExamples
orderedCollection
	<gtExample>
	| orderedCollection encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	orderedCollection := { 1. 'hello'. #hello. } asOrderedCollection.
	encoder nextPut: orderedCollection.
	byteArray := encoder contents.
	self assert: byteArray size equals: 18.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: OrderedCollection.
	self assert: next = orderedCollection.
%

category: 'examples'
method: GtWireEncodingExamples
packedInteger
	<gtExample>
	| encoder byteArray integer |

	encoder := GtWireEncoder onByteArray.
	encoder nextPut: 0.
	byteArray := encoder contents.
	self assert: byteArray equals: #[13 0].
	self assert: (GtWireDecoder on: byteArray readStream) next equals: 0.
	integer := 1.
	"GemStone is slow at this test, if it works in GT for the full range, testing a small range in
	GemStone is probably enough"
	[ integer < ((self gtDo: [ SmallInteger maxVal ] gemstoneDo: [ 10 ]) * 10) ] whileTrue:
		[ encoder reset.
		encoder nextPut: integer.
		byteArray := encoder contents.
		self assert: (GtWireDecoder on: byteArray readStream) next equals: integer.
		encoder reset.
		encoder nextPut: integer negated.
		byteArray := encoder contents.
		encoder reset.
		self assert: (GtWireDecoder on: byteArray readStream) next equals: integer negated.
		integer := (1.001 * integer) ceiling. ].
%

category: 'examples'
method: GtWireEncodingExamples
set
	<gtExample>
	| set encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	set := { 1. 'hello'. true. } asSet.
	encoder nextPut: set.
	byteArray := encoder contents.
	self assert: byteArray size equals: 12.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Set.
	self assert: next = set.
%

category: 'examples'
method: GtWireEncodingExamples
wideString
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := 'čtyři'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string asString utf8Encoded size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(WideString DoubleByteString Unicode16) includes: next class name).
	self assert: next equals: string.
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
wideStringWithNull
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := 'čty', (String with: Character null), 'ři'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string asString utf8Encoded size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(WideString DoubleByteString Unicode16) includes: next class name).
	self assert: next equals: string.
	^ byteArray
%

category: 'examples'
method: GtWireEncodingExamples
wideSymbol
	<gtExample>
	| wideSymbol encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	wideSymbol := #'kancelař'.
	encoder nextPut: wideSymbol.
	byteArray := encoder contents.
	self assert: byteArray size equals: wideSymbol asString utf8Encoded size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(WideSymbol DoubleByteSymbol) includes: next class name).
	self assert: next equals: wideSymbol.
%

! Class implementation for 'GtWireEncodingInspectionObject'

!		Instance methods for 'GtWireEncodingInspectionObject'

category: 'accessing'
method: GtWireEncodingInspectionObject
addComponent: anObject

	components add: anObject
%

category: 'accessing'
method: GtWireEncodingInspectionObject
byteArray

	^ decoder byteArray
%

category: 'accessing'
method: GtWireEncodingInspectionObject
decoder
	^ decoder
%

category: 'accessing'
method: GtWireEncodingInspectionObject
decoder: anObject
	decoder := anObject
%

category: 'accessing'
method: GtWireEncodingInspectionObject
endIndex
	^ endIndex
%

category: 'accessing'
method: GtWireEncodingInspectionObject
endIndex: anObject
	endIndex := anObject
%

category: 'ui'
method: GtWireEncodingInspectionObject
gtChildrenFor: aView
	<gtView>

	^ aView columnedList
		  title: 'Components';
		  priority: 15;
		  items: [ components ];
		  column: 'Field' text: [ :item | item key ];
		  column: 'Value' text: [ :item | item value ];
		  send: [ :item | item value ];
		  actionUpdateButton
%

category: 'ui'
method: GtWireEncodingInspectionObject
gtHexDumpFor: aView
	<gtView>

	^ aView forward
		title: 'Buffer';
		priority: 20;
		object: [ self objectByteArray ];
		view: #gtHexDumpFor:
%

category: 'ui'
method: GtWireEncodingInspectionObject
gtSummaryFor: aView
	<gtView>

	^ aView columnedList
		  title: 'Summary';
		  priority: 10;
		  items: [ self summaryAttributes ];
		  column: #Attribute text: [ :item | item first ];
		  column: #Value text: [ :item | item second ];
		  send: [ :item | item last ];
		  actionUpdateButton
%

category: 'initialization'
method: GtWireEncodingInspectionObject
initialize

	super initialize.
	components := OrderedCollection new.
%

category: 'accessing'
method: GtWireEncodingInspectionObject
object
	^ object
%

category: 'accessing'
method: GtWireEncodingInspectionObject
object: anObject
	object := anObject
%

category: 'as yet unclassified'
method: GtWireEncodingInspectionObject
objectByteArray

	^ self byteArray copyFrom: startIndex to: endIndex
%

category: 'accessing'
method: GtWireEncodingInspectionObject
parent
	^ parent
%

category: 'accessing'
method: GtWireEncodingInspectionObject
parent: anObject
	parent := anObject
%

category: 'as yet unclassified'
method: GtWireEncodingInspectionObject
printOn: aStream

	aStream
		<< self type name;
		<< '(';
		print: object;
		<< ')'.
%

category: 'accessing'
method: GtWireEncodingInspectionObject
startIndex
	^ startIndex
%

category: 'accessing'
method: GtWireEncodingInspectionObject
startIndex: anObject
	startIndex := anObject
%

category: 'accessing'
method: GtWireEncodingInspectionObject
summaryAttributes

	^ {
		{ 'Start Index'. startIndex. }.
		{ 'Type Indicator'. self typeIndicator. }.
		{ 'Type'. self type. }.
		{ 'Object'. object. }.
	}
%

category: 'accessing'
method: GtWireEncodingInspectionObject
type

	^ decoder reverseMap at: self typeIndicator
%

category: 'accessing'
method: GtWireEncodingInspectionObject
typeIndicator
	| stream |

	stream := ReadStream on: self byteArray.
	stream position: startIndex - 1.
	^ (GtWireDecoder on: stream) stream packedInteger.
%

! Class implementation for 'GtWireNestedEncodingExamples'

!		Instance methods for 'GtWireNestedEncodingExamples'

category: 'private'
method: GtWireNestedEncodingExamples
cleanUp

	signals ifNotNil: [ signals stop ].
%

category: 'examples'
method: GtWireNestedEncodingExamples
maxDepth2
	<gtExample>
	| array currentArray encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	array := Array new: 2.
	currentArray := array.
	1 to: 5 do: [ :i |
		currentArray
			at: 1 put: i;
			at: 2 put: (Array new: 2).
		currentArray := currentArray second ].
	encoder addMapping: Array to: 
		(GtWireMaxDepthEncoder
			depth: 2
			encoder: GtWireArrayEncoder new).
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 12.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next class equals: Array.
	self assert: next first equals: 2.
	next := next second.
	self assert: next equals: (Array new: 2).
	^ byteArray
%

category: 'examples'
method: GtWireNestedEncodingExamples
minDepth2
	<gtExample>
	| array currentArray encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	array := Array new: 2.
	currentArray := array.
	1 to: 5 do: [ :i |
		currentArray
			at: 1 put: i;
			at: 2 put: (Array new: 2).
		currentArray := currentArray second ].
	encoder addMapping: Array to: 
		(GtWireMinDepthEncoder
			depth: 2
			encoder: GtWireArrayEncoder new).
	encoder 
		remainingDepth: 2;
		nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 24.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next = array.
	^ byteArray
%

category: 'examples'
method: GtWireNestedEncodingExamples
stonEncoding
	<gtExample>
	| object encoder decoder byteArray next |

	"signals := CircularMemoryLogger new startFor: GtWireEncodingSignal."
	encoder := GtWireEncoder onByteArray.
	encoder addMapping: GtWireEncodingExampleInstVarObject
		to: GtWireStonEncoder new.
	object := GtWireEncodingExampleInstVarObject new.
	object
		var1: 1;
		var2: 'two'.
	encoder nextPut: object.
	byteArray := encoder contents.
	self assert: byteArray size equals: 57.
	decoder := GtWireDecoder on: byteArray readStream.
	decoder 
		map: encoder map;
		reverseMap: encoder reverseMap.
	next := decoder next.
	self assert: next class equals: GtWireEncodingExampleInstVarObject.
	self assert: next = object.
%

! Class implementation for 'GtWireObjectEncoder'

!		Class methods for 'GtWireObjectEncoder'

category: 'accessing'
classmethod: GtWireObjectEncoder
typeIdentifier

	^ self subclassResponsibility
%

category: 'accessing'
classmethod: GtWireObjectEncoder
typeIdentifierOrNil

	^ [ self typeIdentifier ]
		on: Error
		do: [ :ex | 
			self gtDo: [ ex class name = #SubclassResponsibility ifFalse: [ ex pass ] ]
				gemstoneDo: [].
			nil ]
%

category: 'test'
classmethod: GtWireObjectEncoder
validateTypeIdentifiers
	 "Validate that there aren't duplicate type identifiers and answer the maximum value"
	| visited |
	
	visited := Set new.
	self allSubclassesDo: [ :cls | | typeIdentifier |
		typeIdentifier := cls typeIdentifierOrNil.
		typeIdentifier ifNotNil:
			[ (visited includes: typeIdentifier) ifTrue:
				[ self error: 'Duplicate typeIdentifier found' ].
			visited add: typeIdentifier ] ].
	^ visited max
%

!		Instance methods for 'GtWireObjectEncoder'

category: 'encoding - decoding'
method: GtWireObjectEncoder
decodeWith: aGtWireEncoderContext
	
	^ self subclassResponsibility
%

category: 'encoding - decoding'
method: GtWireObjectEncoder
encode: anObject with: aGtWireEncoderContext

	aGtWireEncoderContext putTypeIdentifier: self class typeIdentifier
%

category: 'as yet unclassified'
method: GtWireObjectEncoder
name

	^ self class name
%

category: 'as yet unclassified'
method: GtWireObjectEncoder
typeIdentifier

	^ self class typeIdentifier
%

! Class implementation for 'GtWireAssociationEncoder'

!		Class methods for 'GtWireAssociationEncoder'

category: 'accessing'
classmethod: GtWireAssociationEncoder
typeIdentifier

	^ 15
%

!		Instance methods for 'GtWireAssociationEncoder'

category: 'encoding - decoding'
method: GtWireAssociationEncoder
decodeWith: aGtWireEncoderContext

	^ Association
		key: aGtWireEncoderContext next
		value: aGtWireEncoderContext next
%

category: 'encoding - decoding'
method: GtWireAssociationEncoder
encode: anInteger with: aGtWireEncoderContext

	aGtWireEncoderContext putTypeIdentifier: self typeIdentifier.
	aGtWireEncoderContext
		nextPut: anInteger key;
		nextPut: anInteger value
%

! Class implementation for 'GtWireBlockClosureEncoder'

!		Class methods for 'GtWireBlockClosureEncoder'

category: 'accessing'
classmethod: GtWireBlockClosureEncoder
typeIdentifier

	^ 21
%

!		Instance methods for 'GtWireBlockClosureEncoder'

category: 'encoding - decoding'
method: GtWireBlockClosureEncoder
decodeWith: aGtWireEncoderContext

	^ self
		gtDo: [ BlockClosure evaluate: aGtWireEncoderContext next ]
		gemstoneDo: [ | bindings receiver |
			bindings := GsCurrentSession currentSession symbolList.
			receiver := self.
			aGtWireEncoderContext next evaluate.
				"_compileInContext: receiver symbolList: bindings" ].
%

category: 'encoding - decoding'
method: GtWireBlockClosureEncoder
encode: aBlockClosure with: aGtWireEncoderContext

	aBlockClosure isClean ifFalse:
		[ self error: 'BlockClosures must be clean' ].
	aGtWireEncoderContext 
		putTypeIdentifier: self class typeIdentifier;
		nextPut: (self
			gtDo: [ aBlockClosure printString ]
			gemstoneDo: [ aBlockClosure method _sourceStringForBlock  ]).
%

! Class implementation for 'GtWireBooleanEncoder'

!		Instance methods for 'GtWireBooleanEncoder'

category: 'encoding - decoding'
method: GtWireBooleanEncoder
encode: anObject with: aGtWireEncoderContext

	aGtWireEncoderContext putTypeIdentifier: (anObject
		ifTrue: [ GtWireTrueEncoder ]
		ifFalse: [ GtWireFalseEncoder ])
			typeIdentifier
%

! Class implementation for 'GtWireFalseEncoder'

!		Class methods for 'GtWireFalseEncoder'

category: 'accessing'
classmethod: GtWireFalseEncoder
typeIdentifier

	^ 3
%

!		Instance methods for 'GtWireFalseEncoder'

category: 'encoding - decoding'
method: GtWireFalseEncoder
decodeWith: aGtWireEncoderContext

	^ false
%

! Class implementation for 'GtWireTrueEncoder'

!		Class methods for 'GtWireTrueEncoder'

category: 'accessing'
classmethod: GtWireTrueEncoder
typeIdentifier

	^ 2
%

!		Instance methods for 'GtWireTrueEncoder'

category: 'encoding - decoding'
method: GtWireTrueEncoder
decodeWith: aGtWireEncoderContext

	^ true
%

! Class implementation for 'GtWireByteArrayEncoder'

!		Class methods for 'GtWireByteArrayEncoder'

category: 'accessing'
classmethod: GtWireByteArrayEncoder
typeIdentifier

	^ 4
%

!		Instance methods for 'GtWireByteArrayEncoder'

category: 'encoding - decoding'
method: GtWireByteArrayEncoder
decodeWith: aGtWireEncoderContext

	^ aGtWireEncoderContext nextByteArray
%

category: 'encoding - decoding'
method: GtWireByteArrayEncoder
encode: aByteArray with: aGtWireEncoderContext

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putByteArray: aByteArray
%

! Class implementation for 'GtWireCharacterArrayEncoder'

!		Instance methods for 'GtWireCharacterArrayEncoder'

category: 'encoding - decoding'
method: GtWireCharacterArrayEncoder
decodeWith: aGtWireEncoderContext

	^ aGtWireEncoderContext nextString
%

category: 'encoding - decoding'
method: GtWireCharacterArrayEncoder
encode: aString with: aGtWireEncoderContext

	aGtWireEncoderContext 
		putTypeIdentifier: self typeIdentifier;
		putString: aString
%

! Class implementation for 'GtWireStringEncoder'

!		Class methods for 'GtWireStringEncoder'

category: 'accessing'
classmethod: GtWireStringEncoder
typeIdentifier

	^ 5
%

! Class implementation for 'GtWireSymbolEncoder'

!		Class methods for 'GtWireSymbolEncoder'

category: 'accessing'
classmethod: GtWireSymbolEncoder
typeIdentifier

	^ 6
%

!		Instance methods for 'GtWireSymbolEncoder'

category: 'encoding - decoding'
method: GtWireSymbolEncoder
decodeWith: aGtWireEncoderContext
	
	^ (super decodeWith: aGtWireEncoderContext) asSymbol
%

! Class implementation for 'GtWireCharacterEncoder'

!		Class methods for 'GtWireCharacterEncoder'

category: 'accessing'
classmethod: GtWireCharacterEncoder
typeIdentifier

	^ 7
%

!		Instance methods for 'GtWireCharacterEncoder'

category: 'encoding - decoding'
method: GtWireCharacterEncoder
decodeWith: aGtWireEncoderContext

	^ Character value: aGtWireEncoderContext nextPackedInteger
%

category: 'encoding - decoding'
method: GtWireCharacterEncoder
encode: aCharacter with: aGtWireEncoderContext

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: aCharacter codePoint.
%

! Class implementation for 'GtWireCollectionEncoder'

!		Instance methods for 'GtWireCollectionEncoder'

category: 'encoding - decoding'
method: GtWireCollectionEncoder
decodeWith: aGtWireEncoderContext
	"Decode the array on the supplied context"
	| count |

	count := aGtWireEncoderContext nextSize.
	^ Array new: count streamContents: [ :arrayStream |
		count timesRepeat:
			[ arrayStream nextPut: aGtWireEncoderContext next ] ]
%

category: 'encoding - decoding'
method: GtWireCollectionEncoder
encode: aCollection with: aGtWireEncoderContext

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putSize: aCollection size.
	aCollection do: [ :each |
		aGtWireEncoderContext nextPut: each ].
%

! Class implementation for 'GtWireArrayEncoder'

!		Class methods for 'GtWireArrayEncoder'

category: 'accessing'
classmethod: GtWireArrayEncoder
typeIdentifier

	^ 8
%

! Class implementation for 'GtWireDictionaryEncoder'

!		Class methods for 'GtWireDictionaryEncoder'

category: 'accessing'
classmethod: GtWireDictionaryEncoder
typeIdentifier

	^ 9
%

!		Instance methods for 'GtWireDictionaryEncoder'

category: 'encoding - decoding'
method: GtWireDictionaryEncoder
decodeWith: aGtWireEncoderContext
	"Decode the dictionary on the supplied context"
	| count dictionary |

	count := aGtWireEncoderContext nextSize.
	dictionary := Dictionary new: count * 2.
	count timesRepeat:
		[ dictionary
			at: aGtWireEncoderContext next
			put: aGtWireEncoderContext next ].
	^ dictionary
%

category: 'encoding - decoding'
method: GtWireDictionaryEncoder
encode: aDictionary with: aGtWireEncoderContext

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putSize: aDictionary size.
	aDictionary associationsDo: [ :each |
		aGtWireEncoderContext 
			nextPut: each key;
			nextPut: each value ].
%

! Class implementation for 'GtWireOrderedCollectionEncoder'

!		Class methods for 'GtWireOrderedCollectionEncoder'

category: 'accessing'
classmethod: GtWireOrderedCollectionEncoder
typeIdentifier

	^ 10
%

!		Instance methods for 'GtWireOrderedCollectionEncoder'

category: 'encoding - decoding'
method: GtWireOrderedCollectionEncoder
decodeWith: aGtWireEncoderContext
	"Decode the OrderedCollection on the supplied context"
	| count |

	count := aGtWireEncoderContext nextSize.
	^ OrderedCollection new: count streamContents: [ :arrayStream |
		count timesRepeat:
			[ arrayStream nextPut: aGtWireEncoderContext next ] ]
%

! Class implementation for 'GtWireSetEncoder'

!		Class methods for 'GtWireSetEncoder'

category: 'accessing'
classmethod: GtWireSetEncoder
typeIdentifier

	^ 11
%

!		Instance methods for 'GtWireSetEncoder'

category: 'encoding - decoding'
method: GtWireSetEncoder
decodeWith: aGtWireEncoderContext
	"Decode the OrderedCollection on the supplied context"
	| count set |

	count := aGtWireEncoderContext nextSize.
	set := Set new: count * 2.
	count timesRepeat:
		[ set add: aGtWireEncoderContext next ].
	^ set
%

! Class implementation for 'GtWireDateAndTimeEncoder'

!		Class methods for 'GtWireDateAndTimeEncoder'

category: 'accessing'
classmethod: GtWireDateAndTimeEncoder
typeIdentifier

	^ 12
%

!		Instance methods for 'GtWireDateAndTimeEncoder'

category: 'encoding - decoding'
method: GtWireDateAndTimeEncoder
decodeWith: aGtWireEncoderContext
	"Decode the array on the supplied context"
	| unixSeconds nanoSeconds offset |

	unixSeconds :=  aGtWireEncoderContext nextPackedInteger.
	nanoSeconds := aGtWireEncoderContext nextPackedInteger.
	offset := aGtWireEncoderContext next.
	^ self
		gtDo: [ (DateAndTime fromUnixTime: unixSeconds)
			setNanoSeconds: nanoSeconds;
			translateTo: offset ]
		gemstoneDo: [ DateAndTime posixSeconds: (unixSeconds + (nanoSeconds / (10 raisedTo: 9))) offset: (Duration seconds: offset) ].
%

category: 'encoding - decoding'
method: GtWireDateAndTimeEncoder
encode: aDateAndTime with: aGtWireEncoderContext

	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putPackedInteger: aDateAndTime asUnixTime truncated;
		putPackedInteger: aDateAndTime nanoSecond;
		nextPut: aDateAndTime offset asSeconds.
%

! Class implementation for 'GtWireFloatEncoder'

!		Class methods for 'GtWireFloatEncoder'

category: 'accessing'
classmethod: GtWireFloatEncoder
typeIdentifier

	^ 17
%

!		Instance methods for 'GtWireFloatEncoder'

category: 'encoding - decoding'
method: GtWireFloatEncoder
decodeWith: aGtWireEncoderContext

	^ aGtWireEncoderContext nextFloat64.
%

category: 'encoding - decoding'
method: GtWireFloatEncoder
encode: aFloat with: aGtWireEncoderContext

	aGtWireEncoderContext 
		putPackedInteger: self typeIdentifier;
		putFloat64: aFloat
%

! Class implementation for 'GtWireGemStoneOopEncoder'

!		Class methods for 'GtWireGemStoneOopEncoder'

category: 'accessing'
classmethod: GtWireGemStoneOopEncoder
typeIdentifier

	^ 23
%

!		Instance methods for 'GtWireGemStoneOopEncoder'

category: 'encoding - decoding'
method: GtWireGemStoneOopEncoder
decodeWith: aGtWireEncoderContext
	"It is up to the user to ensure the Object isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	^ self
		gtDo: [ #GtGemStoneCurrentSession asClass value evaluateAndWaitReturnProxy:
			'Object objectForOop: ', aGtWireEncoderContext nextPackedInteger asString ]
		gemstoneDo: [ Object objectForOop: aGtWireEncoderContext nextPackedInteger ]
%

category: 'encoding - decoding'
method: GtWireGemStoneOopEncoder
encode: anObject with: aGtWireEncoderContext
	"It is up to the user to ensure that anObject isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	aGtWireEncoderContext 
		putTypeIdentifier: self class typeIdentifier;
		putPackedInteger: anObject asOop
%

! Class implementation for 'GtWireGemStoneRsrEncoder'

!		Class methods for 'GtWireGemStoneRsrEncoder'

category: 'accessing'
classmethod: GtWireGemStoneRsrEncoder
typeIdentifier

	^ 24
%

!		Instance methods for 'GtWireGemStoneRsrEncoder'

category: 'encoding - decoding'
method: GtWireGemStoneRsrEncoder
decodeWith: aGtWireEncoderContext
	"It is up to the user to ensure the Object isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	^ (self connection serviceAt: aGtWireEncoderContext next) asGtGsArgument.
%

category: 'encoding - decoding'
method: GtWireGemStoneRsrEncoder
encode: aRsrService with: aGtWireEncoderContext
	"It is up to the user to ensure that anObject isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	"Ensure that the service is at least registered so that it has an _id
	and has a reference in the service.
	Remaining set up will be done during snapshot analysis."

	self connection _ensureRegistered: aRsrService.
	self currentWireService addRoot: aRsrService.
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		nextPut: aRsrService _id
%

! Class implementation for 'GtWireInstVarEncoder'

!		Class methods for 'GtWireInstVarEncoder'

category: 'as yet unclassified'
classmethod: GtWireInstVarEncoder
typeIdentifier

	^ 20
%

!		Instance methods for 'GtWireInstVarEncoder'

category: 'as yet unclassified'
method: GtWireInstVarEncoder
decodeWith: aGtWireEncoderContext
	| count instance className |

	count := aGtWireEncoderContext nextSize.
	className := aGtWireEncoderContext next.
	instance := (self class environment classOrTraitNamed: className) basicNew.
	count timesRepeat:
		[ | instVarName |
		instVarName := aGtWireEncoderContext next.
		instance
			instVarNamed: instVarName
			put: aGtWireEncoderContext next ].
	^ instance
%

category: 'as yet unclassified'
method: GtWireInstVarEncoder
encode: anObject with: aGtWireEncoderContext

	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putSize: instVarMap size.
	aGtWireEncoderContext nextPut: anObject class name.
	instVarMap keysAndValuesDo: [ :key :value |
		aGtWireEncoderContext nextPut: key.
		value
			ifNil: [ aGtWireEncoderContext nextPut: (anObject instVarNamed: key) ]
			ifNotNil: [ value encode: (anObject instVarNamed: key) with: aGtWireEncoderContext ] ].
%

category: 'accessing'
method: GtWireInstVarEncoder
instVarMap
	^ instVarMap
%

category: 'accessing'
method: GtWireInstVarEncoder
instVarMap: anObject
	instVarMap := anObject
%

! Class implementation for 'GtWireIntegerEncoder'

!		Class methods for 'GtWireIntegerEncoder'

category: 'accessing'
classmethod: GtWireIntegerEncoder
typeIdentifier

	^ 16
%

!		Instance methods for 'GtWireIntegerEncoder'

category: 'encoding - decoding'
method: GtWireIntegerEncoder
encode: anInteger with: aGtWireEncoderContext

	anInteger >= 0
		ifTrue: [ GtWirePositiveIntegerEncoder new encode: anInteger with: aGtWireEncoderContext ]
		ifFalse: [ GtWireNegativeIntegerEncoder new encode: anInteger with: aGtWireEncoderContext ]
%

! Class implementation for 'GtWireNegativeIntegerEncoder'

!		Class methods for 'GtWireNegativeIntegerEncoder'

category: 'accessing'
classmethod: GtWireNegativeIntegerEncoder
typeIdentifier

	^ 14
%

!		Instance methods for 'GtWireNegativeIntegerEncoder'

category: 'encoding - decoding'
method: GtWireNegativeIntegerEncoder
decodeWith: aGtWireEncoderContext

	^ aGtWireEncoderContext nextPackedInteger negated
%

category: 'encoding - decoding'
method: GtWireNegativeIntegerEncoder
encode: anInteger with: aGtWireEncoderContext

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: anInteger negated
%

! Class implementation for 'GtWirePositiveIntegerEncoder'

!		Class methods for 'GtWirePositiveIntegerEncoder'

category: 'accessing'
classmethod: GtWirePositiveIntegerEncoder
typeIdentifier

	^ 13
%

!		Instance methods for 'GtWirePositiveIntegerEncoder'

category: 'encoding - decoding'
method: GtWirePositiveIntegerEncoder
decodeWith: aGtWireEncoderContext

	^ aGtWireEncoderContext nextPackedInteger
%

category: 'encoding - decoding'
method: GtWirePositiveIntegerEncoder
encode: anInteger with: aGtWireEncoderContext

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: anInteger
%

! Class implementation for 'GtWireMaxDepthEncoder'

!		Class methods for 'GtWireMaxDepthEncoder'

category: 'instance creation'
classmethod: GtWireMaxDepthEncoder
depth: anInteger encoder: aGtWireEncoder

	^ self new
		depth: anInteger;
		encoder: aGtWireEncoder
%

!		Instance methods for 'GtWireMaxDepthEncoder'

category: 'encoding - decoding'
method: GtWireMaxDepthEncoder
decodeWith: aGtWireEncoderContext

	self error: 'Should not be decoded'
%

category: 'accessing'
method: GtWireMaxDepthEncoder
depth
	^ depth
%

category: 'accessing'
method: GtWireMaxDepthEncoder
depth: anObject
	depth := anObject
%

category: 'encoding - decoding'
method: GtWireMaxDepthEncoder
encode: anInteger with: aGtWireEncoderContext
	| oldDepth |

	oldDepth := aGtWireEncoderContext remainingDepth.
	aGtWireEncoderContext remainingDepth: (oldDepth min: depth).
	encoder
		ifNil: [ aGtWireEncoderContext nextPut: anInteger ]
		ifNotNil: [ encoder encode: anInteger with: aGtWireEncoderContext ].
	aGtWireEncoderContext remainingDepth: oldDepth.
%

category: 'accessing'
method: GtWireMaxDepthEncoder
encoder
	^ encoder
%

category: 'accessing'
method: GtWireMaxDepthEncoder
encoder: anObject
	encoder := anObject
%

! Class implementation for 'GtWireMinDepthEncoder'

!		Class methods for 'GtWireMinDepthEncoder'

category: 'instance creation'
classmethod: GtWireMinDepthEncoder
depth: anInteger encoder: aGtWireEncoder

	^ self new
		depth: anInteger;
		encoder: aGtWireEncoder
%

!		Instance methods for 'GtWireMinDepthEncoder'

category: 'encoding - decoding'
method: GtWireMinDepthEncoder
decodeWith: aGtWireEncoderContext

	self error: 'Should not be decoded'
%

category: 'accessing'
method: GtWireMinDepthEncoder
depth
	^ depth
%

category: 'accessing'
method: GtWireMinDepthEncoder
depth: anObject
	depth := anObject
%

category: 'encoding - decoding'
method: GtWireMinDepthEncoder
encode: anObject with: aGtWireEncoderContext
	| oldDepth |

	oldDepth := aGtWireEncoderContext remainingDepth.
	aGtWireEncoderContext remainingDepth: (oldDepth max: depth).
	encoder
		ifNil: [ aGtWireEncoderContext nextPut: anObject ]
		ifNotNil: [ encoder encode: anObject with: aGtWireEncoderContext ].
	aGtWireEncoderContext remainingDepth: oldDepth.
%

category: 'accessing'
method: GtWireMinDepthEncoder
encoder
	^ encoder
%

category: 'accessing'
method: GtWireMinDepthEncoder
encoder: anObject
	encoder := anObject
%

! Class implementation for 'GtWireNilEncoder'

!		Class methods for 'GtWireNilEncoder'

category: 'accessing'
classmethod: GtWireNilEncoder
typeIdentifier

	^ 1
%

!		Instance methods for 'GtWireNilEncoder'

category: 'encoding - decoding'
method: GtWireNilEncoder
decodeWith: aGtWireEncoderContext
	
	^ nil
%

! Class implementation for 'GtWireObjectByNameEncoder'

!		Class methods for 'GtWireObjectByNameEncoder'

category: 'instance creation'
classmethod: GtWireObjectByNameEncoder
typeIdentifier

	^ 22
%

!		Instance methods for 'GtWireObjectByNameEncoder'

category: 'encoding - decoding'
method: GtWireObjectByNameEncoder
decodeWith: aGtWireEncoderContext
	| count instance className cls |

	className := aGtWireEncoderContext nextString.
	"Retrieve the number of variable slots"
	count := aGtWireEncoderContext nextSize.
	cls := self lookupClass: className.
	cls ifNil: [ self error: 'Unknown class: ', className asString ].
	instance := cls isVariable
		ifTrue: [ cls basicNew: count ]
		ifFalse: [ cls basicNew ].
	1 to: count do: [ :i |
		instance basicAt: i put: aGtWireEncoderContext next ].
	count := aGtWireEncoderContext nextSize.
	count timesRepeat:
		[ | instVarName |
		instVarName := aGtWireEncoderContext next.
		instance
			instVarNamed: instVarName
			put: aGtWireEncoderContext next ].
	^ instance
%

category: 'encoding - decoding'
method: GtWireObjectByNameEncoder
encode: anObject with: aGtWireEncoderContext
	| instVarNames namesAndValues |

	instVarNames := anObject class allInstVarNames.
	namesAndValues := OrderedCollection new.
	instVarNames do: [ :name |
		(anObject instVarNamed: name) ifNotNil: [ :value |
			namesAndValues add: name -> value ] ].
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putString: anObject class name.
	anObject class isVariable ifTrue:
		[ | basicSize |
		basicSize := anObject basicSize.
		aGtWireEncoderContext putSize: basicSize.
		1 to: basicSize do: [ :i |
			aGtWireEncoderContext nextPut: (anObject basicAt: i) ] ]
	ifFalse:
		[ aGtWireEncoderContext putSize: 0 ].
	aGtWireEncoderContext putSize: namesAndValues size.
	namesAndValues do: [ :assoc |
		aGtWireEncoderContext nextPut: assoc key.
		aGtWireEncoderContext nextPut: assoc value ].
%

category: 'private'
method: GtWireObjectByNameEncoder
lookupClass: className
	"Answer the class with the supplied name or nil if not found.
	For GemStone, see STONReader>>lookupClass: for inspiration."

	^ self
		gtDo: [ self class environment classOrTraitNamed: className ]
		gemstoneDo: [ System myUserProfile objectNamed: className asSymbol ]
%

! Class implementation for 'GtWireStonEncoder'

!		Class methods for 'GtWireStonEncoder'

category: 'accessing'
classmethod: GtWireStonEncoder
typeIdentifier

	^ 19
%

!		Instance methods for 'GtWireStonEncoder'

category: 'encoding - decoding'
method: GtWireStonEncoder
decodeWith: aGtWireEncoderContext

	^ STON fromString: aGtWireEncoderContext nextByteArray utf8Decoded.
%

category: 'encoding - decoding'
method: GtWireStonEncoder
encode: anObject with: aGtWireEncoderContext
	| stonEncoded |

	stonEncoded := (STON toString: anObject) utf8Encoded.
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putByteArray: stonEncoded.
%

! Class implementation for 'GtWireStream'

!		Class methods for 'GtWireStream'

category: 'instance creation'
classmethod: GtWireStream
on: aStream

	^ self basicNew on: aStream
%

!		Instance methods for 'GtWireStream'

category: 'accessing'
method: GtWireStream
contents

	^ wrappedStream contents
%

category: 'encoding - decoding'
method: GtWireStream
float64
	| byteArray |

	byteArray := self next: 8.
	^ byteArray doubleAt: 1.
%

category: 'encoding - decoding'
method: GtWireStream
float64: aFloat
	| byteArray |

	byteArray := ByteArray new: 8.
	byteArray doubleAt: 1 put: aFloat.
	self nextPutAll: byteArray.
%

category: 'as yet unclassified'
method: GtWireStream
int64
	"Answer the next signed, 32-bit integer from this (binary) stream."
	"Details: As a fast check for negative number, check the high bit of the first digit"
	| n firstDigit |
	n := firstDigit := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	firstDigit >= 128 ifTrue: [n := -16r10000000000000000 + n].  "decode negative 64-bit integer"
	^ n
%

category: 'as yet unclassified'
method: GtWireStream
int64: anInteger
	| n |
	(anInteger < -16r8000000000000000) | (anInteger >= 16r8000000000000000)
		ifTrue: [self error: 'outside 64-bit integer range'].

	anInteger < 0
		ifTrue: [n := 16r10000000000000000 + anInteger]
		ifFalse: [n := anInteger].
	self nextPut: (n digitAt: 8).
	self nextPut: (n digitAt: 7).
	self nextPut: (n digitAt: 6).
	self nextPut: (n digitAt: 5).
	self nextPut: (n digitAt: 4).
	self nextPut: (n digitAt: 3).
	self nextPut: (n digitAt: 2).
	self nextPut: (n digitAt: 1).
%

category: 'accessing'
method: GtWireStream
next

	^ wrappedStream next
%

category: 'accessing'
method: GtWireStream
next: anInteger
	"Answer the next anInteger number of objects accessible by the receiver."

	^ wrappedStream next: anInteger
%

category: 'accessing'
method: GtWireStream
nextPut: aByte

	wrappedStream nextPut: aByte
%

category: 'as yet unclassified'
method: GtWireStream
on: aStream

	wrappedStream := aStream
%

category: 'encoding - decoding'
method: GtWireStream
packedInteger
	| result |

	result := 0.
	[ | byte |
	byte := self next.
	result := (result bitShift: 7) + (byte bitAnd: 16r7F).
	byte >= 16r80 ] whileTrue.
	^ result
%

category: 'encoding - decoding'
method: GtWireStream
packedInteger: anInteger
	| bitCount byteCount |

	anInteger isInteger ifFalse:
		[ self error: anInteger asString, ' isn''t integer' ].
	anInteger < 0 ifTrue:
		[ self error: anInteger asString, ' is less than 0' ].
	anInteger = 0 ifTrue:
		[ self nextPut: 0.
		^ self ].

	bitCount := (anInteger log: 2) floor + 1.
	byteCount := (bitCount / 7) ceiling.
	byteCount to: 1 by: -1 do: [ :byteOffset |
		| byte |
		byte := (anInteger bitShift: (byteOffset - 1 * -7)) bitAnd: 16r7F.
		byteOffset > 1 ifTrue: [ byte := byte bitOr: 16r80 ].
		self nextPut: byte ].
%

category: 'accessing'
method: GtWireStream
position

	^ wrappedStream position
%

category: 'as yet unclassified'
method: GtWireStream
reset

	wrappedStream reset
%

! Class extensions for 'DateAndTimeANSI'

!		Instance methods for 'DateAndTimeANSI'

category: '*GToolkit-WireEncoding-GemStone'
method: DateAndTimeANSI
asUnixTime

	^ self asPosixSeconds
%

category: '*GToolkit-WireEncoding-GemStone'
method: DateAndTimeANSI
nanoSecond

	^ (self second fractionPart * (10 raisedTo: 9)) rounded
%

category: '*GToolkit-WireEncoding-GemStone'
method: DateAndTimeANSI
setNanoSeconds: nanoSeconds
	"Set the fractional seconds of the receiver"

	^ DateAndTime posixSeconds: self asPosixSeconds truncated + (nanoSeconds / (10 raisedTo: 9))
		offset: self offset
%

! Class extensions for 'GtWireDecoder'

!		Instance methods for 'GtWireDecoder'

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireDecoder
nextNullTerminatedUtf8
	| ch wStream |

	wStream := WriteStream on: (ByteArray new: 1024).
	[ (ch := stream next) = 0 ] whileFalse:
		[ wStream nextPut: ch ].
	^ wStream contents utf8Decoded asString
%

! Class extensions for 'GtWireEncoder'

!		Instance methods for 'GtWireEncoder'

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireEncoder
putNullTerminatedUtf8: aString

	stream nextPutAll: aString utf8Encoded.
	stream nextPut: 0.
%

! Class extensions for 'GtWireEncoderDecoder'

!		Class methods for 'GtWireEncoderDecoder'

category: '*GToolkit-WireEncoding-GemStone'
classmethod: GtWireEncoderDecoder
defaultMap

	^ SessionTemps current
		at: #gtGsWireEncodingDefaultMap
		ifAbsentPut: [ self getDefaultMap ]
%

category: '*GToolkit-WireEncoding-GemStone'
classmethod: GtWireEncoderDecoder
defaultMapping
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
		at: SmallDateAndTime put: GtWireDateAndTimeEncoder new.
	^ mapping
%

category: '*GToolkit-WireEncoding-GemStone'
classmethod: GtWireEncoderDecoder
defaultReverseMap

	^ SessionTemps current
		at: #gtGsWireEncodingDefaultReverseMap
		ifAbsentPut: [ self getDefaultReverseMap ]
%

category: '*GToolkit-WireEncoding-GemStone'
classmethod: GtWireEncoderDecoder
getDefaultMap
	"Generated by #generateDefaultMapMethodFrom:.
	Original source is #defaultMapping, changes should be made there and the code regenerated."

	^ IdentityDictionary new
		at: ((self lookupClass: #Array) ifNil: [ self error: 'Unable to find: Array' ]) put: GtWireArrayEncoder new;
		at: ((self lookupClass: #Association) ifNil: [ self error: 'Unable to find: Association' ]) put: GtWireAssociationEncoder new;
		at: ((self lookupClass: #Boolean) ifNil: [ self error: 'Unable to find: Boolean' ]) put: GtWireBooleanEncoder new;
		at: ((self lookupClass: #ByteArray) ifNil: [ self error: 'Unable to find: ByteArray' ]) put: GtWireByteArrayEncoder new;
		at: ((self lookupClass: #Character) ifNil: [ self error: 'Unable to find: Character' ]) put: GtWireCharacterEncoder new;
		at: ((self lookupClass: #DateAndTime) ifNil: [ self error: 'Unable to find: DateAndTime' ]) put: GtWireDateAndTimeEncoder new;
		at: ((self lookupClass: #Dictionary) ifNil: [ self error: 'Unable to find: Dictionary' ]) put: GtWireDictionaryEncoder new;
		at: ((self lookupClass: #DoubleByteString) ifNil: [ self error: 'Unable to find: DoubleByteString' ]) put: GtWireStringEncoder new;
		at: ((self lookupClass: #DoubleByteSymbol) ifNil: [ self error: 'Unable to find: DoubleByteSymbol' ]) put: GtWireSymbolEncoder new;
		at: ((self lookupClass: #ExecBlock) ifNil: [ self error: 'Unable to find: ExecBlock' ]) put: GtWireBlockClosureEncoder new;
		at: ((self lookupClass: #ExecBlock0) ifNil: [ self error: 'Unable to find: ExecBlock0' ]) put: GtWireBlockClosureEncoder new;
		at: ((self lookupClass: #ExecBlock1) ifNil: [ self error: 'Unable to find: ExecBlock1' ]) put: GtWireBlockClosureEncoder new;
		at: ((self lookupClass: #ExecBlock2) ifNil: [ self error: 'Unable to find: ExecBlock2' ]) put: GtWireBlockClosureEncoder new;
		at: ((self lookupClass: #ExecBlock3) ifNil: [ self error: 'Unable to find: ExecBlock3' ]) put: GtWireBlockClosureEncoder new;
		at: ((self lookupClass: #ExecBlock4) ifNil: [ self error: 'Unable to find: ExecBlock4' ]) put: GtWireBlockClosureEncoder new;
		at: ((self lookupClass: #ExecBlock5) ifNil: [ self error: 'Unable to find: ExecBlock5' ]) put: GtWireBlockClosureEncoder new;
		at: ((self lookupClass: #ExecBlockN) ifNil: [ self error: 'Unable to find: ExecBlockN' ]) put: GtWireBlockClosureEncoder new;
		at: ((self lookupClass: #Float) ifNil: [ self error: 'Unable to find: Float' ]) put: GtWireFloatEncoder new;
		at: ((self lookupClass: #GtRsrEvaluatorFeaturesService) ifNil: [ self error: 'Unable to find: GtRsrEvaluatorFeaturesService' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrEvaluatorFeaturesServiceServer) ifNil: [ self error: 'Unable to find: GtRsrEvaluatorFeaturesServiceServer' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrEvaluatorService) ifNil: [ self error: 'Unable to find: GtRsrEvaluatorService' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrEvaluatorServiceServer) ifNil: [ self error: 'Unable to find: GtRsrEvaluatorServiceServer' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrProxyService) ifNil: [ self error: 'Unable to find: GtRsrProxyService' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrProxyServiceServer) ifNil: [ self error: 'Unable to find: GtRsrProxyServiceServer' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrTestService) ifNil: [ self error: 'Unable to find: GtRsrTestService' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrTestServiceClient) ifNil: [ self error: 'Unable to find: GtRsrTestServiceClient' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrTestServiceServer) ifNil: [ self error: 'Unable to find: GtRsrTestServiceServer' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrWireTransferService) ifNil: [ self error: 'Unable to find: GtRsrWireTransferService' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #GtRsrWireTransferServiceServer) ifNil: [ self error: 'Unable to find: GtRsrWireTransferServiceServer' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #LargeInteger) ifNil: [ self error: 'Unable to find: LargeInteger' ]) put: GtWireIntegerEncoder new;
		at: ((self lookupClass: #OrderedCollection) ifNil: [ self error: 'Unable to find: OrderedCollection' ]) put: GtWireOrderedCollectionEncoder new;
		at: ((self lookupClass: #RsrPolicyRejectedService) ifNil: [ self error: 'Unable to find: RsrPolicyRejectedService' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #RsrPolicyRejectedServiceClient) ifNil: [ self error: 'Unable to find: RsrPolicyRejectedServiceClient' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #RsrPolicyRejectedServiceServer) ifNil: [ self error: 'Unable to find: RsrPolicyRejectedServiceServer' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #RsrReasonService) ifNil: [ self error: 'Unable to find: RsrReasonService' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #RsrRemoteException) ifNil: [ self error: 'Unable to find: RsrRemoteException' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #RsrRemoteExceptionClient) ifNil: [ self error: 'Unable to find: RsrRemoteExceptionClient' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #RsrRemoteExceptionServer) ifNil: [ self error: 'Unable to find: RsrRemoteExceptionServer' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #RsrService) ifNil: [ self error: 'Unable to find: RsrService' ]) put: GtWireGemStoneRsrEncoder new;
		at: ((self lookupClass: #Set) ifNil: [ self error: 'Unable to find: Set' ]) put: GtWireSetEncoder new;
		at: ((self lookupClass: #SmallDateAndTime) ifNil: [ self error: 'Unable to find: SmallDateAndTime' ]) put: GtWireDateAndTimeEncoder new;
		at: ((self lookupClass: #SmallDouble) ifNil: [ self error: 'Unable to find: SmallDouble' ]) put: GtWireFloatEncoder new;
		at: ((self lookupClass: #SmallInteger) ifNil: [ self error: 'Unable to find: SmallInteger' ]) put: GtWireIntegerEncoder new;
		at: ((self lookupClass: #String) ifNil: [ self error: 'Unable to find: String' ]) put: GtWireStringEncoder new;
		at: ((self lookupClass: #Symbol) ifNil: [ self error: 'Unable to find: Symbol' ]) put: GtWireSymbolEncoder new;
		at: ((self lookupClass: #UndefinedObject) ifNil: [ self error: 'Unable to find: UndefinedObject' ]) put: GtWireNilEncoder new;
		at: ((self lookupClass: #Unicode16) ifNil: [ self error: 'Unable to find: Unicode16' ]) put: GtWireStringEncoder new;
		at: ((self lookupClass: #Unicode32) ifNil: [ self error: 'Unable to find: Unicode32' ]) put: GtWireStringEncoder new;
		at: ((self lookupClass: #Unicode7) ifNil: [ self error: 'Unable to find: Unicode7' ]) put: GtWireStringEncoder new;
		yourself.
%

category: '*GToolkit-WireEncoding-GemStone'
classmethod: GtWireEncoderDecoder
getDefaultReverseMap
	"Generated by #generateDefaultReverseMapMethodFrom:.
	Original source is #defaultMapping, changes should be made there and the code regenerated."

	^ IdentityDictionary new
		at: 1 put: GtWireNilEncoder new;
		at: 2 put: GtWireTrueEncoder new;
		at: 3 put: GtWireFalseEncoder new;
		at: 4 put: GtWireByteArrayEncoder new;
		at: 5 put: GtWireStringEncoder new;
		at: 6 put: GtWireSymbolEncoder new;
		at: 7 put: GtWireCharacterEncoder new;
		at: 8 put: GtWireArrayEncoder new;
		at: 9 put: GtWireDictionaryEncoder new;
		at: 10 put: GtWireOrderedCollectionEncoder new;
		at: 11 put: GtWireSetEncoder new;
		at: 12 put: GtWireDateAndTimeEncoder new;
		at: 13 put: GtWirePositiveIntegerEncoder new;
		at: 14 put: GtWireNegativeIntegerEncoder new;
		at: 15 put: GtWireAssociationEncoder new;
		at: 16 put: GtWireIntegerEncoder new;
		at: 17 put: GtWireFloatEncoder new;
		at: 19 put: GtWireStonEncoder new;
		at: 20 put: GtWireInstVarEncoder new;
		at: 21 put: GtWireBlockClosureEncoder new;
		at: 22 put: GtWireObjectByNameEncoder new;
		at: 23 put: GtWireGemStoneOopEncoder new;
		at: 24 put: GtWireGemStoneRsrEncoder new;
		yourself.
%

! Class extensions for 'GtWireEncodingExamples'

!		Instance methods for 'GtWireEncodingExamples'

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireEncodingExamples
assert: aBoolean

	self
		assert: aBoolean
		description: 'Assertion failed'.
%

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireEncodingExamples
assert: aBoolean description: aString

	aBoolean == true ifFalse:
		[ TestResult failure signal: aString value ]
%

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireEncodingExamples
assert: actual equals: expected

	self
		assert: actual = expected
		description: actual printString, ' is not equal to ', expected printString.
%

! Class extensions for 'GtWireGemStoneRsrEncoder'

!		Instance methods for 'GtWireGemStoneRsrEncoder'

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireGemStoneRsrEncoder
connection

	^ (SessionTemps current at: #GtRsrServer) connection
%

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireGemStoneRsrEncoder
currentWireService

	^ SessionTemps current at: #GtRsrCurrentWireService
%

! Class extensions for 'GtWireNestedEncodingExamples'

!		Instance methods for 'GtWireNestedEncodingExamples'

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireNestedEncodingExamples
assert: aBoolean

	self
		assert: aBoolean
		description: 'Assertion failed'.
%

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireNestedEncodingExamples
assert: aBoolean description: aString

	aBoolean == true ifFalse:
		[ TestResult failure signal: aString value ]
%

category: '*GToolkit-WireEncoding-GemStone'
method: GtWireNestedEncodingExamples
assert: actual equals: expected

	self
		assert: actual = expected
		description: actual printString, ' is not equal to ', expected printString.
%

! Class Initialization

run
GtWireEncoderDecoder initialize.
true
%
