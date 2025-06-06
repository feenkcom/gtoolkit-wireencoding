Extension { #name : #GtWireEncoderDecoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoderDecoder class >> defaultMap [

	^ GtDefaultMap ifNil: [ GtDefaultMap := self getDefaultMap ].
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoderDecoder class >> defaultMapping [
	"Generate the default mapping.  This is then normally saved to #defaultMap, which is much faster to run
	(particularly on GemStone)"
	| mapping |

	mapping := IdentityDictionary new.
	self map: RsrService withSubclassesTo: GtWireGemStoneRsrEncoder new in: mapping.
	mapping
		at: Association put: GtWireAssociationEncoder new;
		at: False put: GtWireFalseEncoder new;
		at: True put: GtWireTrueEncoder new;
		at: Bitmap put: GtWireBitmapEncoder new;
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

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoderDecoder class >> defaultReverseMap [

	^ GtDefaultReverseMap ifNil: [ GtDefaultReverseMap := self getDefaultReverseMap ].
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoderDecoder class >> getDefaultMap [
	"Generated by #generateDefaultMapMethodFrom:.
	Original source is #defaultMapping, changes should be made there and the code regenerated."

	^ IdentityDictionary new
		at: Array put: GtWireArrayEncoder new;
		at: Association put: GtWireAssociationEncoder new;
		at: Bitmap put: GtWireBitmapEncoder new;
		at: BoxedFloat64 put: GtWireFloatEncoder new;
		at: ByteArray put: GtWireByteArrayEncoder new;
		at: ByteString put: GtWireStringEncoder new;
		at: ByteSymbol put: GtWireSymbolEncoder new;
		at: Character put: GtWireCharacterEncoder new;
		at: ConstantBlockClosure put: GtWireBlockClosureEncoder new;
		at: DateAndTime put: GtWireDateAndTimeEncoder new;
		at: Dictionary put: GtWireDictionaryEncoder new;
		at: False put: GtWireFalseEncoder new;
		at: FullBlockClosure put: GtWireBlockClosureEncoder new;
		at: GspoByteArray put: GtWireGemStoneRsrEncoder new;
		at: GspoDictionary put: GtWireGemStoneRsrEncoder new;
		at: GspoGtRemotePhlowDeclarativeTestForCustomProxyInspectable put: GtWireGemStoneRsrEncoder new;
		at: GspoGtRemotePhlowDeclarativeTestInspectable put: GtWireGemStoneRsrEncoder new;
		at: GspoGtRmGeoGpsRecord put: GtWireGemStoneRsrEncoder new;
		at: GspoGtRmGeoGpsTrajectory put: GtWireGemStoneRsrEncoder new;
		at: GspoGtRmGeoUser put: GtWireGemStoneRsrEncoder new;
		at: GspoGtRmGeolife put: GtWireGemStoneRsrEncoder new;
		at: GspoRowanProjectService put: GtWireGemStoneRsrEncoder new;
		at: GspoSmallFraction put: GtWireGemStoneRsrEncoder new;
		at: GspoString put: GtWireGemStoneRsrEncoder new;
		at: GtRsrEvaluatorFeaturesService put: GtWireGemStoneRsrEncoder new;
		at: GtRsrEvaluatorFeaturesServiceClient put: GtWireGemStoneRsrEncoder new;
		at: GtRsrEvaluatorService put: GtWireGemStoneRsrEncoder new;
		at: GtRsrEvaluatorServiceClient put: GtWireGemStoneRsrEncoder new;
		at: GtRsrProxyService put: GtWireGemStoneRsrEncoder new;
		at: GtRsrProxyServiceClient put: GtWireGemStoneRsrEncoder new;
		at: GtRsrTestService put: GtWireGemStoneRsrEncoder new;
		at: GtRsrTestServiceClient put: GtWireGemStoneRsrEncoder new;
		at: GtRsrTestServiceServer put: GtWireGemStoneRsrEncoder new;
		at: GtRsrWireTransferService put: GtWireGemStoneRsrEncoder new;
		at: GtRsrWireTransferServiceClient put: GtWireGemStoneRsrEncoder new;
		at: GtpoBehavior put: GtWireGemStoneRsrEncoder new;
		at: GtpoBlockClosure put: GtWireGemStoneRsrEncoder new;
		at: GtpoClass put: GtWireGemStoneRsrEncoder new;
		at: GtpoDateTime put: GtWireGemStoneRsrEncoder new;
		at: GtpoExecBlock put: GtWireGemStoneRsrEncoder new;
		at: GtpoGsNMethod put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtGemStoneEvaluationContext put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtGemStoneRPackage put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowColumnedListViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowColumnedTreeViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowForwardViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowListViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowTextEditorViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowTextViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowTextualViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowTreeViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtPhlowViewSpecification put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtRemotePhlowDeclarativeBlockActionDataSource put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtRemotePhlowSequenceableCollectionIterator put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtRemotePhlowSpawnObjectWrapper put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtRemotePhlowViewedObject put: GtWireGemStoneRsrEncoder new;
		at: GtpoGtRsrEvaluationExceptionInformation put: GtWireGemStoneRsrEncoder new;
		at: GtpoMetaclass3 put: GtWireGemStoneRsrEncoder new;
		at: GtpoModule put: GtWireGemStoneRsrEncoder new;
		at: LargeNegativeInteger put: GtWireNegativeIntegerEncoder new;
		at: LargePositiveInteger put: GtWirePositiveIntegerEncoder new;
		at: OrderedCollection put: GtWireOrderedCollectionEncoder new;
		at: RsrClientNoInstVars put: GtWireGemStoneRsrEncoder new;
		at: RsrClientReferenceService put: GtWireGemStoneRsrEncoder new;
		at: RsrClientTestService put: GtWireGemStoneRsrEncoder new;
		at: RsrConcurrentTestClient put: GtWireGemStoneRsrEncoder new;
		at: RsrConcurrentTestServer put: GtWireGemStoneRsrEncoder new;
		at: RsrConcurrentTestService put: GtWireGemStoneRsrEncoder new;
		at: RsrInstrumentedClient put: GtWireGemStoneRsrEncoder new;
		at: RsrInstrumentedServer put: GtWireGemStoneRsrEncoder new;
		at: RsrInstrumentedService put: GtWireGemStoneRsrEncoder new;
		at: RsrKnownClient put: GtWireGemStoneRsrEncoder new;
		at: RsrKnownServer put: GtWireGemStoneRsrEncoder new;
		at: RsrMockClient put: GtWireGemStoneRsrEncoder new;
		at: RsrMockServer put: GtWireGemStoneRsrEncoder new;
		at: RsrMockService put: GtWireGemStoneRsrEncoder new;
		at: RsrPolicyRejectedService put: GtWireGemStoneRsrEncoder new;
		at: RsrPolicyRejectedServiceClient put: GtWireGemStoneRsrEncoder new;
		at: RsrPolicyRejectedServiceServer put: GtWireGemStoneRsrEncoder new;
		at: RsrReasonService put: GtWireGemStoneRsrEncoder new;
		at: RsrReflectedVariableTestClient put: GtWireGemStoneRsrEncoder new;
		at: RsrReflectedVariableTestServer put: GtWireGemStoneRsrEncoder new;
		at: RsrReflectedVariableTestServiceA put: GtWireGemStoneRsrEncoder new;
		at: RsrReflectedVariableTestServiceB put: GtWireGemStoneRsrEncoder new;
		at: RsrRemoteAction put: GtWireGemStoneRsrEncoder new;
		at: RsrRemoteActionClient put: GtWireGemStoneRsrEncoder new;
		at: RsrRemoteActionServer put: GtWireGemStoneRsrEncoder new;
		at: RsrRemoteException put: GtWireGemStoneRsrEncoder new;
		at: RsrRemoteExceptionClient put: GtWireGemStoneRsrEncoder new;
		at: RsrRemoteExceptionServer put: GtWireGemStoneRsrEncoder new;
		at: RsrReturnUnknownService put: GtWireGemStoneRsrEncoder new;
		at: RsrSendUnknownService put: GtWireGemStoneRsrEncoder new;
		at: RsrServerNoInstVars put: GtWireGemStoneRsrEncoder new;
		at: RsrServerReferenceService put: GtWireGemStoneRsrEncoder new;
		at: RsrServerTestService put: GtWireGemStoneRsrEncoder new;
		at: RsrService put: GtWireGemStoneRsrEncoder new;
		at: RsrServiceNoInstVars put: GtWireGemStoneRsrEncoder new;
		at: RsrServiceReferenceService put: GtWireGemStoneRsrEncoder new;
		at: RsrTestService put: GtWireGemStoneRsrEncoder new;
		at: RsrValueHolder put: GtWireGemStoneRsrEncoder new;
		at: RsrValueHolderClient put: GtWireGemStoneRsrEncoder new;
		at: RsrValueHolderServer put: GtWireGemStoneRsrEncoder new;
		at: Set put: GtWireSetEncoder new;
		at: SmallFloat64 put: GtWireFloatEncoder new;
		at: SmallInteger put: GtWireIntegerEncoder new;
		at: True put: GtWireTrueEncoder new;
		at: UndefinedObject put: GtWireNilEncoder new;
		at: WideString put: GtWireStringEncoder new;
		at: WideSymbol put: GtWireSymbolEncoder new;
		yourself.

]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoderDecoder class >> getDefaultReverseMap [
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
		at: 18 put: GtWireLeJsonV4Encoder new;
		at: 19 put: GtWireStonEncoder new;
		at: 20 put: GtWireInstVarEncoder new;
		at: 21 put: GtWireBlockClosureEncoder new;
		at: 22 put: GtWireObjectByNameEncoder new;
		at: 23 put: GtWireGemStoneOopEncoder new;
		at: 24 put: GtWireGemStoneRsrEncoder new;
		at: 25 put: GtWireDummyProxyEncoder new;
		at: 26 put: GtWireBitmapEncoder new;
		yourself.

]
