Extension { #name : #GtWireEncodingExamples }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncodingExamples >> bitmap [
	<gtExample>
	<return: #GtWireEncodingExamples>
	| form source encoder byteArray next |
	encoder := GtWireEncoder onByteArray.
	form := Form extent: 100@100 depth: 32.
	form getCanvas fillRectangle: (0@0 corner: 30@30)  color: (Color red).
	form getCanvas fillRectangle: (10@10 corner: 40@40)  color: (Color green).
	form getCanvas fillRectangle: (20@20 corner: 50@50)  color: (Color blue).
	source := form bits.
	encoder nextPut: source.
	byteArray := encoder contents.
	self assert: byteArray size equals: source compressToByteArray size + 3.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Bitmap.
	self assert: next equals: source
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncodingExamples >> rsrProxy [
	<gtExample>
	<return: #GtWireEncodingExamples>
	| object encoder rsrConnection wireService session byteArray next root |

	encoder := GtWireEncoder onByteArray.
	"Configure the encoder to use the fallback GemStone encoder 
	and remove the default map to force the fallback to be used."
	encoder defaultEncoder: GtWireEncoder gemStoneEncoder.
	encoder map removeKey: GtRsrProxyServiceClient.
	rsrConnection := RsrConnection
			channel: RsrNullChannel new
			transactionSpigot: RsrThreadSafeNumericSpigot naturals
			oidSpigot: RsrThreadSafeNumericSpigot naturals.
	session := GtGemStoneSession new.
	session connection: rsrConnection.
	object := GtRsrProxyServiceClient new.
	object
		remoteClass: self class name;
		gtSession: session;
		_id: 42 connection: rsrConnection.
	wireService := GtRsrWireTransferServiceClient new
		_id: 41 connection: rsrConnection;
		encoder: encoder.
	GtRsrCurrentWireService
		value: wireService
		during: [ GtGemStoneCurrentSession
			value: session
			during: [ wireService bufferObject: object ] ].
	rsrConnection _register: object as: 42.
	byteArray := wireService buffer.
	self assert: byteArray size equals: 3.
	root := GtRsrCurrentWireService
		value: wireService
		during: [ GtGemStoneCurrentSession
			value: session
			during: [ GtWireInspectionDecoder byteArray: byteArray ] ].
	next := root object.
	self assert: next class equals: GtRsrProxyServiceClient.
	self assert: next _id equals: 42.
	self assert: next remoteClass equals: self class name.
]
