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
