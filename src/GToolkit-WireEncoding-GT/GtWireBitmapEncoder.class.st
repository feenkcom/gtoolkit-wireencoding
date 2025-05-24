Class {
	#name : #GtWireBitmapEncoder,
	#superclass : #GtWireCollectionEncoder,
	#category : #'GToolkit-WireEncoding-GT'
}

{ #category : #accessing }
GtWireBitmapEncoder class >> typeIdentifier [

	^ 26
]

{ #category : #'encoding - decoding' }
GtWireBitmapEncoder >> decodeWith: aGtWireEncoderContext [

	^ Bitmap decompressFromByteArray:aGtWireEncoderContext nextByteArray
]

{ #category : #'encoding - decoding' }
GtWireBitmapEncoder >> encode: aBitmap with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putByteArray: aBitmap compressToByteArray
]
