Class {
	#name : #GtWireDateAndTimeEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireDateAndTimeEncoder class >> typeIdentifier [

	^ 12
]

{ #category : #'encoding - decoding' }
GtWireDateAndTimeEncoder >> decodeWith: aGtWireEncoderContext [
	"Decode the array on the supplied context"
	| stream |

	stream := aGtWireEncoderContext stream.
	^ DateAndTime
		basicYear: stream packedInteger
		month: stream packedInteger
		day: stream packedInteger
		hour: stream packedInteger
		minute: stream packedInteger
		second: stream packedInteger
		nanoSecond: stream packedInteger
		offset: stream int64 seconds
]

{ #category : #'encoding - decoding' }
GtWireDateAndTimeEncoder >> encode: aDateAndTime with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: aDateAndTime year;
		putPackedInteger: aDateAndTime month;
		putPackedInteger: aDateAndTime dayOfMonth;
		putPackedInteger: aDateAndTime hour;
		putPackedInteger: aDateAndTime minute;
		putPackedInteger: aDateAndTime second;
		putPackedInteger: aDateAndTime nanoSecond;
		putInt64: aDateAndTime offset asSeconds.
]
