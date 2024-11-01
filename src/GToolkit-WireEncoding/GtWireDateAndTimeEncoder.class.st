Class {
	#name : #GtWireDateAndTimeEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding-ObjectEncoder'
}

{ #category : #'encoding - decoding' }
GtWireDateAndTimeEncoder class >> decodeWith: aGtWireEncoderContext [
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
GtWireDateAndTimeEncoder class >> encode: aDateAndTime with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier;
		packedInteger: aDateAndTime year;
		packedInteger: aDateAndTime month;
		packedInteger: aDateAndTime dayOfMonth;
		packedInteger: aDateAndTime hour;
		packedInteger: aDateAndTime minute;
		packedInteger: aDateAndTime second;
		packedInteger: aDateAndTime nanoSecond;
		int64: aDateAndTime offset asSeconds.
]

{ #category : #accessing }
GtWireDateAndTimeEncoder class >> typeIdentifier [

	^ 12
]
