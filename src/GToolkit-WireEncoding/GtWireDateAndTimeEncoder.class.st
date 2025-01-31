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

	^ DateAndTime
		basicYear: aGtWireEncoderContext nextPackedInteger
		month: aGtWireEncoderContext nextPackedInteger
		day: aGtWireEncoderContext nextPackedInteger
		hour: aGtWireEncoderContext nextPackedInteger
		minute: aGtWireEncoderContext nextPackedInteger
		second: aGtWireEncoderContext nextPackedInteger
		nanoSecond: aGtWireEncoderContext nextPackedInteger
		offset: aGtWireEncoderContext nextInt64 seconds
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
