"
Regression examples for DateAndTime wire decoding: preserve the instant, nanoseconds and explicit UTC offset.
"
Class {
	#name : #GtWireDateAndTimeEncoderExamples,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding-Examples'
}

{ #category : #examples }
GtWireDateAndTimeEncoderExamples >> roundTrips [
	<gtExample>
	| dates |
	GtWireEncoderDecoder reset.
	dates := #(0 19800 -12600) collect: [ :offset |
		self gtDo:
			[ DateAndTime year: 2020 month: 1 day: 2 hour: 3 minute: 4 second: 5 nanoSecond: 123456000 offset: (Duration seconds: offset) ]
		gemstoneDo:
			[ DateAndTime year: 2020 month: 1 day: 2 hour: 3 minute: 4 second: 5 offset: (Duration seconds: offset) ] ].
	^ dates collect: [ :date | | encoder decoded |
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: date; nextPut: #sentinel.
		decoded := GtWireDecoder on: encoder contents readStream.
		[ | result |
		result := decoded next.
		self assert: result equals: date.
		self assert: result offset equals: date offset.
		self assert: result nanoSecond equals: date nanoSecond.
		self assert: decoded next equals: #sentinel.
		result ] value ]
]
