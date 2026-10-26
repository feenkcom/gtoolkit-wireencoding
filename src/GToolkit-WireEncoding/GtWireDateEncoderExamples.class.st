"
Examples for Date wire payloads, offset preservation, and timezone compatibility checks.
"
Class {
	#name : #GtWireDateEncoderExamples,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding-Examples'
}

{ #category : #examples }
GtWireDateEncoderExamples >> calendarBoundaries [
	"Calendar components, including century leap rules and year boundaries, survive round trips."
	<gtExample>
	^ #(#(1900 2 28) #(1900 3 1) #(2000 2 29) #(2023 12 31) #(2024 1 1) #(2024 2 29) #(2024 7 1)) collect: [ :parts |
		| date encoder decoder decoded |
		date := self
			gtDo: [ (DateAndTime year: parts first month: parts second day: parts third hour: 0 minute: 0 second: 0) asDate ]
			gemstoneDo: [ Date newDay: parts third monthNumber: parts second year: parts first ].
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: date; nextPut: #sentinel.
		decoder := GtWireDecoder on: encoder contents readStream.
		decoded := decoder next.
		self assert: (decoded isKindOf: Date).
		self assert: decoded year equals: parts first.
		self assert: decoded monthIndex equals: parts second.
		self assert: decoded dayOfMonth equals: parts third.
		self assert: decoded equals: date.
		self gtDo: [ self assert: decoded offset equals: date offset ] gemstoneDo: [ ].
		self assert: decoder next equals: #sentinel.
		decoded ]

]

{ #category : #examples }
GtWireDateEncoderExamples >> decodesDifferentOffset [
	"GT preserves a foreign offset; GemStone must reject it rather than silently lose it."
	<gtExample>
	| systemOffset offset decoder result |
	systemOffset := GtWireDateEncoder new systemOffsetForYear: 2024 month: 2 day: 29.
	offset := systemOffset = 19800 ifTrue: [ -12600 ] ifFalse: [ 19800 ].
	decoder := GtWireDecoder on: (self payloadForYear: 2024 month: 2 day: 29 offset: offset) readStream.
	self
		gtDo: [
			result := decoder next.
			self assert: (result isKindOf: Date).
			self assert: result year equals: 2024.
			self assert: result monthIndex equals: 2.
			self assert: result dayOfMonth equals: 29.
			self assert: result offset asSeconds equals: offset ]
		gemstoneDo: [
			result := nil.
			[ decoder next ] on: Error do: [ :error | result := error ].
			self assert: result notNil.
			self assert: result messageText equals: 'Date UTC offset ', offset printString,
				' does not match the system timezone offset ', systemOffset printString ].
	self assert: decoder next equals: #sentinel.
	^ result
]

{ #category : #examples }
GtWireDateEncoderExamples >> decodesMatchingOffset [
	"A system-compatible payload is accepted on both platforms and consumes exactly one object."
	<gtExample>
	| offset decoder date |
	offset := GtWireDateEncoder new systemOffsetForYear: 2024 month: 2 day: 29.
	decoder := GtWireDecoder on: (self payloadForYear: 2024 month: 2 day: 29 offset: offset) readStream.
	date := decoder next.
	self assert: (date isKindOf: Date).
	self assert: date year equals: 2024.
	self assert: date monthIndex equals: 2.
	self assert: date dayOfMonth equals: 29.
	self gtDo: [ self assert: date offset asSeconds equals: offset ] gemstoneDo: [ ].
	self assert: decoder next equals: #sentinel.
	^ date
]

{ #category : #examples }
GtWireDateEncoderExamples >> nestedDates [
	"Date dispatch works inside collections, without confusing Dates with DateAndTimes."
	<gtExample>
	| dates encoder decoder decoded |
	dates := self calendarBoundaries.
	encoder := GtWireEncoder onByteArray.
	encoder nextPut: { dates. dates first. #sentinel }.
	decoder := GtWireDecoder on: encoder contents readStream.
	decoded := decoder next.
	self assert: decoded first equals: dates.
	self assert: decoded second equals: dates first.
	self assert: (decoded second isKindOf: Date).
	self assert: decoded third equals: #sentinel.
	decoded first do: [ :date | self assert: (date isKindOf: Date) ].
	^ decoded

]

{ #category : #examples }
GtWireDateEncoderExamples >> offsetValidation [
	"Exercise the GemStone guard on dates in different seasons, without changing the timezone."
	<gtExample>
	| encoder |
	encoder := GtWireDateEncoder new.
	#(#(2024 2 29) #(2024 7 1) #(2023 12 31)) do: [ :parts |
		| offset caught |
		offset := encoder systemOffsetForYear: parts first month: parts second day: parts third.
		encoder validateOffset: offset forYear: parts first month: parts second day: parts third.
		caught := nil.
		[ encoder validateOffset: offset + 1 forYear: parts first month: parts second day: parts third ]
			on: Error do: [ :error | caught := error ].
		self assert: caught notNil.
		self assert: caught messageText equals: 'Date UTC offset ', (offset + 1) printString,
			' does not match the system timezone offset ', offset printString ].
	^ encoder
]

{ #category : #private }
GtWireDateEncoderExamples >> payloadForYear: year month: month day: day offset: offset [
	"Construct an independent Date payload followed by another object to detect over-reading."
	| encoder |
	encoder := GtWireEncoder onByteArray.
	encoder
		putTypeIdentifier: GtWireDateEncoder typeIdentifier;
		putPackedInteger: year;
		putPackedInteger: month;
		putPackedInteger: day;
		nextPut: offset;
		nextPut: #sentinel.
	^ encoder contents
]

{ #category : #examples }
GtWireDateEncoderExamples >> roundTrips [
	<gtExample>
	| dates |
	GtWireEncoderDecoder reset.
	GtWireObjectEncoder validateTypeIdentifiers.
	dates := self
		gtDo: [ #(0 19800 -12600) collect: [ :offset |
			(DateAndTime year: 2024 month: 2 day: 29 hour: 0 minute: 0 second: 0 offset: (Duration seconds: offset)) asDate ] ]
		gemstoneDo: [ { Date newDay: 29 monthNumber: 2 year: 2024. Date newDay: 31 monthNumber: 12 year: 2023 } ].
	^ dates collect: [ :date | | encoder decoded |
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: date.
		decoded := (GtWireDecoder on: encoder contents readStream) next.
		self assert: (decoded isKindOf: Date).
		self assert: decoded equals: date.
		self gtDo: [ self assert: decoded offset equals: date offset ] gemstoneDo: [ ].
		decoded ]
]

{ #category : #examples }
GtWireDateEncoderExamples >> wireFormat [
	<gtExample>
	| date encoder decoder offset |
	GtWireEncoderDecoder reset.
	date := self
		gtDo: [ (DateAndTime year: 2024 month: 2 day: 29 hour: 0 minute: 0 second: 0 offset: (Duration seconds: -12600)) asDate ]
		gemstoneDo: [ Date newDay: 29 monthNumber: 2 year: 2024 ].
	offset := self gtDo: [ -12600 ] gemstoneDo: [ GtWireDateEncoder new systemOffsetForYear: 2024 month: 2 day: 29 ].
	encoder := GtWireEncoder onByteArray.
	encoder nextPut: date; nextPut: #sentinel.
	self assert: encoder contents first equals: GtWireDateEncoder typeIdentifier.
	decoder := GtWireDecoder on: encoder contents allButFirst readStream.
	self assert: decoder nextPackedInteger equals: 2024.
	self assert: decoder nextPackedInteger equals: 2.
	self assert: decoder nextPackedInteger equals: 29.
	self assert: decoder next equals: offset.
	self assert: decoder next equals: #sentinel.
	^ encoder contents
]
