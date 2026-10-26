"
Encodes a Date as year, numeric month, day of month, and UTC offset in seconds. GT preserves the supplied offset. GemStone uses the system timezone offset at midnight on the supplied date and rejects incompatible offsets.
"
Class {
	#name : #GtWireDateEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireDateEncoder class >> typeIdentifier [
	^ 37
]

{ #category : #encoding }
GtWireDateEncoder >> decodeWith: aContext [
	| year month day offset |
	year := aContext nextPackedInteger.
	month := aContext nextPackedInteger.
	day := aContext nextPackedInteger.
	offset := aContext next.
	^ self
		gtDo: [ (DateAndTime year: year month: month day: day hour: 0 minute: 0 second: 0 offset: (Duration seconds: offset)) asDate ]
		gemstoneDo: [
			self validateOffset: offset forYear: year month: month day: day.
			Date newDay: day monthNumber: month year: year ]
]

{ #category : #encoding }
GtWireDateEncoder >> encode: aDate with: aContext [
	| offset |
	offset := self
		gtDo: [ aDate offset asSeconds ]
		gemstoneDo: [ self systemOffsetForYear: aDate year month: aDate monthIndex day: aDate dayOfMonth ].
	aContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: aDate year;
		putPackedInteger: aDate monthIndex;
		putPackedInteger: aDate dayOfMonth;
		nextPut: offset
]

{ #category : #private }
GtWireDateEncoder >> systemOffsetForYear: year month: month day: day [
	"Resolve the system timezone at this date, not at today's date (DST may differ)."
	^ (DateAndTime year: year month: month day: day hour: 0 minute: 0 second: 0) offset asSeconds
]

{ #category : #private }
GtWireDateEncoder >> validateOffset: offset forYear: year month: month day: day [
	| systemOffset |
	systemOffset := self systemOffsetForYear: year month: month day: day.
	offset = systemOffset ifFalse: [
		self error: 'Date UTC offset ', offset printString,
			' does not match the system timezone offset ', systemOffset printString ]
]
