"
I am a simple dialog that presents a question and waits for a yes or no answer.

escape closes the dialog without any result.
return selects the current button, or you can click on the button.
tab cycles between the buttons.
"
Class {
	#name : 'ZagImageEncoderQuestion',
	#superclass : 'ProceedDialogWindow',
	#instVars : [
		'choice'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'instance creation' }
ZagImageEncoderQuestion class >> choiceWithPrompt: title text: text [
	"self choiceWithPrompt: 'Foo is the question'"

	" returns true for yes, false for no, nil for escape"

	| pd |
	pd := (self newWithTheme: self currentWorld theme)
		      title: title asString;
		      text: text.
	"World may be not the best choice because a window may want to be in controlaList
	this point should be investigated based on UIManager default modalMorph"
	^ pd openModal choice
]

{ #category : 'accessing' }
ZagImageEncoderQuestion >> choice [
	^ choice
]

{ #category : 'accessing' }
ZagImageEncoderQuestion >> newButtons [
	"Answer new buttons as appropriate."

	^ {
		  self newNoButton.
		  self newYesButton }
]

{ #category : 'accessing' }
ZagImageEncoderQuestion >> no [

	choice := false.
	self close
]

{ #category : 'actions' }
ZagImageEncoderQuestion >> returnPressed [

	self halt
]

{ #category : 'accessing' }
ZagImageEncoderQuestion >> yes [

	choice := true.
	self close
]
