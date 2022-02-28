TITLE Project 6: String Primitives and Macros     (Proj6_bearan.asm)

; Author: Andrew Bear
; Last Modified: 27Feb2022
; OSU email address: bearan@oregonstate.edu
; Course number/section:   CS271 Section 406
; Project Number: 6                Due Date: 13Mar2022
; Description:	This program centers around the creation of two procedures - ReadVal and WriteVal 
; that mimic the Irvine Library ReadInt and WriteInt, using macros and string primitives to achieve 
; this functionality without using any int- or dec-writing irvine prodecures. 

INCLUDE Irvine32.inc

; (insert macro definitions here)

;---------------------------------------------------------------------------------------------
; Name: mGetString
;
; Outputs prompt, and calls Irvine ReadString to read response into memory
;
; Receives: prompt -- OFFSET of prompt to display
;			targetLoc -- output location 
;			maxLength -- Length of user input (required for Irvine ReadString)
; Returns: screen output of string
;----------------------------------------------------------------------------------------------
mGetString MACRO prompt, targetLoc, maxLength
	PUSH	EAX
	PUSH	ECX						; preserve registers
	PUSH	EDX

;	MOV		EDX, prompt				; standard WriteString call for prompt
;	CALL	WriteString
	mDisplayString prompt

; set up and call ReadString
	MOV		EDX, targetLoc			; set up and call ReadString
	MOV		ECX, maxLength
	CALL	ReadString


	POP		EDX
	POP		ECX
	POP		EAX
ENDM


;---------------------------------------------------------------------------------------------
; Name: mDisplayString
;
; Calls WriteString on the supplied address
;
; Receives: OFFSET of string to display
; Returns: screen output of string
;----------------------------------------------------------------------------------------------
mDisplayString MACRO stringOffset		; param - offset of string to print
	PUSH	EDX							; uses EDX so we protect it

	MOV		EDX, stringOffset			; standard writeString call
	CALL	WriteString

	POP		EDX
ENDM

; (insert constant definitions here)

ARRAYLENGTH		=	5
MAXCHARS		=	11					; a SDWORD int can only be 11 chars long, -2,147,483,648, plus padding 0


.data

; greeting/program/exit prompts
greeting	BYTE	"Project 6: String Primitives and Macros.",13,10
			BYTE	"Programmed by A. Bear",13,10,13,10
			BYTE	"**EC: Numbers the entry lines and displays a running sum.",13,10
			BYTE	"**EC: HAHAHAHAAH MAYBE IN ANOTHER LIFE.",13,10,13,10
			BYTE	"This program will ask you for 10 signed decimal integers.",13,10
			BYTE	"Each integer must be small enough to fit into a 32-bit register (-2,147,483,648 to 2,147,483,647)",13,10
			BYTE	"Once you're done entering integers, the program will display a list",13,10
			BYTE	"of the integers, the sum of the integers, and the average of the values.",13,10
			BYTE	"Salmon on the bank / curing in the sun for days / eat them up yum yum ~A bear",13,10,13,10,0
numEntry	BYTE	": Please enter a signed integer: ",0
invalidNum	BYTE	"Something's wrong with your entry. Please try again.",13,10,0
numsEntered	BYTE	13,10,"Here are your numbers, all in a nice row:",13,10,0
numSum		BYTE	13,10,"The sum of your numbers is: ",0
numAverage	BYTE	13,10,"The truncated average of your numbers is: ",0
byePrompt	BYTE	13,10,13,10,"Goodbye, and thanks for all the fish! ROAR!",13,10,0

enteredVals	BYTE	"You've entered this many values: ",0
subTotMsg	BYTE	"Your subtotal so far is: ",0

oBracket	BYTE	"[",0
cBracket	BYTE	"]",0
comma		BYTE	", ",0
linebreak	BYTE	13,10,0

negSign		BYTE	"-",0

; variables
numArray	SDWORD	ARRAYLENGTH DUP(?)			; 10
userString	BYTE	MAXCHARS DUP(?)				; 16


valToWrite	SDWORD	?							; value to write
tempArray	BYTE	(MAXCHARS + 1) DUP(0)		; array used as scratch by writeVal -- 11 max characters, plus padding 0

strToWrite	BYTE	?							; 

; EC variables
runTotal	SDWORD	?

; final calcs
sumValues	DWORD	?
avgValues	DWORD	?






.code
main PROC
; display greeting prompt
	mDisplayString	OFFSET greeting

COMMENT &					; testing mgetstring
	CALL	crlf
	mGetString	OFFSET numEntry, OFFSET userString, MAXCHARS
	caLL crlf
	mdisplayString	OFFSET userString
	call crlf
	mov	EAX, uStrLen
	call WriteInt
	call crlf


	PUSH	OFFSET tempArray
	PUSH	-4200
	CALL	writeVal
	call	Crlf

&





; here is loop to fill array
	MOV		EDI, OFFSET numArray
	MOV		ECX, 1
_fillArray:
; number the output line and display prompt
	MOV		EAX, ECX

	PUSH	OFFSET tempArray
	PUSH	EAX
	CALL	writeVal


; call read procedure, store result in array
	mDisplayString	OFFSET numEntry			; output number entry prompt
	CALL	Readint							; READVAL GOES HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	MOV		[EDI], EAX

	ADD		runTotal, EAX
	
	INC		ECX								; next loop value
	ADD		EDI, TYPE numArray

	CMP		ECX, ARRAYLENGTH
	JG		_doneFill


; Display running subtotal
	mDisplayString	OFFSET subTotMsg
	MOV		EAX, runTotal

	PUSH	OFFSET tempArray
	PUSH	EAX
	CALL	writeVal

	mDisplayString	OFFSET linebreak
	mDisplayString	OFFSET linebreak

	JMP		_fillArray

_doneFill:


; display array + calc sum of elements

	mDisplayString	OFFSET numsEntered
	
	mDisplayString	OFFSET oBracket

	MOV		ESI, OFFSET numArray			;offset of first element
	MOV		ECX, ARRAYLENGTH				; count of loops to run

_displayLoop:
	LODSD									; Val to EAX, Increment ESI by 4
	ADD		sumValues, EAX					
	
	PUSH	OFFSET tempArray
	PUSH	EAX
	CALL	writeVal


; test if last loop, 
	CMP		ECX, 1
	JE		_endDisplay

; if not, write comma and loop again
	mDisplayString	OFFSET comma
	LOOP	_displayLoop

_endDisplay:
	mDisplayString	OFFSET cBracket			; closing bracket and linebreak

; here we display results
; procedure to calculate sum/average

	mDisplayString 	OFFSET numSum			; sum prompt

	MOV		EAX,	sumValues				; sum to EAX

	PUSH	OFFSET tempArray
	PUSH	EAX
	CALL	writeVal

	mDisplayString	OFFSET numAverage		; avg prompt

	CDQ
	MOV		EBX,	ARRAYLENGTH
	IDIV	EBX

	PUSH	OFFSET tempArray
	PUSH	EAX
	CALL	writeVal
	
	mDisplayString	OFFSET byePrompt

	Invoke	ExitProcess,0	; exit to operating system
main ENDP


; (insert additional procedures here)
;---------------------------------------------------------------------------------------------
; Name: readVal
;
; read value
;
; Preconditions: 
; Postconditions: 
; Receives:			
; Returns:			
;----------------------------------------------------------------------------------------------
readVal PROC
; prep stack, save registers
	PUSH	EBP
	MOV		EBP, ESP

	; meat goes here

	POP		EBP
	RET		0

readVal ENDP


;---------------------------------------------------------------------------------------------
; Name: writeVal
;
; Writes SDWORD value using mDisplayString macro. THe algorithm is a bit odd. Uses a temp
; array to store values, working backwards doing division by 10, storing the remainder as ASCII.
; Once fully calculated, it prints the array (forwards of course!)
;
; Preconditions:	
; Postconditions:	
; Receives:			[EBP + 8] -- SDWORD value to be printed	
;					[EBP + 12] -- address of temp array for scratch work
; Returns:			Outputs value as a string, using 
;----------------------------------------------------------------------------------------------
writeVal PROC
; prep stack, save registers
; use some local variables for fun
	LOCAL	localVal:SDWORD, arrayEnd:DWORD, negFlag:DWORD, specialFlag:DWORD
	PUSH	EAX
	PUSH	EBX
	PUSH	EDX
	PUSH	EDI

; get offset value to last usable idex of array -- remember it's MAXCHARS + 1 long,
; but we reserve the last ,0 to terminate the output
	MOV		EAX, MAXCHARS
	MOV		arrayEnd, EAX				
	
; retrieve value to print, and set negFlag = 0
	MOV		EAX, [EBP + 8]				; Rretrieve value from stack
	MOV		negFlag, 0					; set flag for non-negative
	CMP		EAX, 0						; test for negativity
	JGE		_notNegative

; initialize the special case flag to 0, and test for presence of -2,147,483,648
	MOV		specialFlag, 0
	CMP		EAX, -2147483648			; test for -2,147,483,648 -- this bastard thing
	JNE		_notSpeeshul				; not our boah

; rut-roh, we found it...
	INC		EAX							; bump it up to -2,147,483,647 for the next steps
	MOV		specialFlag, 1

; negate number nad set neg flag for later use
_notSpeeshul:
	NEG		EAX							; negate
	MOV		negFlag, 1					; set flag

; wasn't negative, so no special cases or anything
_notNegative:
	MOV		localVal, EAX				; store the value

; get address of array, and find target location
	MOV		EDI, [EBP + 12]
	ADD		EDI, arrayEnd				; increment to target location in array

	STD									; decrementing

; start of the calculating/storing loop
_startLoop:
; sequence of dividing by 10, and storing the remainder in the array as element to print
	MOV		EAX, localVal				; set up for divison
	CDQ		
	MOV		EBX, 10
	IDIV	EBX

; store the quotient for the next round
	MOV		localVal, EAX				; divided number to localval

; test for the "special case" flag
	CMP		specialFlag, 1
	JNE		_asciiAdd					; not special

; bump the value up (the LSB 7 becomes 8)
	INC		EDX
	MOV		specialFlag, 0				; clear flag so it never happens again

; convert to ASCII by adding 48, and store in the array
_asciiAdd:
	ADD		EDX, 48						; this converts it into ASCII equivalent!
	MOV		EAX, EDX
	STOSB

; if the quotient is 0, we've done enough math
	CMP		localVal, 0					; remainder 0, we're done dividing
	JE		_doneDiv					; exit loop

	JMP		_startLoop					; restart loop

; done dividing, do some output
_doneDiv:	 
; test to see if it's a negative number
	CMP		negFlag, 1
	JNE		_printArray					; not neg, just print

; number was negative? write the ASCII for a negative sign into the array!
	MOV		EAX, 45						; negative sign
	STOSB

; Print the finalized array!
_printArray:
	MOV		EAX, EDI
	ADD		EAX, 1						; readjust to account for last STOSB
	mDisplayString	EAX					; mDisplayString just works on an offset :D

; cleanup and exit
	POP		EDI
	POP		EDX
	POP		EBX
	POP		EAX
	RET		8

writeVal ENDP





END main
