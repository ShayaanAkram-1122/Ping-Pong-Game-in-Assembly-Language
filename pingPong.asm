[org 0x0100]

jmp start

;;;;;;;;;; STRINGS FOR PRINTING ;;;;;;;;;;;;;;;
gameString: db '====== Ping Pong Game ======Loading...'
player1: db 'Player 1 Score: '
player2: db 'Player 2 Score: '
winnerMessage: db ' Player 1 Wins! '
winner2Message: db ' Player 2 Wins! '
gamePauseMsg: db ' Game Paused. Press P to continue '
rollNumbers: db 'Muhammad 23F-0677Shayaan 23F-0710'
instructionStr: db '====== Instructions ======Player 1: Use W S keys to movePlayer 2: Use  /\ \/ keys to movePress G to start and P to pauseFirst player to score 5 points wins!'

gameEnd: db 0							;for game loop
isPaused: db 0							;used to pause and continue game
HardLevel: db 0							;used to change modes (hard level)
ballMoving: db 1						;to start the ball movement after scoring

paddle1loc: dw 1604, 1764, 1924, 2084, 2244					;default position of paddle 1
paddle2loc: dw 1754, 1914, 2074, 2234, 2394					;default position of paddle 2
isPaddle1: db 0												;check if the ball hits the paddle 1
isPaddle2: db 0												;check if the ball hits the paddle 2

p1Score: dw 0							;player 1 score
p2Score: dw 0							;player 2 score

changeRight: db 0						;change the ball position when it hits right wall
changeLeft: db 0						;change the ball position when it hits left wall
changeUpDown: db 0						;change the ball position when it hits up down wall

	start:
		call clearScreen
		call printGameName				;loading screen		
					
		mov di, 166						;starting index of ball
		programEnd:
			call detectKeyInt 					;detect key 
				cmp byte [isPaused], 1			;if P = 1 then pause the game
				je skipGameLogic				;else continue
				cmp byte [ballMoving], 0		;check if a player has scored, if yes then stop the ball until g is pressed
				je skipGameLogic
				
			continueGameLogic:
				call clearScreen
				call calculateScore
				call printplayerScore
				call printGameBoundries
				call paddle
				call ball
				jmp skipGameLogic
				
			skipGameLogic:
			call detectKeyInt 
			cmp byte[gameEnd], 1				;check if some player has scored 5 points, if yes then terminate program
			jne programEnd				

	end:
		mov ax, 0x4c00
		int 0x21

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BALL MOVEMENT AND PRINTING LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ball:
		push ax 
		push es
			mov ax, 0xb800
			mov es, ax
			xor ax, ax
			
			ballMovement:
				cmp byte[HardLevel], 1				;check for hard level, 
				jne colWhite1						
				mov ah, 0x47						;red color for hard level
				jmp backLoop1	
				
				colWhite1:
				mov ah, 0x07						;black color with white text for normal level
				
				backLoop1:
				mov al, 0x20					
				mov word[es:di], ax					;prints space on last location to delete the ball
				
				cmp byte [changeUpDown], 1			;compares the bool variable
				je moveUpOnly						;if i= 1 then move ball upwards
				jne movDownOnly						;if = 0 then move ball downwards
				
				moveUpOnly:
					sub di, 156						;subtract 156 to move ball diagnolly upward
					jmp sidescheckRight
				
				movDownOnly:
					add di, 164						;add 164 to move ball diagnolly downwards
				
				sidescheckRight:
					cmp byte[changeRight], 1		;if ball hits right wall then reflect ball to left	
					jne sidecheckLeft
					sub di, 8						;by decrementing 8 from di
				
				sidecheckLeft:
					cmp byte[changeLeft], 1			;if ball hits left wall then reflect ball to right
					jne dynamicMovement
				
				dynamicMovement:
					xor ax, ax
					mov al, 0xdc					
					cmp byte[HardLevel], 1			;if hardlevel = 1 then print in red else print in blacl/white
					jne colWhite9	
					mov ah, 0x47
					jmp printBall
					colWhite9:
					mov ah, 0x07
					
					printBall:
					mov word[es:di], ax				;print the actual character of ball
					call delay						
					call delay						;adding extra delay to make the game playable
					call checkUp					;checks for direcion of ball
					call checkDown
					call checkRight
					call checkLeft	
		
		ballMoveEnd:
		pop es
		pop ax
		ret

	checkUp:
		cmp di, 3680						;if 2nd last row is detected
		jb UpEnd
			mov byte[changeUpDown], 1		;then change the direction to move upwards
		UpEnd:
		ret
		
	checkDown:	
		cmp di, 320							;if 2nd row is detected
		ja downEnd
			mov byte[changeUpDown], 0		; then change the direction to move downwards
		downEnd:
		ret

	checkLeft:
		push ax
		push dx
		push bx
		
		call checkPaddle1					;check if paddle is detected in ball direction
		
		xor dx, dx							;empty dx
		mov ax, di							;move di to ax for calculation
		mov bx, 160							
		div bx								;divide di by 160
		
		cmp byte[isPaddle1], 1				;if paddle is detected then change ball direction
		je chngLeft
		
		cmp dx, 2							;if left wall is detected then move ball back
		jne sideEnd

		chngLeft:
			mov byte[changeLeft], 1			;ball will move to right
			mov byte[changeRight], 0		;left movement is stopped
		sideEnd:
		
		pop bx
		pop dx
		pop ax
		ret

	checkRight:
		push ax
		push dx
		push bx
		
		call checkPaddle2				;check if paddle is in way
		
		xor dx, dx
		mov ax, di
		mov bx, 160
		div bx							;divide di with 160
		
		cmp byte[isPaddle2], 1			;if paddle is detected then change directions
		je chngRight
		
		cmp dx, 158						;if reminder is 158 i.e last column then change direction
		jne sideEnd2
		
		chngRight:
			mov byte[changeRight], 1	;move ball to left
			mov byte[changeLeft], 0		;stop right movement
		sideEnd2:
		
		pop bx
		pop dx
		pop ax
		ret

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DELAYS AND CLEAR SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	delay:
		pusha
		cmp byte[HardLevel], 1			;if hardlevel = 1 then increase the speed of ball
		jne speedLow
		mov cx, 0xAfAf					;44975 times loop
		jmp delayLoop
		
		speedLow:
		mov cx, 0xffff					; the loop will work 65535 times and will produce delay
		delayLoop:
			sub cx, 1
			jne delayLoop
		
		popa
		ret

		
	clearScreen:
		pusha
		
		xor di, di
		mov ax, 0xb800
		mov es, ax
		xor ax,ax
		
		cmp byte[HardLevel], 1			
		jne colWhite
		mov ah, 0x40
		jmp backLoop
		
		colWhite:
		mov ah, 0x07
		
		backLoop:
		mov al, 0x20
		mov cx, 2000
		rep stosw
		
		popa
		ret
		
		
	delayLong:
		pusha
		call delay
		call delay
		call delay
		call delay
		call delay
		call delay
		call delay
		call delay
		popa
		ret

		
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOADING SCREEN AND GAME NAME PRINTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	printGameName:
		pusha
			mov di, 1652
			mov ax, 0xb800
			mov es, ax
			mov si, gameString
			mov ah, 0x0C
			mov cx, 28	
		
			gamenameLoop:					;game name print
				lodsb
				stosw
				loop gamenameLoop
			
			mov cx, 10
			mov ah, 0x07
			mov di, 2312
			loadingLoop:					;loading... print
				lodsb
				stosw
				loop loadingLoop
					
			mov di, 2452					;back loading bar 
			mov cx, 28
			mov ax, 0x07b0
			rep stosw
			
			mov ah, 0x04
			mov si, rollNumbers
			mov di, 3708
			mov cx, 17
			rollNumberPrint1:				;prints mohos roll number
				lodsb
				stosw
				loop rollNumberPrint1
		
			mov di, 3780
			mov cx, 16
			rollNumberPrint2:				;prints shishimarus roll number
				lodsb
				stosw
				loop rollNumberPrint2
			
			mov di, 2452					
			mov cx, 10
			mov ax, 0x07db		
		l1:									;prints front loading bar (chunk 1)
			call delayLong
			mov word[es:di], ax
			add di, 2
			loop l1
		
		mov cx, 950							;clears the upper part of screen
		xor di, di
		mov ax, 0x0720
		rep stosw
			
		
		mov si, instructionStr	
		mov di, 1494
		mov ah, 0x0E
		mov cx, 26
		instructionLoop:					;prints instructions 
			lodsb
			stosw
			loop instructionLoop
		
		mov di, 1810
		mov ah, 0x07
		mov cx, 30
		instructionLoop2:					;print 1st line of instruction
			lodsb
			stosw
			loop instructionLoop2
		
		mov di, 1968
		mov cx, 33
		instructionLoop3:					;;print 2nd line of instruction
			lodsb
			stosw
			loop instructionLoop3

			mov di, 2472
			mov cx, 13
			mov ax, 0x07db
		l2:									;prints front loading bar (chunk 2)
			call delayLong
			mov word[es:di], ax
			add di, 2
			loop l2
		
		
		mov cx, 200							;clear first half for next instructions
		mov di, 1810
		mov ax, 0x0720
		rep stosw
		
		mov di, 1808
		mov cx, 31
		instructionLoop4:					;print 3rd line of instruction
			lodsb
			stosw
			loop instructionLoop4
		
		mov di, 1964
		mov cx, 36
		instructionLoop5:					;print 4th line of instruction
			lodsb
			stosw
			loop instructionLoop5
			
			mov di, 2498
			mov cx, 6
			mov ax, 0x07db
		l3:									;prints front loading bar (chunk 3)
			call delayLong
			call delay
			call delay
			call delay
			mov word[es:di], ax
			add di, 2
			loop l3
			
			xor di, di
			popa
			ret
			
	printGameBoundries:
		pusha
			mov ax, 0xb800
			mov es, ax
			xor di, di
			mov al, 0xdb
			
			cmp byte[HardLevel], 1			;print yellow color on hard level else blue
			jne colWhite7
			mov ah, 0x0F
			jmp printBoundry				
			colWhite7:
			mov ah, 0x03
			
			printBoundry:					;print upper boundry
			mov cx, 80
			rep stosw
			
			mov di, 3840					;print lower boundry
			mov cx, 80
			rep stosw	
		
		popa
		ret

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRIINT AND CALCULATE SCORE LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	printplayerScore:
		pusha
			mov di, 180
			mov ax, 0xb800
			mov es, ax
			mov si, player1					
			
			cmp byte[HardLevel], 1
			jne colWhite2
			mov ah, 0x47
			jmp backLoop
			
			colWhite2:
			mov ah, 0x07

			mov cx, 16
			p1Loop:							;print player 1 string
				lodsb
				stosw
				loop p1Loop
				
			mov al, [p1Score]				;print player 1 score
			add al, 0x30					;to convert into ascii 
			mov word [es:di], ax
			
			mov di, 260
			mov si, player2
			mov cx, 16
			p2Loop:							;print player 2 string
				lodsb
				stosw
				loop p2Loop
			
			mov al, [p2Score]				;print player 2 score
			add al, 0x30
			mov word [es:di], ax
		popa
		ret
			
	calculateScore:
		push ax
		push bx
		push dx
			
			xor dx, dx
			mov ax, di
			mov bx, 160
			div bx							;divide 160 with di
			cmp dx, 158						;it means ball has hit right wall = score for player 1
			je addPlayer1
			cmp dx, 2						;it means ball has hit left wall = score for player 2
			je addPlayer2
			jmp calculateScoreEnd
			
			addPlayer1:
				add word [p1Score], 1		;inc in score
				mov byte [ballMoving], 0	;stops the ball movement
				mov di, 314					;reset ball position to right side
				mov byte [changeLeft], 0	;reset ball directions to default
				mov byte [changeRight], 1
				mov byte [changeUpDown], 0
				cmp word[p1Score], 5		;if score = 5 then player 1 wins
				je winner1MilGaya
				jmp calculateScoreEnd
				
			addPlayer2:
				add word [p2Score], 1			;inc in score
				mov byte [ballMoving], 0		;stops the ball movement
				mov di, 166						;reset ball position to left side
				mov byte [changeLeft], 0		;reset ball directions to default
				mov byte [changeRight], 0
				mov byte [changeUpDown], 0
				cmp word[p2Score], 5			;if score = 5 then player 2 wins
				je winner2MilGaya
				jmp calculateScoreEnd
		
		winner1MilGaya:							;prints player 1 wins
			mov byte[gameEnd], 1	
			mov ax, 0xb800
			mov es, ax
			mov si, winnerMessage
			mov di, 1982
			mov cx, 16
			winner1Loop:
				lodsb
				stosw
				loop winner1Loop
			jmp calculateScoreEnd
				
		winner2MilGaya:
			mov byte[gameEnd], 1				;prints player 2 wins
			mov ax, 0xb800
			mov es, ax	
			mov si, winner2Message
			mov di, 1982
			mov cx, 16
			winner2Loop:
				lodsb
				stosw
				loop winner2Loop
				
		calculateScoreEnd:
		pop dx
		pop bx
		pop ax
		ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PADDLE PRINT AND LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	paddle:
		pusha 
		
		mov ax, 0x07ba
		mov cx, 5
		mov si, 0
		
		paddle1:
			mov di, word[paddle1loc+si]			;move di defined default location of paddle
			mov word[es:di], ax					;print the paddle from array
			add si, 2							;inc in si for next index
			loop paddle1	
		
		mov cx, 5
		mov si, 0
		paddle2:								;repeat the process for paddle 2
			mov di, word[paddle2loc+si]
			mov word[es:di], ax
			add si, 2
			loop paddle2
		
		popa
		ret
		
	checkPaddle1:
		pusha
		
			mov bx, 0x07ba				;ascii of paddle
			sub di, 2					;subtract 2 to get one index behind the ball 
			
			cmp word[es:di], bx			;compare the ascii present on screen on di-ith index with the ascii of the paddle		
			je paddle1yes				;if the ascii of paddle and that index is same, it means there is a paddle behind the ball
			mov byte[isPaddle1], 0		;dont change the direction if paddle is not found
			jmp endPaddle1Check
		
		paddle1yes:
			mov byte[isPaddle1], 1		;change the direction if paddle is found
		
		endPaddle1Check:
		popa
		ret

	checkPaddle2:
		pusha
		
			mov bx, 0x07ba				;ascii of paddle
			add di, 4					;add 4 to get index present infront of the ball
			
			cmp word[es:di], bx			;compare the ascii present on screen on di-ith index with the ascii of the paddle
			je paddle2yes				;if the ascii of paddle and that index is same, it means there is a paddle behind the ball
			mov byte[isPaddle2], 0
			jmp endPaddle2Check
		
		paddle2yes:
			mov byte[isPaddle2], 1		;change the direction if paddle is found
		
		endPaddle2Check:
		popa
		ret

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEY DETECT AND INTERRUPTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	detectKeyInt:
		pusha
			xor si, si
			mov cx, 5			;for loop, used to inc/dec in paddle using si
			xor ax, ax
			
			mov ah, 0x01        ; Check if a key is available
			int 0x16
			jz noKeyDetected 
			
			mov ah, 0x00        ; Get the key (non-blocking)
			int 0x16
			
			cmp ah, 0x22        ;for g key
			je StartBall
			
			cmp ah, 0x11		;for w key
			je keyW
			
			cmp ah, 0x1F		;for s key
			je keyS 
			
			cmp ah, 0x48		;for up key
			je keyUp
			
			cmp ah, 0x50		;for down key
			je keyDown
					
			cmp ah, 0x19        ;for 'P' key
			je PauseGame
			
			cmp ah, 0x23		;for h key
			je levelChange
		
			
			noKeyDetected:
				jmp endDetectKey
				
			StartBall:       
				mov byte[ballMoving], 1				;start the ball movement 
				jmp endDetectKey

			
			keyW:
				cmp word[paddle1loc + 8], 804		;if paddle hits the upper boundry then no movement
				je keyWend
			
				sub word [paddle1loc + si], 160		;subtract 160 from array that moves the paddle 1 line up
				add si, 2
				loop keyW							;loop to sub in all indexes
				
				keyWend:
				jmp endDetectKey
				
			keyS:
				cmp word[paddle1loc+8], 3684		;if paddle hits the lower boundry then no movement
				je keySend
				
				add word [paddle1loc + si], 160		;add 160 from array that moves the paddle 1 line down
				add si, 2		
				loop keyS							;loop to add in all indexes
				
				keySend:
				jmp endDetectKey
			
			keyUp:
				cmp word[paddle2loc +8 ], 954		;if paddle hits the upper boundry then no movement
				je keyUpEnd
			
				sub word [paddle2loc + si], 160		;subtract 160 from array that moves the paddle 1 line up
				add si, 2
				loop keyUp							;loop to sub in all indexes
				
				keyUpEnd:
				jmp endDetectKey
			
			keyDown:
				cmp word[paddle2loc+8], 3834		;if paddle hits the lower boundry then no movement
				je keyDownEnd
				
				add word [paddle2loc + si], 160		;add 160 from array that moves the paddle 1 line down
				add si, 2
				loop keyDown						;loop to add in all indexes
				
				keyDownEnd:
				jmp endDetectKey
				
			PauseGame:			
					cmp byte[isPaused], 1
					call PauseGameScreen		;if game is oaused then print pause message
			
					mov al, [isPaused]
					xor al, 1           ;toggle between 0 and 1
					mov [isPaused], al
					jmp endDetectKey
					
			levelChange:
				mov al, [HardLevel]		
				xor al, 1				;toggle between 0 and 1
				mov [HardLevel], al
				jmp endDetectKey
				
		endDetectKey:
		popa
		ret
		
	PauseGameScreen:
		pusha
		
		xor ax, ax
		mov ah, 0x70
		mov di, 1966
		mov si, gamePauseMsg
		mov cx, 34	
		PauseMsgLoop:					;prints game is paused messgae
			lodsb	
			stosw
			loop PauseMsgLoop
			
		popa
		ret