BGM_Title:
dw BGM_Title_Pulse1
dw BGM_Title_Pulse2
dw BGM_Title_Wave
dw BGM_Title_Noise
BGM_Title_Pulse1:
db $C4,$FF,$C0,$BB
; pattern break
 envelope $82
 duty_cycle $40
 octave $3
 note $3E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $E
 note $5E
 wait $E
 note $8E
 wait $E
 octave $4
 note $1E
 wait $4
 wait $7
 octave $3
 note $8E
 wait $E
 note $5E
 wait $E
 note $3E
 wait $E
 note $3E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $5E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $8E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $AE
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $8E
 wait $E
 note $6E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $E
 note $8E
 wait $E
 note $AE
; pattern break
 wait $E
 envelope $82
 duty_cycle $40
 note $3E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $E
 note $5E
 wait $E
 note $8E
 wait $E
 octave $4
 note $1E
 wait $4
 wait $7
 octave $3
 note $8E
 wait $E
 note $5E
 wait $E
 note $3E
 wait $E
 note $3E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $5E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $8E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $AE
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $8E
 wait $E
 note $6E
 wait $4
 wait $7
 octave $4
 note $6E
 wait $4
 wait $7
 octave $3
 note $AE
 wait $E
 note $6E
 wait $E
 note $3E
; pattern break
 wait $E
 envelope $82
 duty_cycle $40
 note $3E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $E
 note $5E
 wait $E
 note $8E
 wait $E
 octave $4
 note $1E
 wait $4
 wait $7
 octave $3
 note $8E
 wait $E
 note $5E
 wait $E
 note $3E
 wait $E
 note $3E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $5E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $8E
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $AE
 wait $E
 octave $2
 note $AE
 wait $E
 octave $3
 note $8E
 wait $E
 note $6E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $E
 note $8E
 wait $E
 note $AE
; pattern break
 wait $E
 envelope $82
 duty_cycle $40
 note $3E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $E
 note $5E
 wait $E
 note $8E
 wait $E
 octave $4
 note $1E
 wait $4
 wait $7
 octave $3
 note $8E
 wait $E
 note $5E
 wait $E
 note $3E
 wait $E
 note $1E
 wait $E
 note $6E
 wait $E
 note $BE
 wait $E
 octave $4
 note $1E
 wait $E
 octave $3
 note $BE
 wait $E
 note $1E
 wait $E
 octave $4
 note $1E
 wait $E
 note $5E
 wait $E
 note $6E
 wait $E
 note $1E
 wait $E
 octave $3
 note $BE
 wait $E
 note $AE
 wait $E
 note $8E
 wait $E
 note $6E
 wait $E
 note $1E
 wait $E
 octave $2
 note $6E
; pattern break
 wait $E
 silence $E
 wait $3
 wait $5
 wait $7
 envelope $87
 duty_cycle $40
 octave $3
 note $1E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 octave $2
 note $BE
; pattern break
 wait $A
 wait $4
 wait $7
 envelope $87
 duty_cycle $40
 note $AE
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $2
 wait $C
 wait $7
 note $1E
; pattern break
 wait $4
 wait $7
 envelope $87
 duty_cycle $40
 note $3E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $C
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $C
 wait $7
 note $1E
; pattern break
 wait $4
 wait $7
 envelope $87
 duty_cycle $40
 octave $2
 note $BE
 wait $4
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $7
 octave $2
 note $BE
 wait $3
 wait $5
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $E
 silence $E
 wait $E
 note $1E
; pattern break
 wait $A
 wait $4
 wait $7
 silence $E
 wait $3
 wait $5
 wait $7
 envelope $87
 duty_cycle $80
 note $1E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 octave $2
 note $BE
; pattern break
 wait $A
 wait $4
 wait $7
 envelope $87
 duty_cycle $80
 note $AE
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $2
 wait $C
 wait $7
 note $1E
; pattern break
 wait $4
 wait $7
 envelope $87
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $C
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $C
 wait $7
 note $1E
; pattern break
 wait $4
 wait $7
 envelope $87
 duty_cycle $80
 octave $2
 note $BE
 wait $4
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
 wait $E
 silence $E
 wait $E
 note $8E
; pattern break
 wait $A
 wait $4
 wait $7
 envelope $97
 duty_cycle $40
 note $6E
 wait $2
 wait $C
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $9E
 wait $3
 wait $5
 wait $7
 note $4E
 wait $E
 silence $E
 wait $E
 note $4E
 wait $2
 wait $C
 wait $7
 note $1E
; pattern break
 wait $4
 wait $7
 envelope $97
 duty_cycle $40
 octave $2
 note $BE
 wait $4
 wait $7
 octave $3
 note $2E
 wait $3
 wait $6
 silence $E
 note $6E
 wait $2
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $9E
 wait $3
 wait $5
 wait $7
 note $4E
 wait $E
 silence $E
 wait $E
 note $4E
; pattern break
 wait $A
 wait $4
 wait $7
 envelope $97
 duty_cycle $80
 note $6E
 wait $2
 wait $C
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $9E
 wait $3
 wait $5
 wait $7
 note $4E
 wait $E
 silence $E
 wait $E
 note $4E
 wait $2
 wait $C
 wait $7
 duty_cycle $40
 note $1E
; pattern break
 wait $4
 wait $7
 envelope $97
 octave $2
 note $BE
 wait $4
 wait $7
 octave $3
 note $2E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $C
 wait $7
 note $1E
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $3
 note $2E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $7
 note $8E
; pattern break
 wait $3
 wait $5
 wait $7
 envelope $97
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
 wait $E
 silence $E
 wait $E
 note $8E
 wait $A
 wait $4
 wait $7
 envelope $94
 duty_cycle $40
 note $6E
 wait $2
 wait $7
 note $AE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
; pattern break
 wait $3
 wait $5
 wait $7
 envelope $95
 duty_cycle $40
 note $6E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $6E
; pattern break
 wait $2
 wait $7
 envelope $86
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $E
 silence $E
 wait $E
 note $1E
 wait $4
 wait $7
 note $3E
 wait $E
 silence $E
 wait $E
 note $3E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $95
 duty_cycle $40
 note $6E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $6E
; pattern break
 wait $2
 wait $7
 envelope $86
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $E
 silence $E
 wait $E
 note $6E
; pattern break
 wait $1
 wait $5
 wait $7
 envelope $95
 duty_cycle $40
 note $6E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $6E
; pattern break
 wait $2
 wait $7
 envelope $86
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $E
 silence $E
 wait $E
 note $1E
 wait $4
 wait $7
 note $3E
 wait $E
 silence $E
 wait $E
 note $3E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $95
 duty_cycle $40
 note $6E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 octave $4
 note $1E
 wait $3
 wait $5
 wait $7
 octave $3
 note $6E
; pattern break
 wait $2
 wait $7
 envelope $86
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $E
 silence $E
 wait $E
 note $6E
 wait $1
 wait $5
 wait $7
db $CE
BGM_Title_Pulse2:
; pattern break
 envelope $82
 duty_cycle $40
 octave $4
 note $3E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $E
 note $5E
 wait $E
 note $8E
 wait $E
 octave $5
 note $1E
 wait $4
 wait $7
 octave $4
 note $8E
 wait $E
 note $5E
 wait $E
 note $3E
 wait $E
 note $3E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $5E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $8E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $AE
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $8E
 wait $E
 note $6E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $E
 note $8E
 wait $E
 note $AE
; pattern break
 wait $E
 envelope $82
 duty_cycle $C0
 note $3E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $E
 note $5E
 wait $E
 note $8E
 wait $E
 octave $5
 note $1E
 wait $4
 wait $7
 octave $4
 note $8E
 wait $E
 note $5E
 wait $E
 note $3E
 wait $E
 note $3E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $5E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $8E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $AE
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $8E
 wait $E
 note $6E
 wait $4
 wait $7
 octave $5
 note $6E
 wait $4
 wait $7
 octave $4
 note $AE
 wait $E
 note $6E
 wait $E
 note $3E
; pattern break
 wait $E
 envelope $82
 duty_cycle $40
 note $3E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $E
 note $5E
 wait $E
 note $8E
 wait $E
 octave $5
 note $1E
 wait $4
 wait $7
 octave $4
 note $8E
 wait $E
 note $5E
 wait $E
 note $3E
 wait $E
 note $3E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $5E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $8E
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $AE
 wait $E
 octave $3
 note $AE
 wait $E
 octave $4
 note $8E
 wait $E
 note $6E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $E
 note $8E
 wait $E
 note $AE
; pattern break
 wait $E
 envelope $82
 duty_cycle $40
 note $3E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $E
 note $5E
 wait $E
 note $8E
 wait $E
 octave $5
 note $1E
 wait $4
 wait $7
 octave $4
 note $8E
 wait $E
 note $5E
 wait $E
 note $3E
 wait $E
 note $1E
 wait $E
 note $6E
 wait $E
 note $BE
 wait $E
 octave $5
 note $1E
 wait $E
 octave $4
 note $BE
 wait $E
 note $1E
 wait $E
 octave $5
 note $1E
 wait $E
 note $5E
 wait $E
 note $6E
 wait $E
 note $1E
 wait $E
 octave $4
 note $BE
 wait $E
 note $AE
 wait $E
 note $8E
 wait $E
 note $6E
 wait $E
 note $1E
 wait $E
 octave $3
 note $6E
; pattern break
 wait $E
 silence $E
 wait $2
 wait $7
 envelope $57
 duty_cycle $40
 note $1E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 octave $2
 note $BE
; pattern break
 wait $A
 wait $4
 wait $7
 envelope $57
 duty_cycle $40
 note $AE
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $57
 duty_cycle $40
 note $1E
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $C
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $6E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $57
 duty_cycle $40
 note $1E
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $7
 octave $2
 note $BE
 wait $3
 wait $5
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $4
 wait $7
 note $1E
; pattern break
 wait $2
 wait $C
 wait $7
 silence $E
 wait $2
 wait $7
 envelope $57
 duty_cycle $80
 note $1E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 octave $2
 note $BE
; pattern break
 wait $A
 wait $4
 wait $7
 envelope $57
 duty_cycle $80
 note $AE
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $57
 duty_cycle $80
 note $1E
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $C
 wait $7
 note $3E
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $6E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $57
 duty_cycle $80
 note $1E
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $3
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $2
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
 wait $4
 wait $7
 note $8E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $63
 duty_cycle $40
 octave $2
 note $BE
 wait $E
 octave $3
 note $2E
 wait $E
 note $6E
 wait $E
 note $9E
 wait $E
 octave $4
 note $2E
 wait $E
 note $6E
 wait $E
 note $9E
 wait $E
 octave $5
 note $2E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $4
 note $9E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $3
 note $9E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $2
 note $9E
 wait $E
 octave $3
 note $1E
 wait $E
 note $4E
 wait $E
 note $8E
 wait $E
 octave $4
 note $1E
 wait $E
 note $4E
 wait $E
 note $8E
 wait $E
 note $BE
 wait $E
 octave $5
 note $4E
 wait $E
 octave $4
 note $BE
 wait $E
 note $8E
 wait $E
 note $4E
 wait $E
 note $1E
 wait $E
 octave $3
 note $8E
 wait $E
 note $4E
 wait $E
 note $1E
; pattern break
 wait $E
 envelope $63
 duty_cycle $40
 octave $2
 note $BE
 wait $E
 octave $3
 note $2E
 wait $E
 note $6E
 wait $E
 note $9E
 wait $E
 octave $4
 note $2E
 wait $E
 note $6E
 wait $E
 note $9E
 wait $E
 octave $5
 note $2E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $4
 note $9E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $3
 note $9E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $2
 note $9E
 wait $E
 octave $3
 note $1E
 wait $E
 note $4E
 wait $E
 note $8E
 wait $E
 octave $4
 note $1E
 wait $E
 note $4E
 wait $E
 note $8E
 wait $E
 note $BE
 wait $E
 octave $5
 note $4E
 wait $E
 octave $4
 note $BE
 wait $E
 note $8E
 wait $E
 note $4E
 wait $E
 note $1E
 wait $E
 octave $3
 note $8E
 wait $E
 note $4E
 wait $E
 note $1E
; pattern break
 wait $E
 envelope $63
 duty_cycle $40
 octave $2
 note $BE
 wait $E
 octave $3
 note $2E
 wait $E
 note $6E
 wait $E
 note $9E
 wait $E
 octave $4
 note $2E
 wait $E
 note $6E
 wait $E
 note $9E
 wait $E
 octave $5
 note $2E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $4
 note $9E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $3
 note $9E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $2
 note $9E
 wait $E
 octave $3
 note $1E
 wait $E
 note $4E
 wait $E
 note $8E
 wait $E
 octave $4
 note $1E
 wait $E
 note $4E
 wait $E
 note $8E
 wait $E
 note $BE
 wait $E
 octave $5
 note $4E
 wait $E
 octave $4
 note $BE
 wait $E
 note $8E
 wait $E
 note $4E
 wait $E
 note $1E
 wait $E
 octave $3
 note $8E
 wait $E
 note $4E
 wait $E
 note $1E
; pattern break
 wait $E
 envelope $63
 duty_cycle $40
 octave $2
 note $BE
 wait $E
 octave $3
 note $2E
 wait $E
 note $6E
 wait $E
 note $9E
 wait $E
 octave $4
 note $2E
 wait $E
 note $6E
 wait $E
 note $9E
 wait $E
 octave $5
 note $2E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $4
 note $9E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $3
 note $9E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 note $8E
 wait $E
 note $BE
 wait $E
 octave $4
 note $2E
 wait $E
 note $6E
 wait $E
 note $8E
 wait $E
 note $BE
 wait $E
 octave $5
 note $2E
 wait $E
 note $6E
 wait $E
 note $8E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $4
 note $BE
 wait $E
 note $8E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $3
 note $BE
; pattern break
 wait $E
 envelope $63
 note $8E
 wait $E
 note $BE
 wait $E
 octave $4
 note $2E
 wait $E
 note $6E
 wait $E
 note $8E
 wait $E
 note $BE
 wait $E
 octave $5
 note $2E
 wait $E
 note $6E
 wait $E
 note $8E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $4
 note $BE
 wait $E
 note $8E
 wait $E
 note $6E
 wait $E
 note $2E
 wait $E
 octave $3
 note $BE
 wait $E
 envelope $54
 duty_cycle $80
 octave $4
 note $6E
 wait $2
 wait $7
 note $AE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
; pattern break
 wait $3
 wait $5
 wait $7
 envelope $95
 duty_cycle $40
 octave $3
 note $1E
 wait $3
 wait $5
 wait $7
 octave $2
 note $BE
 wait $3
 wait $5
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $8E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $4
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $3E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $56
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $3E
; pattern break
 wait $2
 wait $7
 envelope $95
 duty_cycle $40
 note $1E
 wait $3
 wait $5
 wait $7
 octave $2
 note $BE
 wait $3
 wait $5
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $8E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $4
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $3E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $56
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $E
 silence $E
 wait $E
 note $6E
; pattern break
 wait $A
 wait $4
 wait $7
 envelope $95
 duty_cycle $40
 note $1E
 wait $3
 wait $5
 wait $7
 octave $2
 note $BE
 wait $3
 wait $5
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $8E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $4
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $3E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $56
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $4
 wait $7
 note $5E
 wait $3
 wait $5
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $1E
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 note $3E
; pattern break
 wait $2
 wait $7
 envelope $95
 duty_cycle $40
 note $1E
 wait $3
 wait $5
 wait $7
 octave $2
 note $BE
 wait $3
 wait $5
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 note $3E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $3
 wait $5
 wait $7
 note $8E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $8E
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $3E
; pattern break
 wait $2
 wait $C
 wait $7
 envelope $56
 duty_cycle $80
 note $3E
 wait $4
 wait $7
 note $5E
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 note $BE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
 wait $3
 wait $5
 wait $7
 note $6E
 wait $E
 silence $E
 wait $E
 note $6E
 wait $A
 wait $4
 wait $7
db $CE
BGM_Title_Wave:
db $C1,$D5
; pattern break
 wave_vol $40
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 wave_vol $40
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 wave_vol $40
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 wave_vol $40
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
; pattern break
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $5E
 wait $4
 wait $7
 octave $3
 note $5E
 wait $4
 wait $7
 octave $2
 note $5E
 wait $4
 wait $7
 octave $3
 note $5E
 wait $4
 wait $7
 octave $2
 note $5E
 wait $4
 wait $7
 octave $3
 note $5E
 wait $4
 wait $7
 octave $2
 note $5E
 wait $4
 wait $7
 octave $3
 note $5E
; pattern break
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
; pattern break
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
; pattern break
 wait $4
 wait $7
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $5E
 wait $4
 wait $7
 octave $3
 note $5E
 wait $4
 wait $7
 octave $2
 note $5E
 wait $4
 wait $7
 octave $3
 note $5E
 wait $4
 wait $7
 octave $2
 note $5E
 wait $4
 wait $7
 octave $3
 note $5E
 wait $4
 wait $7
 octave $2
 note $5E
 wait $4
 wait $7
 octave $3
 note $5E
; pattern break
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
; pattern break
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
; pattern break
 wait $4
 wait $7
 octave $2
 note $2E
 wait $4
 wait $7
 octave $3
 note $2E
 wait $4
 wait $7
 octave $2
 note $2E
 wait $4
 wait $7
 octave $3
 note $2E
 wait $4
 wait $7
 octave $2
 note $2E
 wait $4
 wait $7
 octave $3
 note $2E
 wait $4
 wait $7
 octave $2
 note $2E
 wait $4
 wait $7
 octave $3
 note $2E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
 wait $4
 wait $7
 octave $1
 note $9E
 wait $4
 wait $7
 octave $2
 note $9E
; pattern break
 wait $4
 wait $7
 note $2E
 wait $4
 wait $7
 octave $3
 note $2E
 wait $4
 wait $7
 octave $2
 note $2E
 wait $4
 wait $7
 octave $3
 note $2E
 wait $4
 wait $7
 octave $2
 note $2E
 wait $4
 wait $7
 octave $3
 note $2E
 wait $4
 wait $7
 octave $2
 note $2E
 wait $4
 wait $7
 octave $3
 note $2E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
 wait $4
 wait $7
 octave $1
 note $8E
 wait $4
 wait $7
 octave $2
 note $8E
; pattern break
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $2
 wait $7
 note $AE
 wait $4
 wait $7
 note $AE
 wait $3
 wait $5
 wait $7
 note $8E
; pattern break
 wait $3
 wait $5
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 octave $1
 note $AE
 wait $4
 wait $7
 octave $2
 note $AE
 wait $4
 wait $7
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
 wait $4
 wait $7
 octave $2
 note $3E
 wait $4
 wait $7
 octave $3
 note $3E
; pattern break
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 octave $1
 note $BE
 wait $4
 wait $7
 octave $2
 note $BE
 wait $4
 wait $7
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $1E
 wait $4
 wait $7
 octave $3
 note $1E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
 octave $2
 note $6E
 wait $4
 wait $7
 octave $3
 note $6E
 wait $4
 wait $7
db $CE
BGM_Title_Noise:
db $CE
