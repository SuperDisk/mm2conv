BGM_WavetablePointer_Table:
    dw Wavetable1 ; Silence
    dw Wavetable1 ; Title
    dw Wavetable1 ; Stage Select
    dw Wavetable1 ; Stage Start
    dw Wavetable1 ; Password
    dw Wavetable1 ; Boss
    dw Wavetable1 ; Stage Clear
    dw Wavetable1 ; Weapon Get
    dw Wavetable1 ; Clash Man
    dw Wavetable1 ; Air Man
    dw Wavetable1 ; Metal Man
    dw Wavetable1 ; Hard Man
    dw Wavetable1 ; Wood Man
    dw Wavetable1 ; Top Man
    dw Wavetable1 ; Magnet Man
    dw Wavetable1 ; Needle Man
    dw Wavetable1 ; Wily Intro
    dw Wavetable1 ; Wily Castle
    dw Wavetable1 ; Ending
    dw Wavetable1 ; Unused Ending
    dw Wavetable1 ; Silence 2
    dw Wavetable1 ; Silence 3
    dw Wavetable1 ; Silence 4
    dw Wavetable1 ; Silence 5
    dw Wavetable1 ; Silence 6
    dw Wavetable1 ; Silence 7
    dw Wavetable1 ; Silence 8
    dw Wavetable1 ; Silence 9
    dw Wavetable1 ; Silence 10
    dw Wavetable1 ; Silence 11
    dw Wavetable1 ; Silence 12
    dw Wavetable1 ; Silence 13

; ================ Wavetables ================

Wavetable1:
db $00, $55, $AA, $FF, $00, $55, $AA, $FF, $00, $55, $AA, $FF, $00, $55, $AA, $FF
 ; db $00, $55, $AA, $FF, $AA, $55, $00, $55, $AA, $FF, $AA, $55, $00, $55, $AA, $FF
    ; db $00,$55,$AA,$FF,$00,$55,$AA,$FF,$00,$55,$AA,$FF,$00,$55,$AA,$FF
; Hex     : 0055AAFF0055AAFF0055AAFF0055AAFF
; Decimal : 0 0 5 5 10 10 15 15 0 0 5 5 10 10 15 15 0 0 5 5 10 10 15 15 0 0 5 5 10 10 15 15


Wavetable2:
    db $00,$55,$AA,$FF,$AA,$55,$00,$55,$AA,$FF,$AA,$55,$00,$55,$AA,$FF
; Hex     : 0055AAFFAA550055AAFFAA550055AAFF
; Decimal : 0 0 5 5 10 10 15 15 10 10 5 5 0 0 5 5 10 10 15 15 10 10 5 5 0 0 5 5 10 10 15 15


Wavetable3:
    db $00,$22,$44,$66,$88,$AA,$CC,$EE,$FF,$DD,$BB,$99,$77,$55,$33,$11
; Hex     : 0022446688AACCEEFFDDBB9977553311
; Decimal : 0 0 2 2 4 4 6 6 8 8 10 10 12 12 14 14 15 15 13 13 11 11 9 9 7 7 5 5 3 3 1 1


Wavetable4:
    db $00,$ED,$FE,$C6,$00,$F5,$F3,$D5,$00,$F5,$FB,$D7,$00,$F5,$03,$C9
; Hex     : 00EDFEC600F5F3D500F5FBD700F503C9
; Decimal : 0 0 14 13 15 14 12 6 0 0 15 5 15 3 13 5 0 0 15 5 15 11 13 7 0 0 15 5 0 3 12 9
