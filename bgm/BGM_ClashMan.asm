BGM_ClashMan:
    dw BGM_ClashMan_Pulse1
    dw BGM_ClashMan_Pulse2
    dw BGM_ClashMan_Wave
    dw BGM_ClashMan_Noise

; ==== Pulse 1 ====

BGM_ClashMan_Pulse1:
    db $C4,$FF,$C0,$D7,$C1,$C0,$C2,$C3

BGM_ClashMan_Pulse1_Loop:
    db $F4,$A2,$F5,$52,$33,$12,$03,$E3,$F4,$A2,$A3,$F5,$02,$12,$F4,$A2
    db $F5,$52,$33,$12,$03,$E3,$F4,$A2,$A3,$F5,$53,$33,$13,$33,$F4,$82
    db $F5,$32,$13,$02,$F4,$A3,$E3,$82,$83,$A2,$F5,$02,$F4,$82,$F5,$32
    db $13,$02,$F4,$A3,$E3,$82,$83,$F5,$13,$03,$F4,$A3,$F5,$03,$03,$03
    db $03,$03,$03,$12,$F4,$A3,$E0,$E2,$F5,$52,$33,$12,$03,$E3,$F4,$A2
    db $A3,$F5,$02,$12,$E2,$52,$33,$12,$03,$E3,$F4,$A2,$A3,$F5,$D2,$12
    db $F4,$80,$A0,$F5,$00,$D0,$E2,$52,$33,$12,$03,$E3,$F4,$A2,$A3,$F5
    db $02,$D2,$E2,$52,$33,$12,$03,$E3,$F4,$A2,$A3,$F5,$02,$D2,$F4,$80
    db $A0,$F5,$00,$11,$31,$52,$33,$11,$D3,$52,$33,$12,$33,$53,$83,$D2
    db $39,$F4,$02,$12,$33,$5A,$F5,$52,$33,$11,$D3,$52,$33,$12,$33,$53
    db $83,$82,$39,$F4,$02,$12,$33,$5A,$F5,$53,$53,$53,$52,$53,$53,$03
    db $E3,$03,$19,$E0
    jp BGM_ClashMan_Pulse1_Loop

; ==== Pulse 2 ====

BGM_ClashMan_Pulse2:
    db $C1,$40,$C2,$C3

BGM_ClashMan_Pulse2_Loop:
    db $F4,$A0,$F5,$00,$10,$30,$50,$80,$F4,$A0,$F5,$00,$F4,$83,$83,$83
    db $83,$83,$A2,$53,$E0
    call BGM_ClashMan_Pulse2_Sub1
    call BGM_ClashMan_Pulse2_Sub1
    call BGM_ClashMan_Pulse2_Sub2
    db $E2,$F5,$12,$03,$F4,$A2,$83,$E3,$52,$53,$82,$A2,$E2,$F5,$12,$03
    db $F4,$A2,$83,$E3,$52,$53,$82,$A2
    call BGM_ClashMan_Pulse2_Sub2
    db $F5,$12,$03,$F4,$81,$D3,$F5,$12,$03,$F4,$82,$F5,$03,$13,$33,$32
    db $09
    call BGM_ClashMan_Pulse2_Sub3
    db $F5,$12,$03,$F4,$81,$D3,$F5,$12,$03,$F4,$82,$F5,$03,$13,$33,$D2
    db $09
    call BGM_ClashMan_Pulse2_Sub3
    db $F5,$13,$13,$13,$12,$13,$13,$F4,$83,$E3,$83,$A9,$E0
    jp BGM_ClashMan_Pulse2_Loop

BGM_ClashMan_Pulse2_Sub1:
    db $E2,$F5,$12,$03,$F4,$A2,$83,$E3,$52,$53,$82,$A2
    ret

BGM_ClashMan_Pulse2_Sub2:
    db $F5,$83,$F6,$13,$53,$F5,$83,$F6,$13,$53,$F5,$83,$F6,$13,$F5,$A3
    db $F6,$13,$53,$F5,$A3,$F6,$13,$53,$F5,$A3,$F6,$13,$03,$13,$53,$03
    db $13,$53,$03,$13,$13,$33,$83,$13,$33,$83,$13,$33
    ret

BGM_ClashMan_Pulse2_Sub3:
    db $F6,$14,$14,$14,$14,$04,$04,$04,$04,$14,$14,$14,$14,$34,$34,$34
    db $34
    ret

; ==== Wave ====

BGM_ClashMan_Wave:
    db $C1,$E0,$C2,$20

BGM_ClashMan_Wave_Loop:
    db $F1,$AA,$53,$83,$92,$A3,$E3,$A2,$53,$82,$92,$AA,$53,$83,$92,$A3
    db $E3,$A2,$53,$F2,$12,$02,$F1,$8A,$33,$53,$82,$83,$E3,$82,$33,$52
    db $82,$8A,$33,$53,$82,$83,$E3,$82,$33,$52,$82,$A3,$A3,$A3,$A3,$A3
    db $A2,$A3,$E0
    call BGM_ClashMan_Wave_Sub1
    call BGM_ClashMan_Wave_Sub1
    db $63,$F2,$13,$63,$F1,$63,$F2,$13,$63,$F1,$63,$F2,$13
    call BGM_ClashMan_Wave_Sub2
    db $F1,$63,$F2,$13,$63,$F1,$63,$F2,$13,$63,$F1,$63,$F2,$13
    call BGM_ClashMan_Wave_Sub2
    db $F1,$A9,$D3,$53,$52,$69,$E0
    jp BGM_ClashMan_Wave_Loop

BGM_ClashMan_Wave_Sub1:
    db $AA,$53,$83,$92,$A3,$E3,$A2,$53,$82,$92,$AA,$53,$83,$92,$A3,$E3
    db $A2,$53,$F2,$12,$02,$F1,$8A,$33,$53,$82,$83,$E3,$82,$33,$52,$82
    db $8A,$33,$53,$82,$83,$E3,$82,$33,$52,$82
    ret

BGM_ClashMan_Wave_Sub2:
    db $F1,$63,$F2,$13,$63,$F1,$63,$F2,$13,$63,$F1,$63,$F2,$13,$F1,$83
    db $F2,$33,$83,$F1,$83,$F2,$33,$83,$F1,$83,$F2,$33,$F1,$83,$F2,$33
    db $83,$F1,$83,$F2,$33,$83,$F1,$83,$F2,$33
    ret

; ==== Noise ====

BGM_ClashMan_Noise:
    db $C1,$01,$C2,$C1

BGM_ClashMan_Noise_Loop:
    call BGM_ClashMan_Noise_Sub1
    db $03,$03,$13,$23,$33,$42,$53,$43,$43,$43,$43,$03,$03,$03,$03
    call BGM_ClashMan_Noise_Sub1
    call BGM_ClashMan_Noise_Sub1
    call BGM_ClashMan_Noise_Sub1
    db $53,$53,$53,$52,$53,$53,$53,$50,$54,$54,$54,$54,$54,$54,$54,$54
    db $04,$04,$04,$04,$04,$04,$04,$04
    jp BGM_ClashMan_Noise_Loop

BGM_ClashMan_Noise_Sub1:
    db $03,$03,$43,$03,$03,$03,$43,$03,$03,$03,$43,$03,$03,$03,$43,$03
    db $03,$03,$43,$03,$03,$03,$43,$03,$03,$03,$43,$03,$03,$03,$43,$03
    db $03,$03,$43,$03,$03,$03,$43,$03,$03,$03,$43,$03,$03,$03,$43,$03
    db $03,$03,$43,$03,$03,$03,$43,$03,$03,$03,$43,$03,$03,$03,$43,$03
    ret
