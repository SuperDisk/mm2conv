BGM_Ending:
    dw BGM_Ending_Pulse1
    dw BGM_Ending_Pulse2
    dw BGM_Ending_Wave
    dw BGM_Ending_Noise

; ==== Pulse 1 ====

BGM_Ending_Pulse1:
    db $C4,$FF,$C0,$C0,$C1,$80,$C2,$83
    call BGM_Ending_Pulse1_Sub1
    call BGM_Ending_Pulse1_Sub1
    call BGM_Ending_Pulse1_Sub1
    call BGM_Ending_Pulse1_Sub1

BGM_Ending_Pulse1_Loop:
    db $F4,$C1,$80,$C2,$83
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub3
    call BGM_Ending_Pulse1_Sub3
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub2
    db $84,$B4,$F5,$14,$34,$F4,$B4,$F5,$34,$84,$B4,$F4
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub3
    call BGM_Ending_Pulse1_Sub3
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub4
    call BGM_Ending_Pulse1_Sub4
    db $C1,$40,$C2,$A7,$F5,$61,$41,$31,$12,$F4,$B2
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub5
    db $61,$41,$31,$12,$F4,$B2
    call BGM_Ending_Pulse1_Sub2
    call BGM_Ending_Pulse1_Sub5
    jp BGM_Ending_Pulse1_Loop

BGM_Ending_Pulse1_Sub1:
    db $F4,$94,$A4,$B4,$F5,$03,$F4,$B4,$A4,$94,$94,$A4,$B4,$F5,$04,$F4
    db $A4,$B4,$F5,$04,$14
    ret

BGM_Ending_Pulse1_Sub2:
    db $84,$B4,$F5,$34,$F4,$84,$B4,$F5,$34,$F4,$84,$B4
    ret

BGM_Ending_Pulse1_Sub3:
    db $84,$B4,$F5,$14,$F4,$84,$B4,$F5,$14,$F4,$84,$B4
    ret

BGM_Ending_Pulse1_Sub4:
    db $64,$A4,$F5,$14,$F4,$64,$A4,$F5,$14,$F4,$64,$A4
    ret

BGM_Ending_Pulse1_Sub5:
    db $84,$B4,$F5,$14,$34,$F4,$B4,$F5,$14,$34,$64,$F4,$B4,$F5,$14,$34
    db $64,$14,$34,$64,$A4,$F4,$AB,$BB,$F5,$13
    ret

; ==== Pulse 2 ====

BGM_Ending_Pulse2:
    db $C1,$40,$C2,$A7,$F4,$9A,$A4,$B4,$F5,$0A,$F4,$B4,$A4
    call BGM_Ending_Pulse2_Sub1
    call BGM_Ending_Pulse2_Sub1
    call BGM_Ending_Pulse2_Sub1

BGM_Ending_Pulse2_Loop:
    call BGM_Ending_Pulse2_Sub2
    db $BB,$F6,$1B,$31,$D3
    call BGM_Ending_Pulse2_Sub2
    db $F6,$6A,$F5,$A1,$D3
    call BGM_Ending_Pulse2_Sub3
    call BGM_Ending_Pulse2_Sub3
    jp BGM_Ending_Pulse2_Loop

BGM_Ending_Pulse2_Sub1:
    db $9A,$A4,$B4,$F5,$0A,$F4,$B4,$A4
    ret

BGM_Ending_Pulse2_Sub2:
    db $E3,$F5,$83,$B3,$F6,$12,$F5,$B2,$83,$F6,$8B,$6B,$33,$1B,$F5,$BB
    db $83,$E3,$83,$B3,$F6,$12,$F5,$B2,$83
    ret

BGM_Ending_Pulse2_Sub3:
    db $BA,$F6,$12,$F5,$B2,$83,$F6,$6A,$F5,$AA,$62,$80,$A1,$AB,$8B,$63
    ret

; ==== Wave ====

BGM_Ending_Wave:
    db $C1,$80,$C2,$20,$F1
    call BGM_Ending_Wave_Sub1
    call BGM_Ending_Wave_Sub1
    call BGM_Ending_Wave_Sub1
    call BGM_Ending_Wave_Sub1

BGM_Ending_Wave_Loop:
    db $F1,$4B,$44,$F2,$43,$F1,$43,$43,$F2,$43,$F1,$44,$F2,$44,$F1,$43
    call BGM_Ending_Wave_Sub2
    call BGM_Ending_Wave_Sub2
    call BGM_Ending_Wave_Sub2
    db $6B,$64,$F2,$63,$F1,$6B,$F2,$63,$F1,$64,$F2,$64,$F1,$6B
    call BGM_Ending_Wave_Sub3
    call BGM_Ending_Wave_Sub3
    jp BGM_Ending_Wave_Loop

BGM_Ending_Wave_Sub1:
    db $B3,$B3,$B3,$B3,$B3,$B3,$B3,$B3
    ret

BGM_Ending_Wave_Sub2:
    db $83,$F2,$83,$F1,$83,$F2,$83,$F1,$B3,$F2,$B3,$F1,$B3,$F2,$B3,$F1
    db $4B,$44,$F2,$43,$F1,$43,$43,$F2,$43,$F1,$44,$F2,$44,$F1,$43
    ret

BGM_Ending_Wave_Sub3:
    db $B3,$B3,$B3,$B3,$A3,$A3,$A3,$A3,$83,$83,$83,$83,$63,$63,$63,$63
    db $43,$43,$43,$43,$43,$43,$43,$43,$63,$63,$63,$63,$6B,$8B,$A3
    ret

; ==== Noise ====

BGM_Ending_Noise:
    db $C1,$01,$C2,$A1
    call BGM_Ending_Noise_Sub1
    call BGM_Ending_Noise_Sub1
    call BGM_Ending_Noise_Sub1
    db $43,$43,$43,$43,$54,$54,$54,$54,$54,$54,$54,$54

BGM_Ending_Noise_Loop:
    call BGM_Ending_Noise_Sub2
    call BGM_Ending_Noise_Sub3
    call BGM_Ending_Noise_Sub2
    call BGM_Ending_Noise_Sub3
    call BGM_Ending_Noise_Sub2
    call BGM_Ending_Noise_Sub3
    call BGM_Ending_Noise_Sub2
    call BGM_Ending_Noise_Sub3
    call BGM_Ending_Noise_Sub2
    call BGM_Ending_Noise_Sub3
    call BGM_Ending_Noise_Sub2
    call BGM_Ending_Noise_Sub4
    call BGM_Ending_Noise_Sub2
    call BGM_Ending_Noise_Sub3
    call BGM_Ending_Noise_Sub2
    call BGM_Ending_Noise_Sub4
    jp BGM_Ending_Noise_Loop

BGM_Ending_Noise_Sub1:
    db $43,$43,$43,$43,$43,$43,$43,$43
    ret

BGM_Ending_Noise_Sub2:
    db $54,$E4,$14,$54,$44,$E4,$14,$54,$14,$E4,$54,$E4,$44,$E4,$14,$E4
    ret

BGM_Ending_Noise_Sub3:
    db $54,$E4,$14,$54,$44,$E4,$14,$54,$14,$E4,$54,$E4,$44,$E4,$44,$E4
    ret

BGM_Ending_Noise_Sub4:
    db $43,$04,$04,$54,$04,$04,$04,$54,$44,$44,$54,$04,$44,$54,$44
    ret
