BGM_StageClear:
    dw BGM_StageClear_Pulse1
    dw BGM_StageClear_Pulse2
    dw BGM_StageClear_Wave
    dw BGM_StageClear_Noise

; ==== Pulse 1 ====

BGM_StageClear_Pulse1:
    db $C4,$FF,$C0,$C4,$C1,$C0,$C2,$C3,$F4,$13,$54,$14,$54,$64,$84,$F5
    db $04,$11
    db $CE

; ==== Pulse 2 ====

BGM_StageClear_Pulse2:
    db $C1,$80,$C2,$C3,$F5,$14,$54,$64,$84,$54,$64,$84,$F6,$04,$F4,$84
    db $84,$8A
    db $CE

; ==== Wave ====

BGM_StageClear_Wave:
    db $C1,$E0,$C2,$20,$F2,$81,$54,$54,$5A
    db $CE

; ==== Noise ====

BGM_StageClear_Noise:
    db $C1,$01,$C2,$C1,$44,$44,$44,$44,$44,$44,$44,$44,$45,$45,$45,$45
    db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45
    db $CE
