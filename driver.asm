SECTION "Sound Driver", ROMX[$4000], BANK[$7]
; ----------------------------------------------------------------------
; Rockman World 2 Sound Driver - "Rabbit GB" Version 2.2 [Dec 20, 1991]
; Programmed by Hiroto Nakamura (alias H.N, Hakase)
; Disassembly by Pale Moon (alias Compass Man, Accuracy)    Oct 6, 2023
; ----------------------------------------------------------------------

; General Page Structure (for reference)
; 0-4DB     : Driver Code
; 4DC-4E9   : Frequency Table Octave Pointers
; 4EA-591   : Frequency Table
; 592-5D1   : BGM Pointer Table
; 5D2-651   : SFX Pointer Table
; 652-691   : BGM Wavetable Pointers
; 692-6D1   : Wavetables (16 bytes each)
; 6D2-6DD   : Noise Polynomials
; 6DE-3056  : BGM data
; 3057-3076 : Unknown
; 3077-3445 : SFX data
; 3466-3FFF : Padding (all $FF)

INCLUDE "hardware.asm"

;================ Driver Registers ================

; Main Control
BGM_Control equ $FF98
SFX_Control equ $FF99
BGM_CurrentID equ $FF9A

; Sequence Step Addresses
Pulse1SeqStepAddr_Lo equ $FFB8
Pulse1SeqStepAddr_Hi equ $FFB9
Pulse2SeqStepAddr_Lo equ $FFBA
Pulse2SeqStepAddr_Hi equ $FFBB
WaveSeqStepAddr_Lo equ $FFBC
WaveSeqStepAddr_Hi equ $FFBD
NoiseSeqStepAddr_Lo equ $FFBE
NoiseSeqStepAddr_Hi equ $FFBF

; Sub-Pattern Return Addresses
Pulse1SubPatternAddr_Lo equ $FFC0
Pulse1SubPatternAddr_Hi equ $FFC1
Pulse2SubPatternAddr_Lo equ $FFC2
Pulse2SubPatternAddr_Hi equ $FFC3
WaveSubPatternAddr_Lo equ $FFC4
WaveSubPatternAddr_Hi equ $FFC5
NoiseSubPatternAddr_Lo equ $FFC6
NoiseSubPatternAddr_Hi equ $FFC7

; Unknown
Lbl_ffc8 equ $FFC8
Lbl_ffc9 equ $FFC9
Lbl_ffca equ $FFCA
Lbl_ffcb equ $FFCB
Lbl_ffcc equ $FFCC
Lbl_ffcd equ $FFCD

; Sequence Step Timers
Pulse1StepTimer equ $FFD0
Pulse2StepTimer equ $FFD1
WaveStepTimer equ $FFD2
NoiseStepTimer equ $FFD3

; Unknown
Lbl_ffd4 equ $FFD4
Lbl_ffd5 equ $FFD5
Lbl_ffd6 equ $FFD6
Lbl_ffd7 equ $FFD7

; Initial Volume
Pulse1VolumeEnvelope1 equ $FFD8
Pulse2VolumeEnvelope1 equ $FFD9
WaveOutputLevel1 equ $FFDA
NoiseVolumeEnvelope1 equ $FFDB

; Pulse 1 States
Pulse1VolumeEnvelope2 equ $FFDC
Pulse1Frequency_Lo equ $FFDD
Pulse1Frequency_Hi equ $FFDE

; Pulse 2 States
Pulse2VolumeEnvelope2 equ $FFDF
Pulse2Frequency_Lo equ $FFE0
Pulse2Frequency_Hi equ $FFE1

; Wave States
WaveLength equ $FFE2
WaveOutputLevel2 equ $FFE3
WaveFrequency_Lo equ $FFE4
WaveFrequency_Hi equ $FFE5

; Noise States
NoiseLength equ $FFE6
NoiseVolumeEnvelope2 equ $FFE7
NoisePolynomial equ $FFE8
NoiseLoop equ $FFE9

; Other
StereoPanning equ $FFEA
Lbl_ffeb equ $FFEB
Lbl_ffec equ $FFEC
Lbl_ffed equ $FFED
Lbl_ffee equ $FFEE
Lbl_ffef equ $FFEF
Lbl_fff0 equ $FFF0
Lbl_fff1 equ $FFF1
Lbl_fff2 equ $FFF2
Lbl_fff4 equ $FFF4
Lbl_fff8 equ $FFF8

;================ Sound Driver ================

BGM_Update:
  ldh a, [BGM_Control]
  cp $ff                   ;Check if BGM ID was written
  jp z, SFX_Update
  ld b, a
  ldh a, [BGM_CurrentID]
  cp b                     ;Check if new ID matches the currently playing BGM's ID
  jp z, SFX_Update
;Load the song pointer
  ld a, b
  ldh [BGM_CurrentID], a
  add a, a
  ld e, a
  ld d, $00
  ld hl, BGM_Pointer_Table      ;$4592
  add hl, de
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  xor a
  ldh [NR52_FF26], a
  ld de, Pulse1SeqStepAddr_Lo             ;$ffb8
  ld b, $08
BGM_ChannelPointerRead:
  ld a, [hl+]
  ld [de], a
  inc de
  dec b
  jr nz, BGM_ChannelPointerRead
  ld a, $db
  ldh [Lbl_ffeb], a
  ldh [Lbl_fff0], a
  ld a, $44
  ldh [Lbl_ffec], a
  ldh [Lbl_fff1], a
  ld a, $d0
  ldh [TIMA_FF05], a
  ldh [TMA_FF06], a
  ld a, $32
  ldh [Lbl_ffc8], a
  ldh [Lbl_ffca], a
  ldh [Lbl_ffcc], a
  ld a, $45
  ldh [Lbl_ffc9], a
  ldh [Lbl_ffcb], a
  ldh [Lbl_ffcd], a
  ld a, $04
  ldh [Lbl_ffd4], a
  ldh [Lbl_ffd5], a
  ldh [Lbl_ffd6], a
  ldh [Lbl_ffd7], a
  ld a, $80
  ldh [Pulse1Frequency_Hi], a
  ldh [Pulse2Frequency_Hi], a
  ld a, $c0
  ldh [NoiseLoop], a
  xor a
  ldh [WaveFrequency_Hi], a
  ldh [Pulse1StepTimer], a
  ldh [Pulse2StepTimer], a
  ldh [WaveStepTimer], a
  ldh [NoiseStepTimer], a
  ldh [Lbl_fff2], a
  ld a, $80
  ldh [NR52_FF26], a
;Get wavetable pointer for the BGM track
  ldh a, [BGM_Control]
  add a, a
  ld c, a
  ld b, $00
  ld hl, BGM_WavetablePointer_Table
  add hl, bc
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  ld bc, $1030      ; -!-
BGM_LoadWavetable:
  ld a, [hl+]
  ld [$ff00+c], a
  inc c
  dec b
  jr nz, BGM_LoadWavetable
;Clear status of audio registers
  ld a, $ff
  ldh [StereoPanning], a
  ldh [NR51_FF25], a
  ld a, $77
  ldh [NR50_FF24], a
  ld a, $08
  ldh [NR10_FF10], a
  ld a, $80
  ldh [NR30_FF1A], a
  xor a
  ldh [NR32_FF1C], a
  ld a, $80
  ldh [NR34_FF1E], a
  ld a, $ff
  ldh [BGM_Control], a
SFX_Update:
  ldh a, [SFX_Control]
  cp $ff                   ;Check if SFX ID was written
  jr z, Pulse1StepCheck
  ld c, a
  ldh a, [Lbl_ffed]
  ld b, a
  ldh a, [Lbl_fff2]
  or b
  jr z, Lbl_40c2
  ld hl, Unknown_Table
  ld b, $00
  add hl, bc
  ld b, [hl]
  ldh a, [Lbl_fff8]
  cp b
  jr c, Lbl_40ee
  ld a, b
  ldh [Lbl_fff8], a
Lbl_40c2:
  ldh a, [SFX_Control]
  add a, a
  ld e, a
  ld d, $00
  ld hl, SFX_PulsePointer_Table
  add hl, de
  ld a, [hl+]
  or a
  jr z, Lbl_40db        ;$40db
  ldh [Lbl_ffeb], a
  ld a, [hl]
  ldh [Lbl_ffec], a
  xor a
  ldh [Lbl_ffed], a
  inc a
  ldh [Lbl_ffef], a
Lbl_40db:
  ld hl, SFX_NoisePointer_Table             ;$4612 = SFX_NoisePointer_Table
  add hl, de
  ld a, [hl+]
  or a
  jr z, Lbl_40ee
  ldh [Lbl_fff0], a
  ld a, [hl]
  ldh [Lbl_fff1], a
  xor a
  ldh [Lbl_fff2], a
  inc a
  ldh [Lbl_fff4], a
Lbl_40ee:
  ld a, $ff
  ldh [SFX_Control], a

;================ Pulse 1 Handling ================
Pulse1StepCheck:
  ldh a, [Pulse1StepTimer]
  or a
  jp nz, Pulse1StepProcessing
  ldh a, [Pulse1SeqStepAddr_Lo]
  ld l, a
  ldh a, [Pulse1SeqStepAddr_Hi]
  ld h, a
Pulse1SeqByteProcess:
  ld a, [hl+]
  ld c, a
  swap a
  and $0f
  cp $0c                   ;Check if $0C or higher
  jr nc, Pulse1Command
  add a, a
  ld e, a
  ldh a, [Lbl_ffc8]
  add a, e
  ld e, a
  ldh a, [Lbl_ffc9]
  adc a, $00
  ld d, a
  ld a, [de]
  ldh [Pulse1Frequency_Lo], a
  inc de
  ld a, [de]
  or $80
  ldh [Pulse1Frequency_Hi], a
  ldh a, [Pulse1VolumeEnvelope1]
  ldh [Pulse1VolumeEnvelope2], a
  call Sub_44c5
  ldh [Pulse1StepTimer], a
  jp Pulse1Update
Pulse1Command:
  jr nz, Pulse1Delay       ;Jumps if $Dx-$Fx
  ld a, c
  and $0f
  jr nz, Pulse1SetDutyCycle
  ld a, [hl+]              ;Set Timer speed if $C0 was used
  ldh [TIMA_FF05], a
  ldh [TMA_FF06], a
  jr Pulse1SeqByteProcess
Pulse1SetDutyCycle:
  dec a
  jr nz, Pulse1SetVolumeEnvelope
  ld a, [hl+]
  ldh [NR11_FF11], a
  jr Pulse1SeqByteProcess
Pulse1SetVolumeEnvelope:
  dec a
  jr nz, Pulse1Jump
  ld a, [hl+]
  ldh [Pulse1VolumeEnvelope1], a
  jr Pulse1SeqByteProcess
Pulse1Jump:
  dec a
  jr nz, Pulse1SetStereoPanning
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  jr Pulse1SeqByteProcess
Pulse1SetStereoPanning:
  dec a
  jr nz, Pulse1SubReturn
  ld a, [hl+]
  ldh [StereoPanning], a
  ldh [NR51_FF25], a
  jr Pulse1SeqByteProcess
Pulse1SubReturn:
  cp $05
  jr nz, Pulse1SubJump
  ldh a, [Pulse1SubPatternAddr_Lo]
  ld l, a
  ldh a, [Pulse1SubPatternAddr_Hi]
  ld h, a
  jp Pulse1SeqByteProcess
Pulse1SubJump:
  cp $09
  jr nz, Pulse1Stop
  ld a, [hl+]
  ld e, a
  ld a, [hl+]
  ld d, a
  ld a, l
  ldh [Pulse1SubPatternAddr_Lo], a
  ld a, h
  ldh [Pulse1SubPatternAddr_Hi], a
  ld l, e
  ld h, d
  jp Pulse1SeqByteProcess
Pulse1Stop:
  ld a, $08
  ldh [Pulse1VolumeEnvelope2], a
  dec hl
  jr Pulse1Update
Pulse1Delay:
  cp $0d
  jr nz, Pulse1Rest
  call Sub_44c5
  ldh [Pulse1StepTimer], a
  jr Pulse1SeqStepAddrLoad
Pulse1Rest:
  cp $0e
  jr nz, Pulse1OctaveCheck
  ld a, $08
  ldh [Pulse1VolumeEnvelope2], a
  call Sub_44c5
  ldh [Pulse1StepTimer], a
  jr Pulse1Update
Pulse1OctaveCheck:
  ld a, c
  and $07
  bit 3, c
  jr z, Lbl_41a9
  cp $04
  jr c, Lbl_41a5
  ld a, c
Lbl_41a5:
  ld b, a
  ldh a, [Lbl_ffd4]
  adc a, b
Lbl_41a9:
  ldh [Lbl_ffd4], a
  dec a
  add a, a
  add a, $dc
  ld e, a
  ld a, $00
  adc a, $44
  ld d, a
  ld a, [de]
  ldh [Lbl_ffc8], a
  inc de
  ld a, [de]
  ldh [Lbl_ffc9], a
  jp Pulse1SeqByteProcess
Pulse1Update:
  ldh a, [Pulse1VolumeEnvelope2]
  ldh [NR12_FF12], a
  ldh a, [Pulse1Frequency_Lo]
  ldh [NR13_FF13], a
  ldh a, [Pulse1Frequency_Hi]
  ldh [NR14_FF14], a
Pulse1SeqStepAddrLoad:
  ld a, l
  ldh [Pulse1SeqStepAddr_Lo], a
  ld a, h
  ldh [Pulse1SeqStepAddr_Hi], a
Pulse1StepProcessing:
  ld hl, Pulse1StepTimer
  dec [hl]

;================ Pulse 2 Handling ================
  ldh a, [Pulse2StepTimer]
  or a
  jp nz, Pulse2StepProcessing
  ldh a, [Pulse2SeqStepAddr_Lo]
  ld l, a
  ldh a, [Pulse2SeqStepAddr_Hi]
  ld h, a
Pulse2SeqByteProcessing:
  ld a, [hl+]
  ld c, a
  swap a
  and $0f
  cp $0c
  jr nc, Pulse2Command
  add a, a
  ld e, a
  ldh a, [Lbl_ffca]
  add a, e
  ld e, a
  ldh a, [Lbl_ffcb]
  adc a, $00
  ld d, a
  ld a, [de]
  ldh [Pulse2Frequency_Lo], a
  inc de
  ld a, [de]
  or $80
  ldh [Pulse2Frequency_Hi], a
  ldh a, [Pulse2VolumeEnvelope1]
  ldh [Pulse2VolumeEnvelope2], a
  call Sub_44c5
  ldh [Pulse2StepTimer], a
  jp Pulse2Update
Pulse2Command:
  jr nz, Pulse2Delay
  ld a, c
  and $0f
  jr nz, Pulse2SetDutyCycle
  ld a, [hl+]
  ldh [TIMA_FF05], a
  ldh [TMA_FF06], a
  jr Pulse2SeqByteProcessing
Pulse2SetDutyCycle:
  dec a
  jr nz, Pulse2SetVolumeEnvelope
  ld a, [hl+]
  ldh [NR21_FF16], a
  jr Pulse2SeqByteProcessing
Pulse2SetVolumeEnvelope:
  dec a
  jr nz, Pulse2Jump
  ld a, [hl+]
  ldh [Pulse2VolumeEnvelope1], a
  jr Pulse2SeqByteProcessing
Pulse2Jump:
  dec a
  jr nz, Pulse2SubReturn
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  jr Pulse2SeqByteProcessing
Pulse2SubReturn:
  cp $06
  jr nz, Pulse2SubJump
  ldh a, [Pulse2SubPatternAddr_Lo]
  ld l, a
  ldh a, [Pulse2SubPatternAddr_Hi]
  ld h, a
  jp Pulse2SeqByteProcessing
Pulse2SubJump:
  cp $0a
  jr nz, Lbl_4251
  ld a, [hl+]
  ld e, a
  ld a, [hl+]
  ld d, a
  ld a, l
  ldh [Pulse2SubPatternAddr_Lo], a
  ld a, h
  ldh [Pulse2SubPatternAddr_Hi], a
  ld l, e
  ld h, d
  jp Pulse2SeqByteProcessing
Lbl_4251:
  ld a, $08
  ldh [Pulse2VolumeEnvelope2], a
  dec hl
  jr Pulse2Update
Pulse2Delay:
  cp $0d
  jr nz, Lbl_4263
  call Sub_44c5
  ldh [Pulse2StepTimer], a
  jr Pulse2SeqStepAddrLoad
Lbl_4263:
  cp $0e
  jr nz, Lbl_4272
  ld a, $08
  ldh [Pulse2VolumeEnvelope2], a
  call Sub_44c5
  ldh [Pulse2StepTimer], a
  jr Pulse2Update
Lbl_4272:
  ld a, c
  and $07
  bit 3, c
  jr z, Lbl_4282
  cp $04
  jr c, Lbl_427e
  ld a, c
Lbl_427e:
  ld b, a
  ldh a, [Lbl_ffd5]
  adc a, b
Lbl_4282:
  ldh [Lbl_ffd5], a
  dec a
  add a, a
  add a, $dc
  ld e, a
  ld a, $00
  adc a, $44
  ld d, a
  ld a, [de]
  ldh [Lbl_ffca], a
  inc de
  ld a, [de]
  ldh [Lbl_ffcb], a
  jp Pulse2SeqByteProcessing
Pulse2Update:
  ldh a, [Lbl_ffef]
  or a
  jr nz, Pulse2SeqStepAddrLoad
  ldh a, [Pulse2VolumeEnvelope2]
  ldh [NR22_FF17], a
  ldh a, [Pulse2Frequency_Lo]
  ldh [NR23_FF18], a
  ldh a, [Pulse2Frequency_Hi]
  ldh [NR24_FF19], a
Pulse2SeqStepAddrLoad:
  ld a, l
  ldh [Pulse2SeqStepAddr_Lo], a
  ld a, h
  ldh [Pulse2SeqStepAddr_Hi], a
Pulse2StepProcessing:
  ld hl, Pulse2StepTimer
  dec [hl]
  ldh a, [Lbl_ffef]
  or a
  jr z, WaveStepCheck
  ldh a, [Lbl_ffed]
  or a
  jr nz, Lbl_42eb
  ldh a, [Lbl_ffeb]
  ld l, a
  ldh a, [Lbl_ffec]
  ld h, a
  ld a, [hl+]
  or a
  jr nz, Lbl_42d4
  ld a, $08
  ldh [NR22_FF17], a
  swap a
  ldh [NR24_FF19], a
  xor a
  ldh [Lbl_ffef], a
  jr WaveStepCheck
Lbl_42d4:
  ldh [NR21_FF16], a
  and $3f
  ldh [Lbl_ffed], a
  ld a, [hl+]
  ldh [NR22_FF17], a
  ld a, [hl+]
  ldh [NR23_FF18], a
  ld a, [hl+]
  or $80
  ldh [NR24_FF19], a
  ld a, l
  ldh [Lbl_ffeb], a
  ld a, h
  ldh [Lbl_ffec], a
Lbl_42eb:
  ld hl, Lbl_ffed
  dec [hl]

;================ Wave Handling ================
WaveStepCheck:
  ldh a, [WaveStepTimer]
  or a
  jp nz, WaveStepProcessing
  ldh a, [WaveSeqStepAddr_Lo]
  ld l, a
  ldh a, [WaveSeqStepAddr_Hi]
  ld h, a
WaveSeqByteProcessing:
  ld a, [hl+]
  ld c, a
  swap a
  and $0f
  cp $0c
  jr nc, WaveCommand
  add a, a
  ld e, a
  ldh a, [Lbl_ffcc]
  add a, e
  ld e, a
  ldh a, [Lbl_ffcd]
  adc a, $00
  ld d, a
  ld a, [de]
  ldh [WaveFrequency_Lo], a
  inc de
  ld a, [de]
  or $c0
  ldh [WaveFrequency_Hi], a
  ldh a, [WaveOutputLevel1]
  ldh [WaveOutputLevel2], a
  call Sub_44c5
  ldh [WaveStepTimer], a
  jp WaveUpdate
WaveCommand:
  jr nz, WaveDelay
  ld a, c
  and $0f
  jr nz, WaveSetLength
  ld a, [hl+]
  ldh [TIMA_FF05], a
  ldh [TMA_FF06], a
  jr WaveSeqByteProcessing
WaveSetLength:
  dec a
  jr nz, WaveSetVolumeLevel
  ld a, [hl+]
  ldh [WaveLength], a
  jr WaveSeqByteProcessing
WaveSetVolumeLevel:
  dec a
  jr nz, WaveJump
  ld a, [hl+]
  ldh [WaveOutputLevel1], a
  jr WaveSeqByteProcessing
WaveJump:
  dec a
  jr nz, WaveSubReturn
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  jr WaveSeqByteProcessing
WaveSubReturn:
  cp $06
  jr nz, WaveSubJump
  ldh a, [WaveSubPatternAddr_Lo]
  ld l, a
  ldh a, [WaveSubPatternAddr_Hi]
  ld h, a
  jp WaveSeqByteProcessing
WaveSubJump:
  cp $0a
  jr nz, Lbl_436b
  ld a, [hl+]
  ld e, a
  ld a, [hl+]
  ld d, a
  ld a, l
  ldh [WaveSubPatternAddr_Lo], a
  ld a, h
  ldh [WaveSubPatternAddr_Hi], a
  ld l, e
  ld h, d
  jp WaveSeqByteProcessing
Lbl_436b:
  xor a
  ldh [WaveOutputLevel2], a
  dec hl
  jr WaveUpdate
WaveDelay:
  cp $0d
  jr nz, Lbl_437c
  call Sub_44c5
  ldh [WaveStepTimer], a
  jr WaveSeqStepAddrLoad
Lbl_437c:
  cp $0e
  jr nz, Lbl_438a
  xor a
  ldh [WaveOutputLevel2], a
  call Sub_44c5
  ldh [WaveStepTimer], a
  jr WaveUpdate
Lbl_438a:
  ld a, c
  and $07
  bit 3, c
  jr z, Lbl_439a
  cp $04
  jr c, Lbl_4396
  ld a, c
Lbl_4396:
  ld b, a
  ldh a, [Lbl_ffd6]
  adc a, b
Lbl_439a:
  ldh [Lbl_ffd6], a
  dec a
  add a, a
  add a, $dc
  ld e, a
  ld a, $00
  adc a, $44
  ld d, a
  ld a, [de]
  ldh [Lbl_ffcc], a
  inc de
  ld a, [de]
  ldh [Lbl_ffcd], a
  jp WaveSeqByteProcessing
WaveUpdate:
  ldh a, [WaveLength]
  ldh [NR31_FF1B], a
  ldh a, [WaveOutputLevel2]
  ldh [NR32_FF1C], a
  ldh a, [WaveFrequency_Lo]
  ldh [NR33_FF1D], a
  ldh a, [WaveFrequency_Hi]
  ldh [NR34_FF1E], a
WaveSeqStepAddrLoad:
  ld a, l
  ldh [WaveSeqStepAddr_Lo], a
  ld a, h
  ldh [WaveSeqStepAddr_Hi], a
WaveStepProcessing:
  ld hl, WaveStepTimer
  dec [hl]

;================ Noise Handling ================
  ldh a, [NoiseStepTimer]
  or a
  jp nz, NoiseStepProcessing
  ldh a, [NoiseSeqStepAddr_Lo]
  ld l, a
  ldh a, [NoiseSeqStepAddr_Hi]
  ld h, a
NoiseSeqByteProcessing:
  ld a, [hl+]
  ld c, a
  swap a
  and $0f
  cp $0c                   ;Check if $0C or higher
  jr nc, NoiseCommand
  ldh a, [NoiseVolumeEnvelope1]
  ldh [NoiseVolumeEnvelope2], a
  ld de, NoisePolynomial_Table
  ld a, c
  and $f0
  swap a
  add a, e
  ld e, a
  ld a, d
  adc a, $00
  ld d, a
  ld a, [de]
  ldh [NoisePolynomial], a
  ld a, $c0
  ldh [NoiseLoop], a
  call Sub_44c5
  ldh [NoiseStepTimer], a
  jp NoiseUpdate
NoiseCommand:
  jr nz, NoiseDelay
  ld a, c
  and $0f
  jr nz, NoiseSetLength
  ld a, [hl+]
  ldh [TIMA_FF05], a
  ldh [TMA_FF06], a
  jr NoiseSeqByteProcessing
NoiseSetLength:
  dec a
  jr nz, NoiseSetVolumeEnvelope
  ld a, [hl+]
  ldh [NoiseLength], a
  jr NoiseSeqByteProcessing
NoiseSetVolumeEnvelope:
  dec a
  jr nz, NoiseJump
  ld a, [hl+]
  ldh [NoiseVolumeEnvelope1], a
  jr NoiseSeqByteProcessing
NoiseJump:
  dec a
  jr nz, NoiseSubReturn
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  jr NoiseSeqByteProcessing
NoiseSubReturn:
  cp $06
  jr nz, NoiseSubJump
  ldh a, [NoiseSubPatternAddr_Lo]
  ld l, a
  ldh a, [NoiseSubPatternAddr_Hi]
  ld h, a
  jp NoiseSeqByteProcessing
NoiseSubJump:
  cp $0a
  jr nz, Lbl_4447
  ld a, [hl+]
  ld e, a
  ld a, [hl+]
  ld d, a
  ld a, l
  ldh [NoiseSubPatternAddr_Lo], a
  ld a, h
  ldh [NoiseSubPatternAddr_Hi], a
  ld l, e
  ld h, d
  jp NoiseSeqByteProcessing
Lbl_4447:
  ld a, $08
  ldh [NoiseVolumeEnvelope2], a
  dec hl
  jr NoiseUpdate
NoiseDelay:
  cp $0d
  jr nz, Lbl_4459
  call Sub_44c5
  ldh [NoiseStepTimer], a
  jr NoiseSeqStepAddrLoad
Lbl_4459:
  cp $0e
  jr nz, NoiseUpdate
  ld a, $08
  ldh [NoiseVolumeEnvelope2], a
  call Sub_44c5
  ldh [NoiseStepTimer], a
  jr NoiseUpdate
  jp NoiseSeqByteProcessing
NoiseUpdate:
  ldh a, [Lbl_fff4]
  or a
  jr nz, NoiseSeqStepAddrLoad
  ldh a, [NoiseLength]
  ldh [NR41_FF20], a
  ldh a, [NoiseVolumeEnvelope2]
  ldh [NR42_FF21], a
  ldh a, [NoisePolynomial]
  ldh [NR43_FF22], a
  ldh a, [NoiseLoop]
  ldh [NR44_FF23], a
NoiseSeqStepAddrLoad:
  ld a, l
  ldh [NoiseSeqStepAddr_Lo], a
  ld a, h
  ldh [NoiseSeqStepAddr_Hi], a
NoiseStepProcessing:
  ld hl, NoiseStepTimer
  dec [hl]
  ldh a, [Lbl_fff4]
  or a
  ret z                    ;Finish

  ldh a, [Lbl_fff2]
  or a
  jr nz, Lbl_44c0
  ldh a, [Lbl_fff0]
  ld l, a
  ldh a, [Lbl_fff1]
  ld h, a
  ld a, [hl+]
  or a
  jr nz, Lbl_44a9
  ld a, $08
  ldh [NR42_FF21], a
  swap a
  ldh [NR44_FF23], a
  xor a
  ldh [Lbl_fff4], a
  ret

Lbl_44a9:
  ldh [NR41_FF20], a
  and $3f
  ldh [Lbl_fff2], a
  ld a, [hl+]
  ldh [NR42_FF21], a
  ld a, [hl+]
  ldh [NR43_FF22], a
  ld a, [hl+]
  or $80
  ldh [NR44_FF23], a
  ld a, l
  ldh [Lbl_fff0], a
  ld a, h
  ldh [Lbl_fff1], a
Lbl_44c0:
  ld hl, Lbl_fff2
  dec [hl]
  ret

Sub_44c5:
  ld a, c
  and $07
  ld b, a
  ld a, $80
  jr z, Lbl_44d2
Lbl_44cd:
  srl a
  dec b
  jr nz, Lbl_44cd
Lbl_44d2:
  ld b, a
  srl b
  bit 3, c
  jr z, Lbl_44da
  add a, b
Lbl_44da:
  ret

  db $00  ; End of code marker

; ==== Essential Data ====
INCLUDE "frequency.asm"
INCLUDE "bgmlist.asm"
INCLUDE "sfxlist.asm"
INCLUDE "wavetable.asm"
INCLUDE "noise.asm"

; ==== BGM Sequences ====
INCLUDE "bgm/BGM_Title.asm"
INCLUDE "bgm/BGM_StageSelect.asm"
INCLUDE "bgm/BGM_StageStart.asm"
INCLUDE "bgm/BGM_Password.asm"
INCLUDE "bgm/BGM_Boss.asm"
INCLUDE "bgm/BGM_StageClear.asm"
INCLUDE "bgm/BGM_WeaponGet.asm"
INCLUDE "bgm/BGM_ClashMan.asm"
INCLUDE "bgm/BGM_AirMan.asm"
INCLUDE "bgm/BGM_MetalMan.asm"
INCLUDE "bgm/BGM_HardMan.asm"
INCLUDE "bgm/BGM_WoodMan.asm"
INCLUDE "bgm/BGM_TopMan.asm"
INCLUDE "bgm/BGM_MagnetMan.asm"
INCLUDE "bgm/BGM_NeedleMan.asm"
INCLUDE "bgm/BGM_WilyIntro.asm"
INCLUDE "bgm/BGM_WilyCastle.asm"
INCLUDE "bgm/BGM_Ending.asm"
INCLUDE "bgm/BGM_UnusedEnding.asm"
INCLUDE "bgm/BGM_Silence.asm"

; ==== Unknown Table ====
Unknown_Table:
    db $00,$07,$08,$04,$03,$05,$00,$01,$01,$01,$00,$00,$00,$00,$00,$00
    db $01,$00,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; ==== SFX Sequences ====
INCLUDE "sfx/SFX_RockmanLanding.asm"
INCLUDE "sfx/SFX_RockmanShot.asm"
INCLUDE "sfx/SFX_EnemyDamage.asm"
INCLUDE "sfx/SFX_EnemyDestroy.asm"
INCLUDE "sfx/SFX_Deflect.asm"
INCLUDE "sfx/SFX_DestroyExplosion.asm"
INCLUDE "sfx/SFX_RockmanGauge.asm"
INCLUDE "sfx/SFX_ECan.asm"
INCLUDE "sfx/SFX_ExtraLife.asm"
INCLUDE "sfx/SFX_Shutter.asm"
INCLUDE "sfx/SFX_BossGauge.asm"
INCLUDE "sfx/SFX_TeleportIn.asm"
INCLUDE "sfx/SFX_TeleportOut.asm"
INCLUDE "sfx/SFX_Unknown1.asm"
INCLUDE "sfx/SFX_WeaponAbsorb.asm"
INCLUDE "sfx/SFX_BunBlock.asm"
INCLUDE "sfx/SFX_UFOCrash.asm"
INCLUDE "sfx/SFX_MenuCursor.asm"
INCLUDE "sfx/SFX_RockmanDamage.asm"
INCLUDE "sfx/SFX_Unknown2.asm"
INCLUDE "sfx/SFX_Silence.asm"

    ds 3002, $FF    ; Padding, matches the same amount as vanilla
                    ; Comment this out if doing custom code shenanigans
