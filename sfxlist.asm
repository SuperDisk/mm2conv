SFX_PulsePointer_Table:
    dw SFX_PulseSilence
    dw SFX_PulseRockmanLanding
    dw SFX_PulseRockmanShot
    dw SFX_PulseEnemyDamage
    dw SFX_PulseEnemyDestroy
    dw SFX_PulseDeflect
    dw SFX_PulseDestroyExplosion
    dw SFX_PulseRockmanGauge
    dw SFX_PulseECan
    dw SFX_PulseExtraLife
    dw SFX_PulseShutter
    dw SFX_PulseBossGauge
    dw SFX_PulseTeleportIn
    dw SFX_PulseTeleportOut
    dw SFX_PulseUnknown1
    dw SFX_PulseWeaponAbsorb
    dw SFX_PulseBunBlock
    dw SFX_PulseUFOCrash
    dw SFX_PulseMenuCursor
    dw SFX_PulseRockmanDamage
    dw SFX_PulseUnknown2
    dw SFX_PulseSilence2
    dw SFX_PulseSilence3
    dw SFX_PulseSilence4
    dw SFX_PulseSilence5
    dw SFX_PulseSilence6
    dw SFX_PulseSilence7
    dw SFX_PulseSilence8
    dw SFX_PulseSilence9
    dw SFX_PulseSilence10
    dw SFX_PulseSilence11
    dw SFX_PulseSilence12

SFX_NoisePointer_Table:
    dw SFX_NoiseSilence
    dw SFX_NoiseRockmanLanding
    dw SFX_NoiseRockmanShot
    dw SFX_NoiseEnemyDamage
    dw SFX_NoiseEnemyDestroy
    dw SFX_NoiseDeflect
    dw SFX_NoiseDestroyExplosion
    dw SFX_NoiseRockmanGauge
    dw SFX_NoiseECan
    dw SFX_NoiseExtraLife
    dw SFX_NoiseShutter
    dw SFX_NoiseBossGauge
    dw SFX_NoiseTeleportIn
    dw SFX_NoiseTeleportOut
    dw SFX_NoiseUnknown1
    dw SFX_NoiseWeaponAbsorb
    dw SFX_NoiseBunBlock
    dw SFX_NoiseUFOCrash
    dw SFX_NoiseMenuCursor
    dw SFX_NoiseRockmanDamage
    dw SFX_NoiseUnknown2
    dw SFX_NoiseSilence2
    dw SFX_NoiseSilence3
    dw SFX_NoiseSilence4
    dw SFX_NoiseSilence5
    dw SFX_NoiseSilence6
    dw SFX_NoiseSilence7
    dw SFX_NoiseSilence8
    dw SFX_NoiseSilence9
    dw SFX_NoiseSilence10
    dw SFX_NoiseSilence11
    dw SFX_NoiseSilence12

; -------- Sound Effect Format (Pulse 2) --------
;
;   -wx yz ## ##
;
;   w = Duty cycle+Base speed
;   (how "base speed" works to be determined, RMW2 only uses the "normal" ones)
;   0 = 12.5%, normal
;   1 = 12.5%, slow
;   2 = 12.5%, slower
;   3 = 12.5%, slowest
;   4 = 25%, normal
;   5 = 25%, slow
;   6 = 25%, slower
;   7 = 25%, slowest
;   8 = 50%, normal
;   9 = 50%, slow
;   A = 50%, slower
;   B = 50%, slowest
;   C = 75%, normal
;   D = 75%, slow
;   E = 75%, slower
;   F = 75%, slowest
;
;   x = How long a pitch lasts in frames
;   0, 1-F are valid
;	    0 = 256 Frames
;	    1 = 1 Frame
;	    2 = 2 Frames
;	    etc..
;	    E = 14 Frames
;	    F = 15 Frames
;
;   yz = Base volume and fade
;   Works the same way as the byte C2 reads for music
;
;   ## ## = Desired pitch	
;   Any of the valid GB channel frequencies (in little-endian format)
;
; -------- Sound Effect Format (Noise) --------
;
;   Currently unknown
;
