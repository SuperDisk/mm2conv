    dw Octave1
    dw Octave2
    dw Octave3
    dw Octave4
    dw Octave5
    dw Octave6
    dw Octave7

Octave1:
    dw $002c  ; C-2
    dw $009d  ; C#2
    dw $0107  ; D-2
    dw $016b  ; D#2
    dw $01c9  ; E-2
    dw $0222  ; F-2 -- Fixed tuning should be $0223
    dw $0277  ; F#2
    dw $02c7  ; G-2
    dw $0312  ; G#2
    dw $0358  ; A-2
    dw $039b  ; A#2
    dw $03da  ; B-2

Octave2:
    dw $0416  ; C-3
    dw $044e  ; C#3
    dw $0483  ; D-3
    dw $04b5  ; D#3
    dw $04e5  ; E-3
    dw $0511  ; F-3
    dw $053b  ; F#3
    dw $0563  ; G-3
    dw $0589  ; G#3
    dw $05ac  ; A-3
    dw $05ce  ; A#3
    dw $05ed  ; B-3

Octave3:
    dw $060b  ; C-4
    dw $0627  ; C#4
    dw $0642  ; D-4
    dw $065b  ; D#4
    dw $0672  ; E-4
    dw $0689  ; F-4
    dw $069e  ; F#4
    dw $06b2  ; G-4
    dw $06c4  ; G#4
    dw $06d6  ; A-4
    dw $06e7  ; A#4
    dw $06f7  ; B-4

Octave4:
    dw $0706  ; C-5
    dw $070f  ; C#5 -- Fixed tuning should be $0714 | This happens to be H.N's most egregious detuned note
    dw $0721  ; D-5
    dw $072d  ; D#5
    dw $0739  ; E-5
    dw $0744  ; F-5
    dw $074f  ; F#5
    dw $0759  ; G-5
    dw $0762  ; G#5
    dw $076b  ; A-5
    dw $0773  ; A#5
    dw $077b  ; B-5

Octave5:
    dw $0783  ; C-6
    dw $078a  ; C#6
    dw $0790  ; D-6
    dw $0797  ; D#6
    dw $079d  ; E-6
    dw $07a2  ; F-6
    dw $07a7  ; F#6
    dw $07ac  ; G-6
    dw $07b1  ; G#6
    dw $07b6  ; A-6
    dw $07ba  ; A#6
    dw $07be  ; B-6

Octave6:
    dw $07c1  ; C-7
    dw $07c5  ; C#7
    dw $07c8  ; D-7
    dw $07cb  ; D#7
    dw $07ce  ; E-7
    dw $07d1  ; F-7
    dw $07d4  ; F#7
    dw $07d6  ; G-7
    dw $07d9  ; G#7
    dw $07da  ; A-7 -- Fixed tuning should be $07db
    dw $07dd  ; A#7
    dw $07df  ; B-7

Octave7:
    dw $07e1  ; C-8
    dw $07e2  ; C#8
    dw $07e4  ; D-8
    dw $07e6  ; D#8
    dw $07e7  ; E-8
    dw $07e9  ; F-8
    dw $07ea  ; F#8
    dw $07eb  ; G-8
    dw $07ec  ; G#8
    dw $07ed  ; A-8
    dw $07ee  ; A#8
    dw $07ef  ; B-8
