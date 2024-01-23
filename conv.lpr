program conv;

{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, LazUTF8, fgl, math, strutils, iostream;

type
  ETBMException = class(Exception);

type
  TTBMHeader = packed record
    Signature: array[0..11] of Char; // ' TRACKERBOY '
    VersionMajor: DWord;
    VersionMinor: DWord;
    VersionPatch: DWord;
    MRev: Byte;
    NRev: Byte;
    Reserved1: Word;

    Title: array[0..31] of Char;
    Artist: array[0..31] of Char;
    Copyright: array[0..31] of Char;

    ICount: Byte;
    SCount: Byte;
    WCount: Byte;
    System: Byte;
    CustomFrameRate: Word;

    Reserved2: array[0..29] of Byte;
  end;

  TTBMBlockHeader = packed record
    Id: array[0..3] of Char;
    Length: DWord;
  end;

  TTBMSongFormat = packed record
    RowsPerBeat: Byte;
    RowsPerMeasure: Byte;
    Speed: Byte;
    PatternCount: Byte;
    RowsPerTrack: Byte;
    NumberOfTracks: Word;
  end;

  TTBMTrackFormat = packed record
    Channel: Byte;
    TrackId: Byte;
    Rows: Byte;
  end;

  TTBMEffect = packed record
    EffectType: Byte;
    Param: Byte;
  end;

  TTBMTrackRow = packed record
    Note: Byte;
    Instrument: Byte;
    Effects: array[0..2] of TTBMEffect;
  end;

  TTBMRowFormat = packed record
    RowNo: Byte;
    RowData: TTBMTrackRow;
  end;

  TTBMInstrumentFormat = packed record
    Channel: Byte;
    EnvelopeEnabled: Byte;
    Envelope: Byte;
  end;

  TTBMSequenceFormat = packed record
    Length: Word;
    LoopEnabled: Byte;
    LoopIndex: Byte;
  end;

  TPattern = array[0..63] of TTBMTrackRow;
  PPattern = ^TPattern;

  TOrderMatrix = array[0..3] of array of Integer;

  TPatternMap = specialize TFPGMap<Integer, PPattern>;

  TStreamHelper = class helper for TStream
    function ReadLString: String;
  end;

  TWait = record
    Code: String;
    Frames: Integer;
  end;

  TByteArray = array of Byte;

  //https://github.com/stoneface86/libtrackerboy/blob/bf4993d53bc34691ca75819a96d3d00b3b699dea/src/trackerboy/data.nim#L111
  TTBMEffectType = (
    etNoEffect = 0,         // No effect, this effect column is unset.
    etPatternGoto = 1,          // `Bxx` begin playing given pattern immediately
    etPatternHalt = 2,          // `C00` stop playing
    etPatternSkip = 3,          // `D00` begin playing next pattern immediately
    etSetTempo = 4,             // `Fxx` set the tempo
    etSfx = 5,                  // `Txx` play sound effect
    etSetEnvelope = 6,          // `Exx` set the persistent envelope/wave id setting
    etSetTimbre = 7,            // `Vxx` set persistent duty/wave volume setting
    etSetPanning = 8,           // `Ixy` set channel panning setting
    etSetSweep = 9,             // `Hxx` set the persistent sweep setting (CH1 only)
    etDelayedCut = 10,           // `Sxx` note cut delayed by xx frames
    etDelayedNote = 11,          // `Gxx` note trigger delayed by xx frames
    etLock = 12,                 // `L00` (lock) stop the sound effect on the current channel
    etArpeggio = 13,             // `0xy` arpeggio with semi tones x and y
    etPitchUp = 14,              // `1xx` pitch slide up
    etPitchDown = 15,            // `2xx` pitch slide down
    etAutoPortamento = 16,       // `3xx` automatic portamento
    etVibrato = 17,              // `4xy` vibrato
    etVibratoDelay = 18,         // `5xx` delay vibrato xx frames on note trigger
    etTuning = 19,               // `Pxx` fine tuning
    etNoteSlideUp = 20,          // `Qxy` note slide up
    etNoteSlideDown = 21,        // `Rxy` note slide down
    etSetGlobalVolume = 22       // `Jxy` set global volume scale
  );

const
  ContinuousEffects = [
    etArpeggio,
    etPitchUp,
    etPitchDown,
    etAutoPortamento,
    etVibrato
  ];

  Waits: array of TWait = (
    (Code: '8'; Frames: 192),
    (Code: '0'; Frames: 128),
    (Code: '9'; Frames: 96),
    (Code: '1'; Frames: 64),
    (Code: 'A'; Frames: 48),
    (Code: '2'; Frames: 32),
    (Code: 'B'; Frames: 24),
    (Code: '3'; Frames: 16),
    (Code: 'C'; Frames: 12),
    (Code: '4'; Frames: 8),
    (Code: 'D'; Frames: 6),
    (Code: '5'; Frames: 4),
    (Code: 'E'; Frames: 3),
    (Code: '6'; Frames: 2),
    (Code: '7'; Frames: 1),
    (Code: 'F'; Frames: 1)
  );

  NoiseNoteTable: array of Byte = (
  // C       Db    D     Eb    E     F     Gb    G     Ab    A     Bb    B
    $D7,    $D6,  $D5,  $D4,  $C7,  $C6,  $C5,  $C4,  $B7,  $B6,  $B5,  $B4,   // 2
    $A7,    $A6,  $A5,  $A4,  $97,  $96,  $95,  $94,  $87,  $86,  $85,  $84,   // 3
    $77,    $76,  $75,  $74,  $67,  $66,  $65,  $64,  $57,  $56,  $55,  $54,   // 4
    $47,    $46,  $45,  $44,  $37,  $36,  $35,  $34,  $27,  $26,  $25,  $24,   // 5
    $17,    $16,  $15,  $14,  $07,  $06,  $05,  $04,  $03,  $02,  $01,  $00    // 6
  );

function TicksToCode(Ticks: Integer): String;
var
  W: TWait;
begin
  for W in Waits do
    if W.Frames = Ticks then Exit(W.Code);

  Result := 'D';
end;

function TStreamHelper.ReadLString: String;
var
  Len: Word;
  Buf: array of byte;
begin
  Self.ReadBuffer(Len, SizeOf(Word));
  SetLength(Buf, Len);
  Self.ReadBuffer(Buf[0], Len);
  Result := UTF8CStringToUTF8String(@Buf[0], Len);
end;

function FetchPattern(M: TPatternMap; Idx: Integer): PPattern;
begin
  if M.IndexOf(Idx) <> -1 then
    Result := M.KeyData[Idx]
  else begin
    New(Result);
    M.Add(Idx, Result);
  end;
end;

function IsEmptyRow(Row: TTBMTrackRow): Boolean;
begin
  Result :=
    (Row.Note = 0)
    and (Row.Effects[0].EffectType = 0) and (Row.Effects[0].Param = 0)
    and (Row.Effects[1].EffectType = 0) and (Row.Effects[1].Param = 0)
    and (Row.Effects[2].EffectType = 0) and (Row.Effects[2].Param = 0);
end;

function SetAdd(var S: TByteArray; El: Byte): Integer;
var
  I: Integer;
begin
  for I := Low(S) to High(S) do
    if S[I] = El then Exit(I);

  SetLength(S, Length(S)+1);
  S[High(S)] := El;
  Result := High(S);
end;

var
  Stream: TStream;
  I, J: Integer;

  UsedNoise: TByteArray;
  B: Byte;

  Header: TTBMHeader;
  BlockHeader: TTBMBlockHeader;
  SongFormat: TTBMSongFormat;
  TrackFormat: TTBMTrackFormat;
  RowFormat: TTBMRowFormat;

  TicksPerRow: Integer;
  OrderMatrix: TOrderMatrix;

  Row: TTBMTrackRow;

  Pat: PPattern;
  Patterns: TPatternMap;

  procedure Wait(Rows: Integer);
  var
    Frames: Integer;
    W: TWait;
  begin
    Frames := Rows * TicksPerRow;

    while Frames > 0 do begin
      for W in Waits do
        if W.Frames <= Frames then begin
          Dec(Frames, W.Frames);
          Writeln(' wait $'+W.Code);
          Break;
        end;
    end;
  end;

  procedure Emit(Ch: Integer);
  var
    Octave: Integer;
    Note: Integer = 0;
    FX: TTBMEffect;
    Waited: Integer = 0;
    OldOctave: Integer = -1;
    NoiseVal: Byte;
    DidEnvelope: Boolean;
  begin
    for I := Low(OrderMatrix[Ch]) to High(OrderMatrix[Ch])-1 do begin
      writeln('; pattern break');
      Pat := FetchPattern(Patterns, OrderMatrix[Ch, I]);

      for Row in Pat^ do begin
        DidEnvelope := False;

        if (not IsEmptyRow(Row)) and (Waited > 0) then begin
          Wait(Waited);
          Waited := 0;
        end;

        for FX in Row.Effects do begin
          case TTBMEffectType(FX.EffectType) of
            etSetEnvelope: begin
              WriteLn(' envelope $', IntToHex(FX.Param, 2));
              DidEnvelope := True;
            end;
            etSetTimbre: begin
              if Ch = 2 then begin
                case FX.Param of
                  0: WriteLn(' wave_vol $00');
                  1: WriteLn(' wave_vol $60');
                  2: WriteLn(' wave_vol $40');
                  3: WriteLn(' wave_vol $20');
                end;
                DidEnvelope := True;
              end
              else
                case FX.Param of
                  0: WriteLn(' duty_cycle $00');
                  1: WriteLn(' duty_cycle $40');
                  2: WriteLn(' duty_cycle $80');
                  3: WriteLn(' duty_cycle $C0');
                end;
            end;
            //etPatternSkip: Break;
          end;
        end;

        if IsEmptyRow(Row) then
          Inc(Waited)
        else begin
          if (Row.Note = 85) then begin
            WriteLn(' silence $', TicksToCode(TicksPerRow));
          end else begin
            if (Ch = 3) then begin
              NoiseVal := NoiseNoteTable[Row.Note];
              WriteLn(' note $'+IntToHex(SetAdd(UsedNoise, NoiseVal), 1), TicksToCode(TicksPerRow));
            end else begin
              if Row.Note = 0 then begin
                if DidEnvelope then begin
                  WriteLn(' note $'+IntToHex(Note, 1), TicksToCode(TicksPerRow));
                  Continue;
                end else begin
                  Inc(Waited);
                  Continue;
                end;
              end;

              Note := (Row.Note - 1) mod 12;
              Octave := EnsureRange(((Row.Note - 1) div 12)+1, 1, 7);

              if (Octave <> OldOctave) then
                WriteLn(' octave $'+IntToHex(Octave, 1));

              WriteLn(' note $'+IntToHex(Note, 1), TicksToCode(TicksPerRow));

              OldOctave := Octave;
            end;
          end;
        end;
      end;
    end;

    if Waited > 0 then
      Wait(Waited);
  end;

begin
  TicksPerRow := StrToInt(ParamStr(2));

  Stream := TIOStream.Create(iosInput);
  //Stream := TFileStream.Create('gb2_digic.tbm', fmOpenRead);

  Stream.ReadBuffer(Header, SizeOf(TTBMHeader));

  // COMM block
  Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
  Stream.Seek(BlockHeader.Length, soCurrent); // Skip the comment for now

  // SONG block
  Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
  {Result.Name := }Stream.ReadLString;
  Stream.ReadBuffer(SongFormat, SizeOf(TTBMSongFormat));

  // Number of visible effect columns-- undocumented currently but I asked
  // stoneface about it.
  Stream.ReadByte;

  //TicksPerRow := SongFormat.RowsPerBeat;

  for I := Low(OrderMatrix) to High(OrderMatrix) do
    SetLength(OrderMatrix[I], SongFormat.PatternCount+2); // off-by-one error on my part

  for I := 0 to SongFormat.PatternCount do begin
    OrderMatrix[0, I] := 100 + Stream.ReadByte;
    OrderMatrix[1, I] := 200 + Stream.ReadByte;
    OrderMatrix[2, I] := 300 + Stream.ReadByte;
    OrderMatrix[3, I] := 400 + Stream.ReadByte;
  end;

  Patterns := TPatternMap.Create;
  for I := 0 to SongFormat.NumberOfTracks-1 do begin
    Stream.ReadBuffer(TrackFormat, SizeOf(TTBMTrackFormat));

    New(Pat);
    Pat^ := Default(TPattern);
    for J := 0 to TrackFormat.Rows do begin
      Stream.ReadBuffer(RowFormat, SizeOf(TTBMRowFormat));
      Pat^[RowFormat.RowNo] := RowFormat.RowData;
    end;
    Patterns.Add(((TrackFormat.Channel+1)*100) + TrackFormat.TrackId, Pat);
  end;

  // Convert to bytecode
  WriteLn('BGM_Title:');
  WriteLn('dw BGM_Title_Pulse1');
  WriteLn('dw BGM_Title_Pulse2');
  WriteLn('dw BGM_Title_Wave');
  WriteLn('dw BGM_Title_Noise');

  WriteLn('BGM_Title_Pulse1:');
  WriteLn('db $C4,$FF,$C0,', ParamStr(1));
  Emit(0);
  WriteLn('db $CE');

  WriteLn('BGM_Title_Pulse2:');
  Emit(1);
  WriteLn('db $CE');

  WriteLn('BGM_Title_Wave:');
  WriteLn('db $C1,$00');
  Emit(2);
  WriteLn('db $CE');

  WriteLn('BGM_Title_Noise:');
  Emit(3);
  WriteLn('db $CE');

  writeln(StdErr, 'Used noise:');
  for B in UsedNoise do
    writeln(StdErr, 'db %', IntToBin(B, 8), ' ');
end.


