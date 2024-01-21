program conv;

{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, LazUTF8, fgl, math;

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

var
  Stream: TStream;
  I, J: Integer;
  F: Text;

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
    Frames := Rows * 3;

    while Frames > 0 do begin
      for W in Waits do
        if W.Frames <= Frames then begin
          Dec(Frames, W.Frames);
          Writeln(F, ' wait $'+W.Code);
          Break;
        end;
    end;
  end;

  procedure Emit(Ch: Integer);
  var
    Octave, Note: Integer;
    FX: TTBMEffect;
    Waited: Integer = 0;
  begin
    for I := Low(OrderMatrix[Ch]) to High(OrderMatrix[Ch])-1 do begin
      writeln(F, '; pattern break');
      Pat := FetchPattern(Patterns, OrderMatrix[Ch, I]);

      for Row in Pat^ do begin
        for FX in Row.Effects do begin
          case TTBMEffectType(FX.EffectType) of
            etSetEnvelope: Writeln(F, ' envelope $', IntToHex(FX.Param, 2));
            etSetTimbre: begin
              if Ch = 2 then
                case FX.Param of
                  0: Writeln(F, ' wave_vol $00');
                  1: Writeln(F, ' wave_vol $60');
                  2: Writeln(F, ' wave_vol $40');
                  3: Writeln(F, ' wave_vol $20');
                end
              else
                case FX.Param of
                  0: Writeln(F, ' duty_cycle $00');
                  1: Writeln(F, ' duty_cycle $40');
                  2: Writeln(F, ' duty_cycle $80');
                  3: Writeln(F, ' duty_cycle $C0');
                end;
            end;
          end;
        end;
        if Row.Note = 0 then
          Inc(Waited)
          //Writeln(F, ' wait $E')
        else if Row.Note = 85 then
          Inc(Waited)
        else begin
          if Waited > 0 then begin
            Wait(Waited);
            Waited := 0;
          end;
          Note := (Row.Note - 1) mod 12;
          Octave := EnsureRange(((Row.Note - 1) div 12)+1, 1, 7);

          Writeln(F, ' octave $'+IntToHex(Octave, 1));
          Writeln(F, ' note $'+IntToHex(Note, 1)+'E');
        end;
      end;
    end;

    if Waited > 0 then
      Wait(Waited);
  end;

begin
  Stream := TFileStream.Create('gb2_digic.tbm', fmOpenRead);

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

  TicksPerRow := SongFormat.RowsPerBeat;

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

  Assign(F, 'bgm/BGM_Title.asm');
  Rewrite(F);

  writeln(F, 'BGM_Title:');
  writeln(F, 'dw BGM_Title_Pulse1');
  writeln(F, 'dw BGM_Title_Pulse2');
  writeln(F, 'dw BGM_Title_Wave');
  writeln(F, 'dw BGM_Title_Noise');

  writeln(F, 'BGM_Title_Pulse1:');
  writeln(F, 'db $C4,$FF,$C0,$BB');
  Emit(0);
  writeln(F, 'db $CE');

  writeln(F, 'BGM_Title_Pulse2:');
  Emit(1);
  writeln(F, 'db $CE');

  writeln(F, 'BGM_Title_Wave:');
  writeln(F, 'db $C1,$D5');
  Emit(2);
  writeln(F, 'db $CE');

  writeln(F, 'BGM_Title_Noise:');
  //Emit(3);
  writeln(F, 'db $CE');

  Close(F);
end.

