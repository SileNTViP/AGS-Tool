unit AGS.Characters;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, System.SysUtils, System.Types,
  Vcl.Graphics;

type
  TRGBQuadArray = packed array [0..MaxInt div SizeOf (TRGBQuad) - 1] of TRGBQuad;
  PRGBQuadArray = ^TRGBQuadArray;
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = ARRAY[0..32767] OF TRGBTriple;
  TFrame = record
    ID:             Integer;
    Image:          Integer;
    Delay:          Word;
    Flipped:        Boolean;
    Sound:          Integer;
  end;
  TView = record
    numLoops:       Word;
    numFrames:      array of Word;
    Frames:         array [0..319] of TFrame;
    Bitmaps:        array of array of TBitmap;
  end;
  TCharacter = record
    Version:        Integer;
    Pallete:        array [0..255] of DWORD;
    SpeechView:     DWORD;
    StartingRoom:   Integer;
    StartX:         Integer;
    StartY:         Integer;
    IdleView:       DWORD;
    SpeechColor:    Integer;
    ThinkingView:   DWORD;
    BlinkingView:   Word;
    MovementSpeed:  Word;
    AnimationDelay: Word;
    RealName:       array [0..39] of AnsiChar;
    ScriptName:     array [0..19] of AnsiChar;
  end;
  TChaFile = class (TObject)
      ChaFileName: string;
      ChaStream: TFileStream;
      Character: TCharacter;
      pal: PLogPalette;
      ViewsOffset: Integer;
      SpeechView, IdleView, ThinkingView, BlinkingView: TView;
    private
      procedure ReadOldStyleView(var View: TView);
      procedure ReadOldStyleViewFrame(var Bit: TBitmap);
    public
      function GetFullName: string;
      constructor Create(FileName: string); overload;
      destructor Free;
      function GetLoops(ViewType: Integer): Integer;
      function ViewExist(ViewType: Integer): Boolean;
      function GetBitmap(Loop, Frame: Integer; ViewType: Integer): TBitmap;
  end;

const
  CHARACTER_FILE_SIGNATURE = 'AGSCharacter';
  SPEECH_VIEW   = 0;
  IDLE_VIEW     = 1;
  THINKING_VIEW = 2;
  BLINKING_VIEW = 3;

implementation

function TChaFile.GetFullName: string;
begin
  Result := Character.RealName;
end;

function TChaFile.GetLoops(ViewType: Integer): Integer;
var
  CurView: TView;
begin
  Result := 0;
  if ViewType = SPEECH_VIEW then
    if Character.SpeechView = 0 then
      Exit;

  if ViewType = IDLE_VIEW then
    if Character.IdleView = 0 then
      Exit;

  if ViewType = THINKING_VIEW then
    if Character.ThinkingView = 0 then
      Exit;

  if ViewType = BLINKING_VIEW then
    if Character.BlinkingView = 0 then
      Exit;

  case ViewType of
    SPEECH_VIEW:
      CurView := SpeechView;
    IDLE_VIEW:
      CurView := IdleView;
    THINKING_VIEW:
      CurView := ThinkingView;
    BLINKING_VIEW:
      CurView := BlinkingView;
    else
      Exit;
  end;
  Result := CurView.numLoops;
end;

function TChaFile.ViewExist(ViewType: Integer): Boolean;
begin
  Result := False;
  case ViewType of
    SPEECH_VIEW:
      if (Character.SpeechView > 0) and (SpeechView.numLoops > 0) then
        Result := True;
    IDLE_VIEW:
      if (Character.IdleView > 0) and (IdleView.numLoops > 0) then
        Result := True;
    THINKING_VIEW:
      if (Character.ThinkingView > 0) and (ThinkingView.numLoops > 0) then
        Result := True;
    BLINKING_VIEW:
      if (Character.BlinkingView > 0) and (BlinkingView.numLoops > 0) then
        Result := True;
    else
      Result := False;
  end;
end;

function TChaFile.GetBitmap(Loop, Frame: Integer; ViewType: Integer): TBitmap;
var
  CurView: TView;
begin
  if ViewType = SPEECH_VIEW then
    if Character.SpeechView = 0 then
      Exit;

  if ViewType = IDLE_VIEW then
    if Character.IdleView = 0 then
      Exit;

  if ViewType = THINKING_VIEW then
    if Character.ThinkingView = 0 then
      Exit;

  if ViewType = BLINKING_VIEW then
    if Character.BlinkingView = 0 then
      Exit;


  case ViewType of
    SPEECH_VIEW:
      CurView := SpeechView;
    IDLE_VIEW:
      CurView := IdleView;
    THINKING_VIEW:
      CurView := ThinkingView;
    BLINKING_VIEW:
      CurView := BlinkingView;
    else
      Exit;
  end;
  Result := CurView.Bitmaps[Loop][Frame];
end;

procedure TChaFile.ReadOldStyleView(var View: TView);
var
  i, j: Integer;
begin
  ChaStream.Read(View.numLoops, 2);
  SetLength(View.numFrames, View.numLoops);
  for i := 0 to View.numLoops - 1 do
    ChaStream.Read(View.numFrames[i], 2);
  if View.numLoops < 16 then
    ChaStream.Seek((16 - View.numLoops) * 2, soFromCurrent);
  ChaStream.Seek(16 * 4 + 2, soFromCurrent);
  for i := 0 to 15 do
    for j := 0 to 19 do
    begin
      View.Frames[j].ID := j;
      ChaStream.Read(View.Frames[j].Image, 4);
      ChaStream.Seek(4, soFromCurrent);
      ChaStream.Read(View.Frames[j].Delay, 2);
      ChaStream.Seek(2, soFromCurrent);
      ChaStream.Read(View.Frames[j].Flipped, 4);
      ChaStream.Read(View.Frames[j].Sound, 4);
      ChaStream.Seek(8, soFromCurrent);
    end;

  SetLength(View.Bitmaps, View.numLoops);
  for i := 0 to View.numLoops - 1 do
    SetLength(View.Bitmaps[i], View.numFrames[i]);

  for i := 0 to View.numLoops - 1 do
    for j := 0 to View.numFrames[i] - 1 do
    begin
      ReadOldStyleViewFrame(View.Bitmaps[i][j]);
//      View.Bitmaps[i][j].SaveToFile(IntToStr(i) + '_' + IntToStr(j) + '.bmp');
    end;
end;

procedure TChaFile.ReadOldStyleViewFrame(var Bit: TBitmap);
var
  i, j, colDepth, width, height: Integer;
  spriteFlags: Byte;
  g: TRGBQuad;
  Row8bit:  pByteArray;
  Row16bit: pWordArray;
  Row24bit: PRGBTripleArray;
  Row32bit: PRGBQuadArray;
  TranspCol: TColor;
begin
  ChaStream.Read(colDepth, 4);
  if colDepth = 200 then
    Exit;
  ChaStream.Read(spriteFlags, 1);
  ChaStream.Read(width, 4);
  ChaStream.Read(height, 4);
  Bit := TBitmap.Create;
  if colDepth = 8 then
  begin
    Bit.PixelFormat := pf8bit;
    Bit.Palette := CreatePalette(pal^);;
  end else if colDepth = 16 then begin
      Bit.PixelFormat := pf16bit;
  end else if colDepth = 24 then begin
      Bit.PixelFormat := pf24bit;
  end else if colDepth = 32 then begin
      Bit.PixelFormat := pf32bit;
  end;
  Bit.Width := width;
  Bit.Height := height;
  TranspCol := $00FF00FF;
  Bit.TransparentColor := TranspCol;
  Bit.Transparent := True;
  for i := 0 to height - 1 do
  begin
    if colDepth = 8 then
      Row8bit := bit.ScanLine[i]
    else if colDepth = 16 then
      Row16bit := bit.ScanLine[i]
    else if colDepth = 24 then
      Row24bit := bit.ScanLine[i]
    else if colDepth = 32 then
      Row32bit := bit.ScanLine[i];

    if colDepth = 8 then begin
      ChaStream.Read(Row8bit[0], width * 1);
    end else if colDepth = 16 then begin
      ChaStream.Read(Row16bit[0], width * 2);
    end else if colDepth = 24 then begin
      ChaStream.Read(Row24bit[0], width * 3);
    end else if colDepth = 32 then begin
      ChaStream.Read(Row32bit[0], width * 4);
    end;
  end;

end;

constructor TChaFile.Create(FileName: string);
var
  fileSig: array [0..11] of AnsiChar;
  fileVersion: Integer;
  aa: Integer;
  pcol: TRGBQuad;
begin
  ChaFileName := FileName;
  ChaStream := TFileStream.Create(FileName, fmOpenRead);
  ChaStream.Read(fileSig, 12);
  if fileSig <> CHARACTER_FILE_SIGNATURE then
  begin
    Exit;
  end;
  ChaStream.Read(Character.Version, 4);
  if (Character.Version < 5) or (Character.Version > 6) then
  begin
    Exit;
  end;
  GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 255);
  pal.palVersion := $300;
  pal.palNumEntries := 256;
  ChaStream.Read(Character.Pallete, 256 * 4);
  for aa := 0 to 255 do
  begin
    pcol := TRGBQuad(Character.Pallete[aa]);
    pal.palPalEntry[aa].peRed := pcol.rgbBlue * 4;
    pal.palPalEntry[aa].peGreen := pcol.rgbGreen * 4;
    pal.palPalEntry[aa].peBlue := pcol.rgbRed * 4;
  end;
  ChaStream.Seek(4, soFromCurrent);
  ChaStream.Read(Character.SpeechView, 4);
  ChaStream.Seek(4, soFromCurrent);
  ChaStream.Read(Character.StartingRoom, 4);
  ChaStream.Seek(4, soFromCurrent);
  ChaStream.Read(Character.StartX, 4);
  ChaStream.Read(Character.StartY, 4);
  ChaStream.Seek(12, soFromCurrent);
  ChaStream.Read(Character.IdleView, 4);
  ChaStream.Seek(12, soFromCurrent);
  ChaStream.Read(Character.SpeechColor, 4);
  ChaStream.Read(Character.ThinkingView, 4);
  ChaStream.Read(Character.BlinkingView, 2);
  ChaStream.Seek(42, soFromCurrent);
  ChaStream.Read(Character.MovementSpeed, 2);
  ChaStream.Read(Character.AnimationDelay, 2);
  ChaStream.Seek(606, soFromCurrent);
  ChaStream.Read(Character.RealName, 40);
  ChaStream.Read(Character.ScriptName, 20);
  ChaStream.Seek(2, soFromCurrent);

  if Character.SpeechView > 0 then
    ReadOldStyleView(SpeechView)
  else
    Character.SpeechView := 0;

  if Character.IdleView > 0 then
    ReadOldStyleView(IdleView)
  else
    Character.IdleView := 0;

  if (Character.ThinkingView > 0) and (Character.Version >= 6) then
    ReadOldStyleView(ThinkingView)
  else
    Character.ThinkingView := 0;

  if (Character.BlinkingView > 0) and (Character.Version >= 6) then
    ReadOldStyleView(BlinkingView)
  else
    Character.BlinkingView := 0;
end;

destructor TChaFile.Free;
var
  i,j: Integer;
begin
  ChaStream.Free;

  for i := 0 to SpeechView.numLoops - 1 do
    for j := 0 to SpeechView.numFrames[i] - 1 do
      SpeechView.Bitmaps[i][j].Free;

  for i := 0 to IdleView.numLoops - 1 do
    for j := 0 to IdleView.numFrames[i] - 1 do
      IdleView.Bitmaps[i][j].Free;

  for i := 0 to ThinkingView.numLoops - 1 do
    for j := 0 to ThinkingView.numFrames[i] - 1 do
      ThinkingView.Bitmaps[i][j].Free;

  for i := 0 to BlinkingView.numLoops - 1 do
    for j := 0 to BlinkingView.numFrames[i] - 1 do
      BlinkingView.Bitmaps[i][j].Free;

  SetLength(SpeechView.numFrames, 0);
  SetLength(SpeechView.Bitmaps, 0);
  SetLength(IdleView.numFrames, 0);
  SetLength(IdleView.Bitmaps, 0);
  SetLength(ThinkingView.numFrames, 0);
  SetLength(ThinkingView.Bitmaps, 0);
  SetLength(BlinkingView.numFrames, 0);
  SetLength(BlinkingView.Bitmaps, 0);
end;

end.