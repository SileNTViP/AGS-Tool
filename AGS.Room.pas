unit AGS.Room;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, System.SysUtils, System.Types, AGS.Script;

const
  MAX_BSCENE = 5;

type
  PolyPoints = record
    x, y: array [0..29] of Integer;
    numpoints: Integer;
  end;
  TRoom = class(TObject)
    private
      roomstr, objectnames, objectscriptnames, hotspotnames: array of AnsiString;
      numstr, fileVer: Integer;
      numobj, numhotspots, numwalkareas: Integer;
      num_bscenes, bscene_anim_speed: Int8;
      numsprs: Short;
      room: TFileStream;
      ebpalShared: array [0..MAX_BSCENE] of AnsiChar;
      function fgeti(FS: TFileStream): Integer;
      function fgetw(FS: TFileStream): Word;
      function fgetb(FS: TFileStream): Byte;
      function freadstring(FS: TFileStream): AnsiString;
      procedure ReadFromStream(FS: TFileStream);
      procedure ReadFromFile(FileName: AnsiString);
      procedure load_main_block(FS: TFileStream);
      procedure AddExtrString(Str: AnsiString);
      function GetTempDirectory: AnsiString;
      procedure CustomProperties(FS: TFileStream);
      function fgetstring_limit(FS: TFileStream; BufSize: Integer): AnsiString;
    public
      constructor Create; overload;
      constructor Create(FileName: AnsiString); overload;
      constructor Create(FS: TFileStream); overload;
      function GetExtractedStringsCount: Integer;
      function GetExtractedString(Index: Integer): AnsiString;
      procedure ExtractStrings;
  end;

const
  // Room versions
    kRoomVersion_Undefined  = 0;
    kRoomVersion_pre114_3   = 3;  // exact version unknown
    kRoomVersion_pre114_4   = 4;  // exact version unknown
    kRoomVersion_pre114_5   = 5;  // exact version unknown
    kRoomVersion_pre114_6   = 6;  // exact version unknown
    kRoomVersion_114        = 8;
    kRoomVersion_200_alpha  = 9;
    kRoomVersion_200_alpha7 = 10;
    kRoomVersion_200_final  = 11;
    kRoomVersion_208        = 12;
    kRoomVersion_214        = 13;
    kRoomVersion_240        = 14;
    kRoomVersion_241        = 15;
    kRoomVersion_250a       = 16;
    kRoomVersion_250b       = 17;
    kRoomVersion_251        = 18;
    kRoomVersion_253        = 19;
    kRoomVersion_255a       = 20;
    kRoomVersion_255b       = 21;
    kRoomVersion_261        = 22;
    kRoomVersion_262        = 23;
    kRoomVersion_270        = 24;
    kRoomVersion_272        = 25;
    kRoomVersion_300a       = 26;
    kRoomVersion_300b       = 27;
    kRoomVersion_303a       = 28;
    kRoomVersion_303b       = 29;
    kRoomVersion_334        = 30;
    kRoomVersion_Current    = kRoomVersion_334;

  MAXOBJNAMELEN = 30;
  MAX_SCRIPT_NAME_LEN = 20;
  BLOCKTYPE_MAIN = 1;
  BLOCKTYPE_SCRIPT = 2;
  BLOCKTYPE_COMPSCRIPT = 3;
  BLOCKTYPE_COMPSCRIPT2 = 4;
  BLOCKTYPE_OBJECTNAMES = 5;
  BLOCKTYPE_ANIMBKGRND = 6;
  BLOCKTYPE_COMPSCRIPT3 = 7;
  BLOCKTYPE_PROPERTIES = 8;
  BLOCKTYPE_OBJECTSCRIPTNAMES = 9;
  BLOCKTYPE_EOF = $ff;
  MAX_CUSTOM_PROPERTY_VALUE_LENGTH = 500;

implementation

constructor TRoom.Create;
begin

end;

constructor TRoom.Create(FileName: AnsiString);
begin
  ReadFromFile(FileName);
end;

constructor TRoom.Create(FS: TFileStream);
begin
  ReadFromStream(FS);
end;

function TRoom.fgeti(FS: TFileStream): Integer;
begin
  FS.Read(Result, 4);
end;

function TRoom.fgetw(FS: TFileStream): Word;
begin
  FS.Read(Result, 2);
end;

function TRoom.fgetb(FS: TFileStream): Byte;
begin
  FS.Read(Result, 1);
end;

function TRoom.freadstring(FS: TFileStream): AnsiString;
var
  buf: AnsiChar;
  i: Integer;
begin
  Result := '';
  while True do
  begin
    FS.Read(buf, 1);
    if buf = #0 then
      Break
    else
      Result := Result + buf;
  end;
end;

function TRoom.GetExtractedString(Index: Integer): AnsiString;
begin
  Result := roomstr[Index];
end;

function TRoom.GetExtractedStringsCount: Integer;
begin
  Result := numstr;
end;

procedure TRoom.ExtractStrings;
var
  i: Integer;
begin

end;

procedure TRoom.load_main_block(FS: TFileStream);
var
  i: Integer;
  buf: array of AnsiChar;
begin
  if fileVer >= 12 then
    fgeti(FS);
  numobj := fgetw(FS);
  FS.Seek(numobj*2, soFromCurrent);
  numhotspots := fgeti(FS);
  FS.Seek(numhotspots*4, soFromCurrent);
  for i := 0 to numhotspots-1 do
  begin
    if fileVer >= 28 then
      AddExtrString(fgetstring_limit(FS, 2999))
    else
    begin
      SetLength(buf, 30);
      FS.Read(PAnsiChar(buf)^, 30);
      AddExtrString(AnsiString(buf));
      SetLength(buf, 0);
    end;
  end;
  if fileVer >= 24 then
    FS.Seek(MAX_SCRIPT_NAME_LEN*numhotspots, soFromCurrent);
  numwalkareas := fgetw(FS);
  FS.Seek(SizeOf(PolyPoints) * numwalkareas, soFromCurrent);
  FS.Seek(8, soFromCurrent);
  FS.Read(numsprs, 2);
end;

procedure TRoom.AddExtrString(Str: AnsiString);
var
  Found: Boolean;
  i: Integer;
  newstr: AnsiString;
begin
  Found := False;
  newstr := '';
  for i := 1 to Length(Str) do
    if Str[i] <> #0 then
      newstr := newstr + Str[i];
  if (newStr = '') or (newStr[1] = #0) then
    Exit;
  for i := 0 to numstr - 1 do
  begin
    if roomstr[i] = newStr then
    begin
      Found := True;
      Break;
    end;
  end;
  if not Found then
  begin
    inc(numstr);
    SetLength(roomstr, numstr);
    roomstr[numstr-1] := newStr;
  end;
end;

function TRoom.GetTempDirectory: AnsiString;
var
  tempFolder: array[0..MAX_PATH] of AnsiChar;
begin
  GetTempPathA(MAX_PATH, @tempFolder);
  Result := tempFolder;
end;

procedure TRoom.ReadFromStream(FS: TFileStream);
var
  i, thisblock, bloklen, scrsize, fpos, ct: Integer;
  roomscr: TScript;
  tmpfile: TFileStream;
  buf: array of Byte;
begin
  fileVer := fgetw(FS);
  if (fileVer < kRoomVersion_241) or (fileVer > kRoomVersion_Current) then
    Exit;
  thisblock := 0;
  while thisblock <> BLOCKTYPE_EOF do
  begin
    thisblock := fgetb(FS);
    if thisblock = BLOCKTYPE_EOF then
      Break;
    bloklen := fgeti(FS);
    inc(bloklen, FS.Position);
    if thisblock = BLOCKTYPE_MAIN then begin
      load_main_block(FS);
    end else if thisblock = BLOCKTYPE_SCRIPT then begin
    end else if thisblock = BLOCKTYPE_COMPSCRIPT3 then begin
      roomscr := TScript.Create(FS);
      roomscr.ExtractStrings3;
      for i := 0 to roomscr.GetExtractedStringsCount - 1 do
        AddExtrString(roomscr.GetExtractedString(i));
      roomscr.Free;
    end else if (thisblock = BLOCKTYPE_COMPSCRIPT) or (thisblock = BLOCKTYPE_COMPSCRIPT2) then begin
    end else if thisblock = BLOCKTYPE_OBJECTNAMES then begin
      numsprs := fgetb(FS);
        SetLength(objectnames, numsprs);
        for i := 0 to numsprs - 1 do
        begin
          SetLength(objectnames[i], MAXOBJNAMELEN);
          FS.Read(PAnsiChar(objectnames[i])^, MAXOBJNAMELEN);
          AddExtrString(objectnames[i]);
        end;
    end else if thisblock = BLOCKTYPE_OBJECTSCRIPTNAMES then begin
      numsprs := fgetb(FS);
        SetLength(objectscriptnames, numsprs);
        for i := 0 to numsprs - 1 do
        begin
          SetLength(objectscriptnames[i], MAX_SCRIPT_NAME_LEN);
          FS.Read(PAnsiChar(objectscriptnames[i])^, MAX_SCRIPT_NAME_LEN);
        end;
    end else if thisblock = BLOCKTYPE_ANIMBKGRND then begin
      num_bscenes := fgetb(FS);
      bscene_anim_speed := fgetb(FS);
      if fileVer >= kRoomVersion_255a then
        FS.Read(PAnsiChar(ebpalShared[0])^, num_bscenes);
      fpos := FS.Position;
      for ct := 1 to num_bscenes do
      begin

      end;
    end else if thisblock = BLOCKTYPE_PROPERTIES then begin
      i := FS.Position;
      if fgeti(FS) = 1 then
      begin
        CustomProperties(FS);
        for i := 0 to numhotspots - 1 do
          CustomProperties(FS);
//        for i := 0 to numsprs - 1 do
//          CustomProperties(FS);
      end;
    end else if thisblock = -1 then begin
      Exit;
    end else begin
      Exit;
    end;
    if FS.Position <> bloklen then
      FS.Seek(bloklen, soFromBeginning);
  end;
end;

procedure TRoom.CustomProperties(FS: TFileStream);
var
  numProps, i: Integer;
  buf: AnsiString;
begin
  if fgeti(FS) <> 1 then
    Exit;
  numProps := fgeti(FS);
  for i := 0 to numProps - 1 do
  begin
    buf := fgetstring_limit(FS, 200);
    buf := fgetstring_limit(FS, MAX_CUSTOM_PROPERTY_VALUE_LENGTH);
    AddExtrString(buf);
  end;
end;

function TRoom.fgetstring_limit(FS: TFileStream; BufSize: Integer): AnsiString;
var
  buf: AnsiChar;
  curc: Integer;
begin
  Result := ''; curc := 1;
  while True do
  begin
    if (FS.Position = FS.Size) or (curc = BufSize) then
      Break;
    FS.Read(buf, 1);
    inc(curc);
    if buf = #0 then
      Break
    else
      Result := Result + buf;
  end;
end;

procedure TRoom.ReadFromFile(FileName: AnsiString);
begin
  Room := TFileStream.Create(FileName, fmOpenRead);
  ReadFromStream(Room);
  Room.Free;
end;

end.