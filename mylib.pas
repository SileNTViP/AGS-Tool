unit mylib;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.IOUtils,
  System.Generics.Collections, Winapi.ShellAPI;

type
  SJISString = type AnsiString(932);
  TFiles = record
    FileName: string;
    Size: Cardinal;
  end;

procedure ScanDir(StartDir: string; Mask: string; var List: TList<TFiles>);
function WindowsCopyFile(FromFile, ToDir: string): Boolean;
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
function CreateFolder(FileName: string): string;
function GetFolder(FileName: string): string;
function ArrayToString(Value: Pointer; Len: Integer; Enc: TEncoding): string;
function GetFullFilePath(FileName: string): string;
function DelSpCh(str: string): string;
function InsSpCh(str: string): string;
function GetWordsCount(SubStr: AnsiChar; Str: AnsiString): Integer;
function GetWord(SubStr: AnsiChar; Str: AnsiString; Word: Integer): AnsiString;
function cs(Str: AnsiString): AnsiString;
procedure NeedToAdd(Stream: TStream; Count: Integer; Size: Integer);
function CalculateSize(Size: Integer; Count: Integer): Integer;
function ReverseBits(b: Byte): Byte;

implementation


procedure ScanDir(StartDir: string; Mask: string; var List: TList<TFiles>);
var
  SearchRec: TSearchRec;
  newFile: TFiles;
begin
  if Mask = '' then Mask := '*.*';
  if StartDir[Length(StartDir)] <> '\' then StartDir := StartDir + '\';
  if FindFirst(StartDir + Mask, faAnyFile, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory) <> faDirectory then begin
        newFile.FileName := StartDir + SearchRec.Name;
        newFile.Size := SearchRec.Size;
        List.Add(newFile);
      end else
        if (SearchRec.Name <> '..') and (SearchRec.Name <> '.') then
        begin
          ScanDir(StartDir + SearchRec.Name + '\', Mask, List);
        end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

function WindowsCopyFile(FromFile, ToDir: string): Boolean;
var
  F: TShFileOpStruct;
begin
  F.Wnd := 0; F.wFunc := FO_COPY;
  FromFile := FromFile + #0; F.pFrom := PChar(FromFile);
  ToDir := ToDir + #0; F.pTo := PChar(ToDir);
  F.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION;
  result:=ShFileOperation(F) = 0;
end;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
   ListOfStrings.DelimitedText   := Str;
end;

function CreateFolder(FileName: string): string;
begin
  Result := ExtractFilePath(FileName) + TPath.GetFileNameWithoutExtension(FileName)+'\';
  if Result[2] <> ':' then Result := GetCurrentDir + '\' + Result;
  ForceDirectories(Result);
end;

function GetFolder(FileName: string): string;
begin
  Result := ExtractFilePath(FileName) + TPath.GetFileNameWithoutExtension(FileName)+'\';
  if Result[2] <> ':' then Result := GetCurrentDir + '\' + Result;
end;

function ArrayToString(Value: Pointer; Len: Integer; Enc: TEncoding): string;
var
  Buf: TList<Byte>;
  CurByte: Byte;
  i: Integer;
begin
  Buf := TList<Byte>.Create;
  for i := 0 to Len - 1 do begin
    CurByte := PByteArray(Value)[i];
    if CurByte = 0 then Break;
    Buf.Add(CurByte);
  end;
  Result := Enc.GetString(Buf.ToArray);
  Buf.Free;
end;

function GetFullFilePath(FileName: string): string;
begin
  Result := ExtractFilePath(FileName) + '\';
  if Result[2] <> ':' then Result := GetCurrentDir + '\' + Result;
end;

function DelSpCh(str: string): string;
begin
  Result := StringReplace(str, #$0A, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #$07, '\a', [rfReplaceAll]);
  Result := StringReplace(Result, #$08, '\b', [rfReplaceAll]);
  Result := StringReplace(Result, #$09, '\t', [rfReplaceAll]);
  Result := StringReplace(Result, #$0D, '\r', [rfReplaceAll]);
//  Result := StringReplace(Result, #$18, '{$18}', [rfReplaceAll]);
end;

function InsSpCh(str: string): string;
begin
  Result := StringReplace(str, '\n', #$0A, [rfReplaceAll]);
  Result := StringReplace(Result, '\a', #$07, [rfReplaceAll]);
  Result := StringReplace(Result, '\b', #$08, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #$09, [rfReplaceAll]);
  Result := StringReplace(Result, '\r', #$0D, [rfReplaceAll]);
end;

function GetWordsCount(SubStr: AnsiChar; Str: AnsiString): Integer;
var
  i: Integer;
begin
  Result := 1;
  for I := 1 to Length(Str) do
    if AnsiChar(Str[i]) = SubStr then
      Inc(Result);
end;

function GetWord(SubStr: AnsiChar; Str: AnsiString; Word: Integer): AnsiString;
var
  i, p: Integer;
begin
  p := 1;
  Result := '';
  for i := 1 to Length(Str) do
  begin
    if p > Word then Exit;
    if AnsiChar(Str[i]) = SubStr then Inc(p)
    else
      if p = Word then
        Result := Result + Str[i];
  end;
end;

function cs(Str: AnsiString): AnsiString;
var
  nc: Boolean;
  i: Integer;
begin
  Result := '';
  nc := False;
  for i := 1 to Length(Str) do
  begin
    if nc then begin
      if AnsiChar(Str[i]) = 'n' then
        Result := Result + #$a
      else
        Result := Result + '\' + Str[i];
      nc := False;
    end else begin
      if (AnsiChar(Str[i]) = '\') and (i <> Length(Str)) then
        nc := True
      else
        Result := Result + Str[i];
    end;
  end;
end;

// Вставляет пустоту, чтоб было ровненько... хз зачем
procedure NeedToAdd(Stream: TStream; Count: Integer; Size: Integer);
var
  newSize: Integer;
  buf: TBytes;
begin
  newSize := CalculateSize(Size, Count);
  if (newSize <> 0) and (newSize <> Count) then begin
    SetLength(buf, newSize);
    Stream.WriteBuffer(buf[0], newSize);
    SetLength(buf, 0);
  end;
end;

// Подсчет размера со вставленной пустотой
function CalculateSize(Size: Integer; Count: Integer): Integer;
var
  modSize, newSize: Integer;
begin
  newSize := Size div Count;
  modSize := Size mod Count;
  if modSize = 0 then
    Result := 0
  else
    Result := ((newSize + 1) * Count) - Size;
end;

function ReverseBits(b: Byte): Byte;
const
  Table: array [Byte] of Byte = (
    0,128,64,192,32,160,96,224,16,144,80,208,48,176,112,240,
    8,136,72,200,40,168,104,232,24,152,88,216,56,184,120,248,
    4,132,68,196,36,164,100,228,20,148,84,212,52,180,116,244,
    12,140,76,204,44,172,108,236,28,156,92,220,60,188,124,252,
    2,130,66,194,34,162,98,226,18,146,82,210,50,178,114,242,
    10,138,74,202,42,170,106,234,26,154,90,218,58,186,122,250,
    6,134,70,198,38,166,102,230,22,150,86,214,54,182,118,246,
    14,142,78,206,46,174,110,238,30,158,94,222,62,190,126,254,
    1,129,65,193,33,161,97,225,17,145,81,209,49,177,113,241,
    9,137,73,201,41,169,105,233,25,153,89,217,57,185,121,249,
    5,133,69,197,37,165,101,229,21,149,85,213,53,181,117,245,
    13,141,77,205,45,173,109,237,29,157,93,221,61,189,125,253,
    3,131,67,195,35,163,99,227,19,147,83,211,51,179,115,243,
    11,139,75,203,43,171,107,235,27,155,91,219,59,187,123,251,
    7,135,71,199,39,167,103,231,23,151,87,215,55,183,119,247,
    15,143,79,207,47,175,111,239,31,159,95,223,63,191,127,255
  );
begin
  Result := Table[b];
end;

end.
