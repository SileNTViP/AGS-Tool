unit AGS.Translation;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, System.SysUtils, System.Types;

type
  TDictionary = record
    Original:     AnsiString;
    Translated:   AnsiString;
  end;
  TDict = array of TDictionary;
  TTranslation = class(TObject)
    private
      Dict: array of TDictionary;
      exstr: array of AnsiString;
      DictSize, normalFont, speechFont, CurStr, exnum: Integer;
      rightToLeftText: Boolean;
      isNF, isSF, isDIR: Boolean;
      tra: TFileStream;
      wasgamename: AnsiString;
      uidfrom: uInt;
      function getw: Integer;
      procedure putw(int: Integer);
      function read_string_decrypt: AnsiString;
      function decrypt_text(Str: AnsiString): AnsiString;
      procedure write_string_crypt(Str: AnsiString);
      function crypt_text(Str: AnsiString): AnsiString;
      procedure ReadSpecialTags(Str: AnsiString);
      function ReadOptionalInt(Str: AnsiString): Integer;
      function StartsWith(Str, StartStr: AnsiString): Boolean;
      function Substring(Str, Sub: AnsiString): AnsiString;
      function IsNum(Chr: AnsiChar): Boolean;
      procedure ExclAdd(Str: AnsiString);
      procedure ExclPrep;
      function ExclStr(Str: AnsiString): Boolean;
      function ExtractAudioTagInt(Str: AnsiString): Integer;
      procedure FixupAudioTags;
      procedure ReadFromTextInbox(FileName: AnsiString; NumStr: Integer);
      function ReadFromTextOutbox(FileName: AnsiString): Integer;
      procedure ReadFromCompiled(FileName: AnsiString);
      procedure ReadFromDecompiled(FileName: AnsiString);
      procedure SkipInboxTranslation(NumStr: Integer);
      procedure Sort;
    public
      bVersion2: Boolean;
      constructor Create; overload;
      constructor Create(FileName: AnsiString); overload;
      destructor Free; overload;
      function GetGameName: AnsiString;
      function GetGameID: Integer;
      procedure SetGameName(GameName: AnsiString);
      procedure SetGameID(GameID: uInt);
      function GetDict: TDict;
      procedure Add(Original, Translated: AnsiString);
      procedure AddDict(NewDict: TDict; NewDictSize: Integer);
      function GetDictSize: Integer;
      procedure ImportTextFromDir(DirName: AnsiString);
      procedure ReadFrom2Texts(FileName: AnsiString; FileName2: AnsiString);
      procedure ReadFullText(FileName: AnsiString);
      procedure WriteToCompiled(FileName: AnsiString);
      procedure WriteToFullText(FileName: AnsiString);
      procedure WriteToText(Path: AnsiString);
      procedure WriteToDecompiled(FileName: AnsiString);
  end;

const
  NORMAL_FONT_TAG = '//#NormalFont=';
  SPEECH_FONT_TAG = '//#SpeechFont=';
  TEXT_DIRECTION_TAG = '//#TextDirection=';
  END_HEADER = '// ** NOT CHANGE THE EXISTING TEXT.';
  TAG_DEFAULT = 'DEFAULT';
  TAG_DIRECTION_LEFT = 'LEFT';
  TAG_DIRECTION_RIGHT = 'RIGHT';
  TRANS_SIG = 'AGSTranslation';
  passwencstring = 'Avis Durgan';

implementation

constructor TTranslation.Create;
begin
  ExclPrep;
  SetLength(Dict, 0);
  DictSize := 0;
  isNF := False; isSF := False; isDIR := False;
end;

constructor TTranslation.Create(FileName: AnsiString);
var
  ext: AnsiString;
begin
  bVersion2 := False;
  ExclPrep;
  if FileExists(FileName) then
  begin
    SetLength(Dict, 0);
    DictSize := 0;
    isNF := False; isSF := False; isDIR := False;
    ext := ExtractFileExt(FileName);
    if ext = '.tra' then
      ReadFromCompiled(FileName);
    if ext = '.trs' then
      ReadFromDecompiled(FileName);
  end;
end;

destructor TTranslation.Free;
begin
  DictSize := 0;
  SetLength(Dict, 0);
end;

procedure TTranslation.Sort;
var
  i, j, m: Integer;
  buf, buf2: AnsiString;
begin
{  // по длине
  for i := 0 to DictSize - 1 do
    for j := i+1 to DictSize do
    begin
      if Length(Dict[j].Original)<Length(Dict[i].Original) then
      begin
        buf := Dict[i].Original;
        buf2 := Dict[i].Translated;
        Dict[i].Original := Dict[j].Original;
        Dict[i].Translated := Dict[j].Translated;
        Dict[j].Original := buf;
        Dict[j].Translated := buf2;
      end;
    end;
}  // по алфавиту
  for i := 0 to DictSize - 2 do
    for j := i+1 to DictSize - 1 do
    begin
      if (Length(Dict[j].Original)<1) or (Length(Dict[i].Original)<1) then
        Continue;
      m := 1;
      while (Dict[j].Original[m]=Dict[i].Original[m]) and
        (m<=length(Dict[j].Original)) do inc(m);
      if Dict[j].Original[m]<Dict[i].Original[m] then
      begin
        buf := Dict[i].Original;
        buf2 := Dict[i].Translated;
        Dict[i].Original := Dict[j].Original;
        Dict[i].Translated := Dict[j].Translated;
        Dict[j].Original := buf;
        Dict[j].Translated := buf2;
      end;
    end;
end;

function TTranslation.GetGameName: AnsiString;
begin
  Result := wasgamename;
end;

function TTranslation.GetGameID: Integer;
begin
  Result := uidfrom;
end;

procedure TTranslation.SetGameName(GameName: AnsiString);
begin
  wasgamename := GameName;
end;

procedure TTranslation.SetGameID(GameID: uInt);
begin
  uidfrom := GameID;
end;

function TTranslation.getw: Integer;
begin
  tra.Read(Result, 4);
end;

procedure TTranslation.putw(int: Integer);
begin
  tra.Write(int, 4);
end;

function TTranslation.read_string_decrypt: AnsiString;
var
  ss: Integer;
  buf: array of Byte;
begin
  Result := '';
  ss := getw;
  SetLength(buf, ss);
  tra.Read(PChar(buf)^, ss);
  Result := decrypt_text(AnsiString(buf));
  SetLength(buf, 0);
end;

function TTranslation.decrypt_text(Str: AnsiString): AnsiString;
var
  i, adx: Integer;
  nc: AnsiChar;
begin
  Result := '';
  adx := 1;
  for i := 1 to Length(Str) do
  begin
    nc := AnsiChar(Byte(Str[i]) - Byte(passwencstring[adx]));
    if nc <> #0 then
      Result := Result + nc;
    inc(adx);
    if adx = 12 then
      adx := 1;
  end;
end;

procedure TTranslation.write_string_crypt(Str: AnsiString);
var
  ss: Integer;
  buf: AnsiString;
begin
  ss := Length(Str) + 1;
  SetLength(buf, ss);
  buf := crypt_text(Str);
  putw(ss);
  tra.Write(PChar(buf)^, ss);
  SetLength(buf, 0);
end;

function TTranslation.crypt_text(Str: AnsiString): AnsiString;
var
  i, adx: Integer;
begin
  Result := '';
  adx := 1;
  for i := 1 to Length(Str) do
  begin
    Result := Result + AnsiChar(Byte(Str[i]) + Byte(passwencstring[adx]));
    inc(adx);
    if adx = 12 then
      adx := 1;
  end;
  Result := Result + AnsiChar(0 + Byte(passwencstring[adx]));
end;

procedure TTranslation.ReadSpecialTags(Str: AnsiString);
var
  subs: AnsiString;
begin
  if StartsWith(str, NORMAL_FONT_TAG) then
  begin
    normalFont := ReadOptionalInt(Substring(str, NORMAL_FONT_TAG));
    if normalFont <> -1 then
      isNF := True;
  end;
  if StartsWith(str, SPEECH_FONT_TAG) then
  begin
    speechFont := ReadOptionalInt(Substring(str, SPEECH_FONT_TAG));
    if speechFont <> -1 then
      isSF := True;
  end;
  if StartsWith(str, TEXT_DIRECTION_TAG) then
  begin
    subs := Substring(str, TEXT_DIRECTION_TAG);
    if subs = TAG_DEFAULT then
      rightToLeftText := False
    else begin
      isDIR := True;
      if subs = TAG_DIRECTION_LEFT then
        rightToLeftText := False;
      if subs = TAG_DIRECTION_RIGHT then
        rightToLeftText := True;
    end;
  end;
end;

function TTranslation.ReadOptionalInt(Str: AnsiString): Integer;
begin
  if Str = TAG_DEFAULT then
  begin
    Result := -1;
    Exit;
  end;
  Result := StrToInt(Str);
end;

procedure TTranslation.ExclAdd(Str: AnsiString);
begin
  if Str = '' then Exit;
  Inc(exnum);
  SetLength(exstr, exnum);
  exstr[exnum-1] := Str;
end;

procedure TTranslation.ExclPrep;
begin
  exnum := 0;
  SetLength(exstr, 0);
  ExclAdd('__NEWSCRIPTSTART');
  ExclAdd('No hotspot');
  ExclAdd('Hotspot');
end;

function TTranslation.ExclStr(Str: AnsiString): Boolean;
var
  i, m: Integer;
  exexist: Boolean;
begin
  Result := False;
  exexist := True;
  for i := 0 to exnum - 1 do
  begin
    for m := 1 to Length(exstr[i]) do
      if Str[m] <> exstr[i][m] then
      begin
        exexist := False;
        Break;
      end;
    if exexist then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TTranslation.Add(Original, Translated: AnsiString);
var
  i: Integer;
  Found: Boolean;
begin
  if (Original = '') or (ExclStr(Original)){ or (Translated = '')} then
    Exit;
  Found := False;
  for I := 0 to DictSize - 1 do
    if Dict[i].Original = Original then
    begin
      Found := True;
      Break;
    end;
  if not Found then
  begin
    Inc(DictSize);
    SetLength(Dict, DictSize);
    Dict[DictSize-1].Original := Original;
    Dict[DictSize-1].Translated := Translated;
  end;
end;

function TTranslation.GetDict: TDict;
begin
  Result := TDict(Dict);
end;

procedure TTranslation.AddDict(NewDict: TDict; NewDictSize: Integer);
var
  i: Integer;
begin
  for i := 0 to NewDictSize-1 do
    Add(NewDict[i].Original, NewDict[i].Translated);
end;

function TTranslation.GetDictSize: Integer;
begin
  Result := DictSize;
end;

function TTranslation.StartsWith(Str, StartStr: AnsiString): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Length(StartStr) = 0 then
    Result := False;
  for I := 1 to Length(StartStr) do
    if Str[i] <> StartStr[i] then
    begin
      Result := False;
      Break;
    end;
end;

function TTranslation.Substring(Str, Sub: AnsiString): AnsiString;
begin
  Result := Copy(Str, Length(Sub)+1, Length(Str) - Length(Sub));
end;

procedure TTranslation.ReadFromCompiled(FileName: AnsiString);
var
  transsig: array [0..14] of AnsiChar;
  blockType, temp: Integer;
  Original, Translated: AnsiString;
begin
  tra := TFileStream.Create(FileName, fmOpenRead);
  tra.Read(transsig, 15);
  if transsig <> TRANS_SIG then
  begin
    tra.Free;
    Exit;
  end;
  while tra.Position < tra.Size do
  begin
    blockType := getw;
    getw;
    if blockType = -1 then
      Break;
    if blockType = 1 then
    begin
      while True do
      begin
        Original := read_string_decrypt;
        Translated := read_string_decrypt;
        if (Original = '') and (Translated = '') then
          Break
        else
          Add(Original, Translated);
      end;
    end else if blockType = 2 then begin
      tra.Read(uidfrom, 4);
      wasgamename := read_string_decrypt;
    end else if blockType = 3 then begin
      temp := getw;
      if temp >= 0 then
      begin
        isNF := True;
        normalFont := temp;
      end;
      temp := getw;
      if temp >= 0 then
      begin
        isSF := True;
        speechFont := temp;
      end;
      temp := getw;
      if temp = 1 then
      begin
        isDIR := True;
        rightToLeftText := False;
      end;
      if temp = 2 then
      begin
        isDIR := True;
        rightToLeftText := True;
      end;
//    end else begin
    end;
  end;
  tra.Free;
end;

procedure TTranslation.ReadFullText(FileName: AnsiString);
var
  txt: TextFile;
  str: AnsiString;
begin
  AssignFile(txt, FileName);
  Reset(txt);
  while not Eof(txt) do
  begin
    ReadLn(txt, str);
    Add(str, '');
  end;
  CloseFile(txt);
end;

procedure TTranslation.ReadFrom2Texts(FileName: AnsiString; FileName2: AnsiString);
var
  txt, txt2: TextFile;
  str, str2: AnsiString;
begin
  AssignFile(txt, FileName);
  Reset(txt);
  AssignFile(txt2, FileName2);
  Reset(txt2);
  while (not Eof(txt)) and (not Eof(txt2)) do
  begin
    ReadLn(txt, str);
//    while str2 = '' do
    ReadLn(txt2, str2);
//    if str2 = '' then
//      str2 := str;
    Add(str, str2); str := ''; str2 := '';
  end;
  CloseFile(txt2);
  CloseFile(txt);
end;


procedure TTranslation.ReadFromDecompiled(FileName: AnsiString);
var
  trs: TextFile;
  str, Translated: AnsiString;
  i: Integer;
  header: Boolean;
begin
  AssignFile(trs, FileName);
  Reset(trs);
  header := False;
  while not Eof(trs) do
  begin
    str := '';
    if not header then
    begin
      while true do // Read header
      begin
        ReadLn(trs, str);
        if Length(str) >= 2 then
          if StartsWith(str, '//') then
            ReadSpecialTags(str);
        if AnsiCompareStr(str, END_HEADER) = 0 then
        begin
          header := True;
          Break;
        end;
      end;
    end;
    ReadLn(trs, str);
//    if Length(str) >= 2 then
//      if StartsWith(str, '//') then
//      begin
//        ReadSpecialTags(str);
//        Continue;
//      end;
    ReadLn(trs, Translated);
//    if Translated = '' then
//      Break;
    Add(str, Translated);
  end;
  CloseFile(trs);
end;

procedure TTranslation.WriteToCompiled(FileName: AnsiString);
var
  transsig: AnsiString;
  i, dbs: Integer;
  soff, tsize: uInt;
begin
  dbs := 0;
  tra := TFileStream.Create(FileName, fmCreate);
  transsig := TRANS_SIG;
  tra.Write(PAnsiChar(transsig)^, 15); // сигнатура
  putw(2); // второй блок с uid и названим
  putw(8 + Length(wasgamename) + 1); // размер блока
  putw(uidfrom); // uid
  write_string_crypt(wasgamename); // название
  if DictSize > 0 then
  begin
    putw(1); // блок перевода
    soff := tra.Position;
    putw(0);
    for i := 0 to DictSize - 1 do
    begin
      if (Dict[i].Original <> '') and (Dict[i].Translated <> '') then
      begin
        write_string_crypt(Dict[i].Original);
        write_string_crypt(Dict[i].Translated);
      end;
    end;
    write_string_crypt('');
    write_string_crypt('');
    tsize := tra.Position - (soff + 4);
    tra.Seek(soff, soFromBeginning);
    putw(tsize); // правим размер блока
    tra.Seek(0, soFromEnd);
    if not bVersion2 then begin
      putw(3); // Блок флагов
      putw(12); // всегда один размер
      if isNF then
        putw(normalFont)
      else
        putw(-1);
      if isSF then
        putw(speechFont)
      else
        putw(-1);
      if not isDIR then
        putw(-1)
      else
      begin
        if rightToLeftText then
          putw(2)
        else
          putw(1);
      end;
    end;
    putw(-1);
    putw(0);
  end;
  tra.Free;
end;

procedure TTranslation.ImportTextFromDir(DirName: AnsiString);
var
  idir, odir: AnsiString;
  i, nis: Integer;
begin
  SetLength(Dict, 0);
  DictSize := 0; CurStr := 0;
  idir := DirName + 'inbox\';
  odir := DirName + 'outbox\';
  for i := 1 to 99 do
    if FileExists(odir + 'text_' + IntToStr(i) + '.txt') then
    begin
      nis := ReadFromTextOutbox(odir + 'text_' + IntToStr(i) + '.txt');
      if FileExists(idir + 'text_' + IntToStr(i) + '.txt') then
        ReadFromTextInbox(idir + 'text_' + IntToStr(i) + '.txt', nis)
      else
        SkipInboxTranslation(nis);
    end else Break;
    FixupAudioTags;
end;

procedure TTranslation.SkipInboxTranslation(NumStr: Integer);
begin
  inc(CurStr, NumStr);
end;

function TTranslation.IsNum(Chr: AnsiChar): Boolean;
begin
  Result := False;
  if (Chr >= '0') and (Chr <= '9') then
    Result := True;
end;

function TTranslation.ExtractAudioTagInt(Str: AnsiString): Integer;
var
  i: Integer;
  bgn, nbgn: Boolean;
  buf: AnsiString;
begin
  Result := -1;
  if Str[1] <> '&' then
    Exit;
  bgn := False;
  nbgn := False;
  buf := '';
  for i := 1 to Length(Str) do
  begin
    if (not bgn) and (Str[i] = '&') then
      bgn := True
    else if bgn then
    begin
      if not nbgn and IsNum(Str[i]) then nbgn := True;
      if nbgn and (Str[i] = ' ') then
      begin
        Result := StrToInt(buf);
        Break;
      end else if Str[i] <> ' ' then
        if IsNum(Str[i]) then
          buf := buf + Str[i]
        else
        begin
          Result := StrToInt(buf);
          Break;
        end;
    end;
  end;
end;

procedure TTranslation.FixupAudioTags;
var
  i, at: Integer;
begin
  for i := 0 to DictSize - 1 do
    if (Dict[i].Original[1] = '&') and (Dict[i].Translated[1] <> '&') then
    begin
      at := ExtractAudioTagInt(Dict[i].Original);
      Dict[i].Translated := '&' + IntToStr(at) + ' ' + Dict[i].Translated;
    end;
end;

procedure TTranslation.ReadFromTextInbox(FileName: AnsiString; NumStr: Integer);
var
  txt: TextFile; f: File;
  buf, buf2: AnsiString;
  limit, i: Integer;
begin
  limit := 1;
  AssignFile(txt, FileName);
  Reset(txt);
  while not Eof(txt) do
  begin
    ReadLn(txt, buf);
    if UTF8ToAnsi(UTF8String(buf)) = '' then
    begin
//      inc(CurStr);
      Continue;
    end;
    if UTF8ToAnsi(UTF8String(buf)) = 'Внимание! Этот перевод, возможно, ещё не готов.' then
    begin
      Break;
    end;

    Dict[CurStr].Translated := UTF8ToAnsi(UTF8String(buf));
    if limit = 1 then
    begin
      for i := 1 to Length(Dict[CurStr].Translated)-1 do
        Dict[CurStr].Translated[i] := Dict[CurStr].Translated[i+1];
      SetLength(Dict[CurStr].Translated, Length(Dict[CurStr].Translated)-1);
    end;
    ReadLn(txt, buf); // Notabenoid empty string;
    inc(limit);
    inc(CurStr);
    if limit > NumStr then
      Break;
  end;
end;

function TTranslation.ReadFromTextOutbox(FileName: AnsiString): Integer;
var
  txt: TextFile;
begin
  Result := 0;
  AssignFile(txt, FileName);
  Reset(txt);
  while not Eof(txt) do
  begin
    inc(DictSize);
    SetLength(Dict, DictSize);
    ReadLn(txt, Dict[DictSize-1].Original);
    Dict[DictSize-1].Translated := Dict[DictSize-1].Original;
    inc(Result);
  end;
end;

procedure TTranslation.WriteToFullText(FileName: AnsiString);
var
  txt: TextFile;
  i: Integer;
begin
//  Sort;
  AssignFile(txt, FileName);
  ReWrite(txt);
  for i := 0 to DictSize - 1 do
    WriteLn(txt, Dict[i].Original);
  CloseFile(txt);
end;

procedure TTranslation.WriteToText(Path: AnsiString);
var
  txt: TextFile;
  i, fc, cw, limit: Integer;
  cnf: Boolean;
begin
  fc := 0;
  cnf := True;
  limit := 1000;
  cw := 0;
//  Sort;
  for i := 0 to DictSize - 1 do
  begin
    inc(cw);
    if cnf then
    begin
      inc(fc);
      AssignFile(txt, Path + '\text_' + IntToStr(fc) + '.txt');
      ReWrite(txt);
      cnf := False;
    end;
    WriteLn(txt, Dict[i].Original);
    if cw = limit then
    begin
      cnf := True;
      cw := 0;
    end;
    if cnf then
      CloseFile(txt);
  end;
  if not cnf then
    CloseFile(txt);
end;

procedure TTranslation.WriteToDecompiled(FileName: AnsiString);
var
  trs: TextFile;
  i: Integer;
begin
//  Sort;
  AssignFile(trs, FileName);
  ReWrite(trs);
  WriteLn(trs, '// AGS TRANSLATION SOURCE FILE');
  WriteLn(trs, '// Format is alternating lines with original game text and replacement');
  WriteLn(trs, '// text. If you don'+#39+'t want to translate a line, just leave the following');
  WriteLn(trs, '// line blank. Lines starting with '+#39+'//'+#39+' are comments - DO NOT translate');
  WriteLn(trs, '// them. Special characters such as [ and %%s symbolise things within the');
  WriteLn(trs, '// game, so should be left in an appropriate place in the message.');
  WriteLn(trs, '// ');
  WriteLn(trs, '// ** Translation settings are below');
  WriteLn(trs, '// ** Leave them as "DEFAULT" to use the game settings');
  WriteLn(trs, '// The normal font to use - DEFAULT or font number');
  if isNF then
    WriteLn(trs, NORMAL_FONT_TAG + IntToStr(normalFont))
  else
    WriteLn(trs, NORMAL_FONT_TAG + TAG_DEFAULT);
  WriteLn(trs, '// The speech font to use - DEFAULT or font number');
  if isSF then
    WriteLn(trs, SPEECH_FONT_TAG + IntToStr(speechFont))
  else
    WriteLn(trs, SPEECH_FONT_TAG + TAG_DEFAULT);
  WriteLn(trs, '// Text direction - DEFAULT, LEFT or RIGHT');
  if isDIR then
    if rightToLeftText then
      WriteLn(trs, TEXT_DIRECTION_TAG + TAG_DIRECTION_RIGHT)
    else
      WriteLn(trs, TEXT_DIRECTION_TAG + TAG_DIRECTION_LEFT)
  else
    WriteLn(trs, TEXT_DIRECTION_TAG + TAG_DEFAULT);
  WriteLn(trs, '//  ');
  WriteLn(trs, '// ** REMEMBER, WRITE YOUR TRANSLATION IN THE EMPTY LINES, DO');
  WriteLn(trs, '// ** NOT CHANGE THE EXISTING TEXT.');
  for i := 0 to DictSize - 1 do
  begin
    WriteLn(trs, Dict[i].Original);
    WriteLn(trs, Dict[i].Translated);
  end;
  CloseFile(trs);
end;

end.