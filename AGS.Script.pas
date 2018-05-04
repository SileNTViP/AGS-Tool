unit AGS.Script;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, System.SysUtils, System.Types;

type
  ccScript = record
    GlobalData: array of AnsiChar;
    GlobalDataSize: Integer;
    Code: array of Integer;
    CodeSize: Integer;
    Strings: array of AnsiChar;
    StringsSize: Integer;
    FixupTypes: array of AnsiChar;
    Fixups: array of Integer;
    NumFixups: Integer;
    Imports: array of AnsiString;
    NumImports: Integer;
    Exportss: array of AnsiString;
    Export_Addr: array of Integer;
    NumExports: Integer;
    SectionNames: array of AnsiString;
    SectionOffsets: array of Integer;
    NumSections: Integer;
  end;
  TScript = class(TObject)
    private
      Scr: TFileStream;
      scri: ccScript;
      scrstr, scrstr2: array of AnsiString;
      numstr, numstr2: Integer;
      function getw(FS: TfileStream): Integer;
      function freadstring(FS: TfileStream): AnsiString;
      function GetStrByOff(Offset: Integer): AnsiString;
      function IsAChar(Chr: AnsiChar): Boolean;
      procedure ReadFromStream(fs: TFileStream);
      procedure ReadFromFile(FileName: AnsiString);
    public
      constructor Create(FileName: AnsiString); overload;
      constructor Create(FS: TFileStream); overload;
      function GetExtractedStringsCount: Integer;
      function GetExtractedString(Index: Integer): AnsiString;
      function GetExtractedStringsCount2: Integer;
      function GetExtractedString2(Index: Integer): AnsiString;
      procedure ExtractStrings;
      procedure ExtractStrings2;
      procedure ExtractStrings3;
      procedure ExtractStrings4;
      procedure Dump(FileName: AnsiString);
  end;

const
  scfilesig = 'SCOM';
  SCOM_VERSION = 90;
  ENDFILESIG = $beefcafe;
  sccmdnames: array [0..72] of AnsiString = ('NULL', '$add', '$sub', '$$mov', 'memwritelit', 'ret', '$mov',
  '$memread', '$memwrite', '$$mul', '$$div', '$$add', '$$sub', '$$bit_and', '$$bit_or',
  '$$cmp', '$$ncmp', '$$gt', '$$lt', '$$gte', '$$lte', '$$and', '$$or',
  '$call', '$memread.b', '$memread.w', '$memwrite.b', '$memwrite.w', 'jz',
  '$push', '$pop', 'jmp', '$mul', '$farcall', '$farpush', 'farsubsp', 'sourceline',
  '$callscr', 'thisaddr', 'setfuncargs', '$$mod', '$$xor', '$not',
  '$$shl', '$shr', '$callobj', '$checkbounds', '$memwrite.ptr',
  '$memread.ptr', 'memwrite.ptr.0', '$meminit.ptr', 'load.sp.offs',
  'checknull.ptr', '$f.add', '$f.sub', '$$f.mul', '$$f.div', '$$f.add',
  '$$f.sub', '$$f.gt', '$$f.lt', '$$f.gte', '$$f.lte',
  'zeromem', '$newstring', '$$strcmp', '$$strnotcmp', '$checknull',
  'loopcheckoff', 'memwrite.ptr.0.nd', 'jnz', '$dynamicbounds', '$newarray');
  regnames: array[0..7] of AnsiString = ('null', 'sp', 'mar', 'ax', 'bx', 'cx', 'op', 'dx');
  sccmdargs: array [0..72] of Short = (0, 2, 2, 2, 2, 0, 2,
  1, 1, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2,
  1, 1, 1, 1, 1, 1,
  1, 1, 1, 2, 1, 1, 1, 1,
  1, 1, 1, 2, 2, 1,
  2, 2, 1, 2, 1,
  1, 0, 1, 1,
  0, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2,
  1, 1, 2, 2, 1,
  0, 0, 1, 1, 3);

implementation

constructor TScript.Create(FileName: AnsiString);
begin
  ReadFromFile(FileName);
end;

constructor TScript.Create(FS: TFileStream);
begin
  ReadFromStream(FS);
end;

function TScript.getw(FS: TfileStream): Integer;
begin
  FS.Read(Result, 4);
end;

function TScript.freadstring(FS: TfileStream): AnsiString;
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

function TScript.GetExtractedString(Index: Integer): AnsiString;
begin
  Result := scrstr[Index];
end;

function TScript.GetExtractedStringsCount: Integer;
begin
  Result := numstr;
end;

function TScript.GetExtractedString2(Index: Integer): AnsiString;
begin
  Result := scrstr2[Index];
end;

function TScript.GetExtractedStringsCount2: Integer;
begin
  Result := numstr2;
end;

procedure TScript.ExtractStrings;
var
  i: Integer;
begin
  numstr := 0;
  if scri.StringsSize > 0 then
  begin
    inc(numstr);
    SetLength(scrstr, numstr);
    for i := 0 to scri.StringsSize - 1 do
    begin
      if scri.Strings[i] <> #0 then
        scrstr[numstr-1] := scrstr[numstr-1] + scri.Strings[i]
      else begin
        inc(numstr);
        SetLength(scrstr, numstr);
      end;
    end;
  end;
end;

procedure TScript.ExtractStrings2;
var
  i, l: Integer;
begin
  i := 0;
  while i < scri.CodeSize - 1 do
  begin
    if scri.Code[i] = 6 then begin
      if scri.Code[i+2] <= scri.NumFixups then
      begin
        if scri.FixupTypes[scri.Code[i+2]] = #3 then
        begin
          inc(numstr);
          SetLength(scrstr, numstr);
          l := scri.Fixups[scri.Code[i+2]];
          while scri.Strings[l] <> #0 do
          begin
            if scri.Strings[l] <> #0 then
              scrstr[numstr-1] := scrstr[numstr-1] + scri.Strings[l];
            inc(l);
          end;
        end;
        inc(i, 2);
      end;
    end else
      for l := 0 to sccmdargs[scri.Code[i]] - 1 do
      begin
        inc(i);
      end;
    inc(i);
  end;
end;

function TScript.IsAChar(Chr: AnsiChar): Boolean;
begin
  Result := False;
  if (Chr >= ' ') and (Chr <= '~') then
    Result := True;
end;

procedure TScript.ExtractStrings3;
var
  i: Integer;
  sb: Boolean;
begin
  numstr := 0;
  sb := False;
  if scri.StringsSize > 0 then
  begin
    inc(numstr);
    SetLength(scrstr, numstr);
    for i := 0 to scri.StringsSize - 1 do
    begin
      if scri.Strings[i] <> #0 then
      begin
        if IsAChar(scri.Strings[i]) then
          scrstr[numstr-1] := scrstr[numstr-1] + scri.Strings[i]
      end else begin
        inc(numstr);
        SetLength(scrstr, numstr);
        sb := False;
      end;
    end;
  end;
end;

procedure TScript.ExtractStrings4;
var
  i: Integer;
  sb, ns: Boolean;
begin
  numstr := 0;
  sb := False;
  ns := True;
  if scri.StringsSize > 0 then
  begin
    inc(numstr);
    SetLength(scrstr, numstr);
    inc(numstr2);
    SetLength(scrstr2, numstr2);
    for i := 0 to scri.StringsSize - 1 do
    begin
      if IsAChar(scri.Strings[i]) and ns then
      begin
          sb := True;
          ns := False;
      end;
      if sb then begin
        if (scri.Strings[i] <> #0) then
        begin
          scrstr[numstr-1] := scrstr[numstr-1] + scri.Strings[i]
        end else begin
          inc(numstr);
          SetLength(scrstr, numstr);
          sb := False;
          ns := True;
        end
      end else begin
        if (scri.Strings[i] <> #0) then
        begin
          scrstr2[numstr2-1] := scrstr2[numstr2-1] + scri.Strings[i]
        end else begin
          inc(numstr2);
          SetLength(scrstr2, numstr2);
          sb := False;
          ns := True;
        end;
      end;
    end;
  end;
end;

function TScript.GetStrByOff(Offset: Integer): AnsiString;
var
  i: Integer;
begin
  Result := '';
  if (Offset < 0) or (Offset > scri.StringsSize) then
    Exit;
  for i := Offset to scri.StringsSize - 1 do
  begin
    if scri.Strings[i] = #0 then
      Break;
    Result := Result + scri.Strings[i];
  end;
end;

procedure TScript.Dump(FileName: AnsiString);
var
  dmp: TextFile;
  i, t, l, thisop, isreg, numc: Integer;
  buf, toprint: AnsiString;
begin
  AssignFile(dmp, FileName);
  ReWrite(dmp);
  WriteLn(dmp, 'script data size: ' + IntToStr(scri.GlobalDataSize));
  WriteLn(dmp, 'string area size: ' + IntToStr(scri.StringsSize));
  WriteLn(dmp, 'code size: ' + IntToStr(scri.CodeSize));
  WriteLn(dmp, 'SCRIPT VIRTUAL-CODE FOLLOWS:');
  i := 0; numc := 0;
  while i < scri.CodeSize - 1 do
  begin
    isreg := 0;
    thisop := scri.Code[i];
    toprint := sccmdnames[thisop];
    inc(numc);
    if toprint[1] = '$' then
    begin
      isreg := 1;
      for t := 1 to Length(toprint)-1 do
        toprint[t] := toprint[t+1];
      toprint[t] := #0;
    end;
    if toprint[1] = '$' then
    begin
      isreg := isreg or 2;
      for t := 1 to Length(toprint)-1 do
        toprint[t] := toprint[t+1];
      toprint[t] := #0;
    end;
    if Length(toprint) > 6 then
      toprint := toprint + #9
    else
      toprint := toprint + #9 + #9;
    for l := 0 to sccmdargs[thisop] - 1 do
    begin
      inc(i);
      if (l = 0) and ((isreg and 1) = 1) then
        toprint := toprint + ' ' + regnames[scri.Code[i]]
      else if (l = 1) and ((isreg and 2) = 2) then
        toprint := toprint + ' ' + regnames[scri.Code[i]]
      else toprint := toprint + ' ' + IntToStr(scri.Code[i]);
    end;
    if sccmdnames[thisop] = '$mov' then
      toprint := toprint  + ' ; "' + GetStrByOff(scri.Code[i]) + '"';
    WriteLn(dmp, IntToStr(numc) + ' ' + toprint);
    inc(i);
  end;
  CloseFile(dmp);
end;

procedure TScript.ReadFromStream(fs: TFileStream);
var
  buf: array [0..4] of AnsiChar;
  fileVer, i: Integer;
begin
  fs.Read(buf, 4);
  if buf <> scfilesig then
  begin
    Exit;
  end;
  fileVer := getw(fs);
  if fileVer > SCOM_VERSION then
  begin
    Exit;
  end;
  scri.GlobalDataSize := getw(fs);
  scri.CodeSize:= getw(fs);
  scri.StringsSize := getw(fs);
  if scri.GlobalDataSize > 0 then
  begin
    SetLength(scri.GlobalData, scri.GlobalDataSize);
    fs.Read(PAnsiChar(scri.GlobalData)^, scri.GlobalDataSize);
  end;
  if scri.CodeSize > 0 then
  begin
    SetLength(scri.Code, scri.CodeSize);
    fs.Read(PAnsiChar(scri.Code)^, scri.CodeSize*4);
  end;
  if scri.StringsSize > 0 then
  begin
    SetLength(scri.Strings, scri.StringsSize);
    fs.Read(PAnsiChar(scri.Strings)^, scri.StringsSize);
  end;
  scri.NumFixups := getw(fs);
  if scri.NumFixups > 0 then
  begin
    SetLength(scri.FixupTypes, scri.NumFixups);
    SetLength(scri.Fixups, scri.NumFixups);
    fs.Read(PAnsiChar(scri.FixupTypes)^, scri.NumFixups);
    fs.Read(PAnsiChar(scri.Fixups)^, scri.NumFixups*4);
  end;
  scri.NumImports := getw(fs);
  SetLength(scri.Imports, scri.NumImports);
  for i := 0 to scri.NumImports - 1 do
    scri.Imports[i] := freadstring(fs);
  scri.NumExports := getw(fs);
  SetLength(scri.Exportss, scri.NumExports);
  SetLength(scri.Export_Addr, scri.NumExports);
  for i := 0 to scri.NumExports - 1 do
  begin
    scri.Exportss[i] := freadstring(fs);
    scri.Export_Addr[i] := getw(fs);
  end;
  if fileVer >= 83 then
  begin
    scri.NumSections := getw(fs);
    SetLength(scri.SectionNames, scri.NumSections);
    SetLength(scri.SectionOffsets, scri.NumSections);
    for i := 0 to scri.NumSections - 1 do
    begin
      scri.SectionNames[i] := freadstring(fs);
      scri.SectionOffsets[i] := getw(fs);
    end;
  end;
  if uInt(getw(fs)) <> ENDFILESIG then
  begin
    Exit;
  end;
//  Dump('temp.dmp');
end;

procedure TScript.ReadFromFile(FileName: AnsiString);
begin
  Scr := TFileStream.Create(FileName, fmOpenRead);
  ReadFromStream(Scr);
  Scr.Free;
end;

end.