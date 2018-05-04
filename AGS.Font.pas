unit AGS.Font;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, System.SysUtils, System.Types, MyLib;

type
  FNTChar = record
    chSize: Word;
    chOffset: DWORD;
  end;
  FNTHeader = record
    dfVersion: Word;
    dfSize: DWORD;
    dfCopyright: array [0..59] of AnsiChar;
    dfType: Word;
    dfPoints: Word;
    dfVertRes: Word;
    dfHorizRes: Word;
    dfAscent: Word;
    dfInternalLeading: Word;
    dfExternalLeading: Word;
    dfItalic: Byte;
    dfUnderline: Byte;
    dfStrikeOut: Byte;
    dfWeight: Word;
    dfCharSet: Byte;
    dfPixWidth: Word;
    dfPixHeight: Word;
    dfPitchAndFamily: Byte;
    dfAvgWidth: Word;
    dfMaxWidth: Word;
    dfFirstChar: Byte;
    dfLastChar: Byte;
    dfDefaultChar: Byte;
    dfBreakChar: Byte;
    dfWidthBytes: Word;
    dfDevice: DWORD;
    dfFace: DWORD;
    dfBitsPointer: DWORD;
    dfBitsOffset: DWORD;
    dfReserved: Byte;
    dfFlags: DWORD;
    dfAspace: Word;
    dfBspace: Word;
    dfCspace: Word;
    dfColorPointer: DWORD;
    dfReserved1: WORD;
//    szDeviceName: array of AnsiChar;
//    szFaceName: array of AnsiChar;
  end;
  WFNChar = record
    Width:  Word; // Ширина символа
    Height: Word; // Высота символа
    x,y,ox,oy: Byte; // Реальная ширина, высота, и смещения по оси
    xbeg, ybeg: Byte; // Начало символа сверху слева
    Pixels: array of array of Byte; // Точки символа
  end;
  TWFNFont = class(TObject)
    private
      numch, maxh, maxw: Integer;
      chars: array of WFNChar;
      procedure ReadFromBDF(FileName: AnsiString);
      procedure ReadFromWFN(FileName: AnsiString);
      procedure CalcCharParam;
    public
      constructor Create; overload;
      constructor Create(FileName: AnsiString); overload;
      destructor Free; overload;
      procedure ReadFromFNT(FileName: AnsiString);
      procedure WriteToWFN(FileName: AnsiString);
      procedure WriteToWF2(FileName: AnsiString; Version: uInt);
      procedure WriteToBDF(FileName: AnsiString);
  end;

const
  WFN_FILE_SIGNATURE = 'WGT Font File  ';
  WF2_FILE_SIGNATURE = 'WGT Font File 2';

implementation

constructor TWFNFont.Create;
begin
//
end;

constructor TWFNFont.Create(FileName: AnsiString);
var
  ext: AnsiString;
  i: Integer;
begin
  SetLength(chars, 256);
  for i := 0 to 255 do
  begin
    chars[i].Width := 1;
    chars[i].Height := 1;
    SetLength(chars[i].Pixels, 1);
  end;
  ext := ExtractFileExt(FileName);
  if lowercase(ext) = '.wfn' then
    ReadFromWFN(FileName);
  if lowercase(ext) = '.bdf' then
    ReadFromBDF(FileName);
end;

destructor TWFNFont.Free;
var
  i: Integer;
begin
  for i := 0 to 255 do
    SetLength(chars[i].Pixels, 0);
  SetLength(chars, 0);
end;

procedure TWFNFont.ReadFromFNT(FileName: AnsiString);
var
  fnt: TFileStream;
  fnthdr: FNTHeader;
begin
  if not FileExists(FileName) then Exit;
  fnt := TFileStream.Create(FileName, fmOpenRead);
  fnt.Read(fnthdr, SizeOf(FNTHeader));
  fnt.Free;
end;

procedure TWFNFont.ReadFromBDF(FileName: AnsiString);
var
  bdf: TextFile;
  text, comm: AnsiString;
  wit, i, m, n, o, chenc, chw, x, y, ox, oy, chsize: Integer;
  curch: array of array of Byte;
  chbyte: Byte;
begin
  if not FileExists(FileName) then Exit;
  AssignFile(bdf, FileName);
  Reset(bdf);
  while not Eof(bdf) do
  begin
    ReadLn(bdf, text);
    wit := GetWordsCount(' ', text);
    comm := GetWord(' ', text, 1);
    if comm = 'FONTBOUNDINGBOX' then begin
      maxw := StrToInt(GetWord(' ', text, 2));
      maxh := StrToInt(GetWord(' ', text, 3));
    end else if comm = 'CHARS' then
      numch := StrToInt(GetWord(' ', text, 2));
    for i := 0 to numch - 1 do
    begin
      ReadLn(bdf, text);
      comm := GetWord(' ', text, 1);
      if comm = 'STARTCHAR' then begin
        while True do begin
          ReadLn(bdf, text);
          comm := GetWord(' ', text, 1);
          if comm = 'STARTCHAR' then begin
            if StrToInt(GetWord(' ', text, 2)) > numch then Break;
          end else if comm = 'ENCODING' then begin
            chenc := StrToInt(GetWord(' ', text, 2));
          end else if comm = 'SWIDTH' then begin
          end else if comm = 'DWIDTH' then begin
            chw := StrToInt(GetWord(' ', text, 2));
          end else if comm = 'BBX' then begin
            x := StrToInt(GetWord(' ', text, 2));
            y := StrToInt(GetWord(' ', text, 3));
            ox := StrToInt(GetWord(' ', text, 4));
            oy := StrToInt(GetWord(' ', text, 5));
          end else if comm = 'BITMAP' then begin
            SetLength(curch, y);
            for m := 0 to y - 1 do
            begin
              SetLength(curch[m], x);
              ReadLn(bdf, text);
              chsize := ((x - 1) div 8) + 1;
              for n := 0 to chsize - 1 do
              begin
                chbyte := 0;
                chbyte := StrToInt('$' + text[n*2+1] + text[n*2+2]);
                for o := 0 to 7 do
                begin
                  if n*8+o > x - 1 then Break;
                  if (chbyte and $80 = $80) then curch[m][n*8+o] := 1;
                  chbyte := chbyte shl 1;
                end;
              end;
            end;
          end else if comm = 'ENDCHAR' then begin
            if (chenc < 256) {and (x <> 0) and (y <> 0)} then
            begin
              chars[chenc].Width := chw;
              chars[chenc].Height := (maxh + 1) - oy;
              chars[chenc].x := x;
              chars[chenc].y := y;
              chars[chenc].ox := ox;
              chars[chenc].oy := oy;
              chars[chenc].xbeg := ox;
              chars[chenc].ybeg := (maxh + 1) - y - oy;
              SetLength(chars[chenc].Pixels, y);
              for m := 0 to y - 1 do
              begin
                SetLength(chars[chenc].Pixels[m], x);
                for n := 0 to x - 1 do
                  chars[chenc].Pixels[m][n] := curch[m][n];
              end;
            end;
            SetLength(curch, 0);
            Break;
          end;
        end;
      end;
    end;
  end;
  CloseFile(bdf);
end;

procedure TWFNFont.ReadFromWFN(FileName: AnsiString);
var
  wfn: TFileStream;
  buf: array [0..15] of AnsiChar;
  tableoff: Word;
  i, m, n, o, chsize: uInt;
  offsets: array of Word;
  curch: Byte;
begin
  wfn := TFileStream.Create(FileName, fmOpenRead);
  wfn.Read(buf, 15);
  buf[15] := #0;
  if buf <> WFN_FILE_SIGNATURE then
  begin
    wfn.Free;
    Exit;
  end;
  wfn.Read(tableoff, 2);
  if ((wfn.Size - tableoff) div 2) = 128 then
    numch := 128
  else
    numch := 256;
  SetLength(chars, 256);
  SetLength(offsets, numch);
  wfn.Seek(tableoff, soFromBeginning);
  wfn.Read(offsets[0], 2*numch);
  for i := 0 to numch - 1 do
  begin
    wfn.Seek(offsets[i], soFromBeginning);
    wfn.Read(chars[i].Width, 2);
    wfn.Read(chars[i].Height, 2);
    chsize := ((chars[i].Width - 1) div 8) + 1;
    SetLength(chars[i].Pixels, chars[i].Height);
    // Заполнение переходной матрицы
    for m := 0 to chars[i].Height - 1 do
    begin
      SetLength(chars[i].Pixels[m], chars[i].Width);
      for n := 0 to chsize - 1 do
      begin
        wfn.Read(curch, 1);
        for o := 0 to 7 do
        begin
          if n*8+o > chars[i].Width - 1 then Break;
          if (curch and $80 = $80) then chars[i].Pixels[m][(n*8)+o] := 1;
          curch := curch shl 1;
        end;
      end;
    end;
  end;
  SetLength(offsets, 0);
  if numch = 128 then
  for i := 128 to 255 do
  begin
    chars[i].Width := 1;
    chars[i].Height := 1;
    SetLength(chars[i].Pixels, chars[i].Height);
    SetLength(chars[i].Pixels[0], chars[i].Width);
  end;
  wfn.Free;
  CalcCharParam;
end;

procedure TWFNFont.WriteToWFN(FileName: AnsiString);
var
  wfn: TFileStream;
  wfnsig: AnsiString;
  tableoff, i, m, n, o, chsize: Integer;
  offsets: array of Word;
  chbyte: Byte;
  curch: array of array of Byte;
begin
  wfn := TFileStream.Create(FileName, fmCreate);
  wfnsig := WFN_FILE_SIGNATURE;
  wfn.Write(PAnsiChar(wfnsig)^, 15);
  wfn.Write(tableoff, 2); // Table offset write later
  SetLength(offsets, 256);
  for i := 0 to 255 do
  begin
    offsets[i] := wfn.Position;
    wfn.Write(chars[i].Width, 2);
    wfn.Write(chars[i].Height, 2);
    // Заполняем пробелы
    SetLength(curch, chars[i].Height);
    for m := 0 to chars[i].Height - 1 do
      SetLength(curch[m], chars[i].Width);
    for m := 0 to chars[i].y - 1 do
      for n := 0 to chars[i].x - 1 do
        curch[m+chars[i].ybeg][n+chars[i].xbeg] := chars[i].Pixels[m][n];
    chsize := ((chars[i].Width - 1) div 8) + 1;
    for m := 0 to chars[i].Height - 1 do
    begin
      for n := 0 to chsize - 1 do
      begin
        chbyte := 0;
        for o := 0 to 7 do
        begin
          if o > 0 then chbyte := chbyte shl 1;
          if n*8+o < chars[i].Width then
          begin
            if curch[m][n*8+o] = 1 then
              chbyte := chbyte or 1;
          end;
        end;
        wfn.Write(chbyte, 1);
      end;
    end;
    SetLength(curch, 0);
  end;
  tableoff := wfn.Position;
  wfn.Write(offsets[0], 2 * 256);
  SetLength(offsets, 0);
  wfn.Seek(15, soFromBeginning);
  wfn.Write(tableoff, 2);
  wfn.Free;
end;

procedure TWFNFont.WriteToWF2(FileName: AnsiString; Version: uInt);
var
  wfn: TFileStream;
  wfnsig: AnsiString;
  tableoff, i, m, n, o, chsize, ChLimit, offsize: Integer;
  offsets: array of Word;
  chbyte: Byte;
  curch: array of array of Byte;
begin
  if (Version < 1) or (Version > 2) then
    Exit;
  wfn := TFileStream.Create(FileName, fmCreate);
  if Version = 1 then begin
    wfnsig := WFN_FILE_SIGNATURE;
    ChLimit := 127;
    offsize := 2;
  end else begin
    wfnsig := WF2_FILE_SIGNATURE;
    ChLimit := 256;
    offsize := 4;
  end;
  wfn.Write(PAnsiChar(wfnsig)^, 15);
  wfn.Write(tableoff, 2); // Table offset write later
  SetLength(offsets, ChLimit);
  for i := 0 to ChLimit - 1 do
  begin
    offsets[i] := wfn.Position;
    wfn.Write(chars[i].Width, 2);
    wfn.Write(chars[i].Height, 2);
    // Заполняем пробелы
    SetLength(curch, chars[i].Height);
    for m := 0 to chars[i].Height - 1 do
      SetLength(curch[m], chars[i].Width);
    for m := 0 to chars[i].y - 1 do
      for n := 0 to chars[i].x - 1 do
        curch[m+chars[i].ybeg][n+chars[i].xbeg] := chars[i].Pixels[m][n];
    chsize := ((chars[i].Width - 1) div 8) + 1;
    for m := 0 to chars[i].Height - 1 do
    begin
      for n := 0 to chsize - 1 do
      begin
        chbyte := 0;
        for o := 0 to 7 do
        begin
          if o > 0 then chbyte := chbyte shl 1;
          if n*8+o < chars[i].Width then
          begin
            if curch[m][n*8+o] = 1 then
              chbyte := chbyte or 1;
          end;
        end;
        wfn.Write(chbyte, 1);
      end;
    end;
    SetLength(curch, 0);
  end;
  tableoff := wfn.Position;
  wfn.Write(offsets[0], 2 * ChLimit);
  SetLength(offsets, 0);
  wfn.Seek(15, soFromBeginning);
  wfn.Write(tableoff, offsize);
  wfn.Free;
end;

procedure TWFNFont.WriteToBDF(FileName: AnsiString);
var
  txt: TextFile;
  i, m, n, o, resx, resy, numch, chsize: Integer;
  buf: AnsiString;
  chbyte: Byte;
begin
  numch := 256;
  resx := 96;
  resy := 96;
  AssignFile(txt, FileName);
  ReWrite(txt);
  WriteLn(txt, 'STARTFONT 2.1');
  WriteLn(txt, 'COMMENT Exported by AGS Tool by SileNTViP');
  WriteLn(txt, 'FONT');
  WriteLn(txt, Format('SIZE %d %d %d', [maxh, resx, resy]));
  WriteLn(txt, Format('FONTBOUNDINGBOX %d %d %d %d', [maxw, maxh-1, 0, 0]));
  WriteLn(txt, 'STARTPROPERTIES 1');
  WriteLn(txt, Format('FONT_ASCENT %d', [maxh]));
  WriteLn(txt, 'ENDPROPERTIES');
  WriteLn(txt, Format('CHARS %d', [numch]));
  for i := 0 to numch - 1 do
  begin
    WriteLn(txt, Format('STARTCHAR %.3d', [i]));
    WriteLn(txt, Format('ENCODING %d', [i]));
    WriteLn(txt, Format('SWIDTH %d %d', [chars[i].Width*72, 0]));
    WriteLn(txt, Format('DWIDTH %d %d', [chars[i].Width, 0]));
    WriteLn(txt, Format('BBX %d %d %d %d', [chars[i].x, chars[i].y, chars[i].ox, chars[i].oy]));
    WriteLn(txt, 'BITMAP');
    chsize := ((chars[i].x - 1) div 8) + 1;
    for m := 0 to chars[i].y - 1 do
    begin
      buf := '';
      for n := 0 to chsize - 1 do
      begin
        chbyte := 0;
        for o := 0 to 7 do
        begin
          if o > 0 then chbyte := chbyte shl 1;
          if n*8+o < chars[i].x then
          begin
            if chars[i].Pixels[m][n*8+o] = 1 then
              chbyte := chbyte or 1;
          end;
        end;
        buf := buf + Format('%.2x', [chbyte]);
      end;
      WriteLn(txt, buf);
    end;
    WriteLn(txt, 'ENDCHAR');
  end;
  WriteLn(txt, 'ENDFONT');
  CloseFile(txt);
end;

procedure TWFNFont.CalcCharParam;
var
  chsize, i, m, n, o, first, last, firstl, lastl, curstr: Integer;
  clear: Boolean;
  curch: array of array of Byte;
begin
  for i := 0 to numch - 1 do
  begin
    if maxh < chars[i].Height then maxh := chars[i].Height;
    if maxw < chars[i].Width then maxw := chars[i].Width;
  end;
  for o := 0 to 255 do
  begin
    first := -1; last := -1; clear := True; curstr := 0;
    // Вычисляем высоту
    for i := 0 to chars[o].Height - 1 do
    begin
      clear := True;
        for m := 0 to chars[o].Width - 1 do
        if chars[o].Pixels[i][m] <> 0 then
        begin
          clear := False;
          break;
        end;
      if (first < 0) and (not clear) then first := i;
      if (first >= 0) and (not clear) then last := i+1;
    end;
    if (last < 0) and (first < 0) then
    begin
      chars[o].y := 0;
      chars[o].oy := 0;
      chars[o].ybeg := 0;
    end else begin
      chars[o].y := last - first;
      chars[o].oy := maxh - chars[o].y - first;
      chars[o].ybeg := first;
    end;
    first := -1; last := -1;
    // Вычисляем ширину
    for i := 0 to chars[o].Height - 1 do
    begin
      clear := True;
      firstl := -1; lastl := -1;
      for m := 0 to chars[o].Width - 1 do
      begin
        if (firstl < 0) and (chars[o].Pixels[i][m] = 1) then firstl := m;
        if (firstl >= 0) and (chars[o].Pixels[i][m] = 1) then lastl := m+1;
      end;
      if (first < 0) and (firstl >= 0) then first := firstl;
      if (last < 0) and (lastl >= 0) then last := lastl;

      if (first > firstl) and (firstl >= 0) then first := firstl;
      if (lastl > last) and (lastl >= 0) then last := lastl;
    end;
    if (last < 0) and (first < 0) then
    begin
      chars[o].x := 0;
      chars[o].ox := 0;
      chars[o].xbeg := 0;
    end else begin
      chars[o].x := last - first;
      chars[o].ox := first;
      chars[o].xbeg := first;
    end;
    // Обрезаем края
    for i := 0 to chars[o].y - 1 do
    begin
      for m := 0 to chars[o].x - 1 do
        chars[o].Pixels[i][m] := chars[o].Pixels[i+chars[o].ybeg][m+chars[o].xbeg];
      SetLength(chars[o].Pixels[i], chars[o].x);
    end;
    SetLength(chars[o].Pixels, chars[o].y);
  end;
end;

end.
