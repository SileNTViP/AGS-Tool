unit AGS.Sprite;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, System.SysUtils, System.Types, Vcl.ExtCtrls,
  Vcl.Graphics, AGS.GameDta, Xml.XMLIntf, Xml.XMLDoc;

type
  TRGBQuadArray = packed array [0..MaxInt div SizeOf (TRGBQuad) - 1] of TRGBQuad;
  PRGBQuadArray = ^TRGBQuadArray;
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = ARRAY[0..32767] OF TRGBTriple;
  MyImage = record
    Bit: Byte;
    Alpha: Boolean;
    Transparent: Boolean;
    Width, Height: uInt;
    Data: array of array of uInt;
  end;
  SpriteIndex = record
    rspritewidths: Short;
    rspriteheights: Short;
    offsets: uInt;
    coldep: Byte;
    alpha: Boolean;
  end;
  TSprite = class(TObject)
    private
      vers, spriteFileID, spr_initial_offs, numspri, error: Integer;
      spritesAreCompressed: Boolean;
      index: array of SpriteIndex;
      curdir, sprFileName: AnsiString;
      pal: PLogPalette;
      pal4ver: array [0..765] of Byte;
      hpal: HPALETTE;
      image: MyImage;
      function ReadPalette(FS: TFileStream): Boolean;
      procedure GenerateDefaultPalette;
      procedure SetPaletteColor(idx: Integer; R,G,B: Int8);
      procedure ReadFromStream(FS: TFileStream);
      function loadSpriteIndexFile(expectedFileID: Integer; spr_initial_offs: Integer; numspri: Short): Boolean;
      procedure CopyStream(InS, OutS: TFileStream; Size: uInt);
      function GetSpriteColDep(idx: Integer): Byte; overload;
      function GetSpriteColDep(FS: TFileStream; idx: Integer): Byte; overload;
      function IsAlphaSprite(idx: Integer): Boolean; overload;
      function IsAlphaSprite(FS: TFileStream; idx: Integer): Boolean; overload;
      procedure cpackbitl(FS: TFileStream);
      procedure cpackbit16(FS: TFileStream);
      procedure cpackbit32(FS: TFileStream);
      procedure cunpackbitl(FS: TFileStream);
      procedure cunpackbit16(FS: TFileStream);
      procedure cunpackbit32(FS: TFileStream);
    public
      constructor Create; overload;
      constructor Create(FileName: AnsiString); overload;
      constructor Create(FS: TFileStream); overload;
      destructor Free; overload;
      procedure CheckExtract(idx: Integer);
      procedure GenerateSpriteFromDir(Dir: AnsiString; FileName: AnsiString);
      procedure ExtractAllSprites(Path: AnsiString);
      procedure ExtractByIndexRAW(idx: Integer; Path: AnsiString); overload;
      procedure ExtractByIndexRAW(FS: TFileStream; idx: Integer; Path: AnsiString); overload;
      procedure ExtractByIndex(idx: Integer; Path: AnsiString); overload;
      procedure ExtractByIndex(FS: TFileStream; idx: Integer; Path: AnsiString); overload;
      procedure WriteXML(FileName: AnsiString);
  end;

const
  spriteFileSig = ' Sprite File ';
  spindexid = 'SPRINDEX';
  defpalette: array [0..767] of Int8 =
  (
  $00, $00, $00, $00, $00, $2A, $00, $2A, $00, $00,
  $2A, $2A, $2A, $00, $00, $2A, $00, $2A, $2A, $15,
  $00, $2A, $2A, $2A, $15, $15, $15, $15, $15, $3F,
  $15, $3F, $15, $15, $3F, $3F, $3F, $15, $15, $3F,
  $15, $3F, $3F, $3F, $15, $3F, $3F, $3F, $00, $00,
  $00, $05, $05, $05, $08, $08, $08, $0B, $0B, $0B,
  $0E, $0E, $0E, $11, $11, $11, $14, $14, $14, $18,
  $18, $18, $1C, $1C, $1C, $20, $20, $20, $24, $24,
  $24, $28, $28, $28, $2D, $2D, $2D, $32, $32, $32,
  $38, $38, $38, $3F, $3F, $3F, $00, $00, $3F, $11,
  $00, $3F, $1F, $00, $3F, $2E, $25, $00, $2E, $20,
  $00, $3F, $00, $2F, $3F, $00, $20, $3F, $00, $15,
  $3F, $00, $00, $3F, $11, $00, $20, $1E, $1E, $22,
  $1D, $1C, $21, $02, $01, $05, $01, $01, $31, $05,
  $01, $25, $22, $21, $11, $03, $01, $25, $1F, $1E,
  $2C, $2A, $2A, $29, $20, $1F, $16, $15, $15, $20,
  $1D, $1C, $25, $1E, $1C, $30, $28, $25, $26, $20,
  $1D, $2B, $26, $23, $1A, $0B, $03, $28, $24, $21,
  $27, $13, $06, $2C, $20, $17, $32, $1B, $08, $33,
  $30, $2C, $2C, $2A, $25, $2F, $2D, $29, $22, $21,
  $1E, $3A, $39, $31, $35, $34, $32, $1D, $1C, $17,
  $3A, $3A, $3A, $2F, $2F, $0B, $36, $35, $04, $24,
  $24, $17, $2A, $2A, $11, $1F, $20, $1A, $31, $32,
  $30, $00, $39, $33, $0A, $32, $2F, $14, $2B, $2A,
  $01, $2E, $2C, $04, $24, $27, $2C, $2F, $30, $2F,
  $35, $36, $26, $2D, $30, $0E, $26, $35, $1C, $2C,
  $36, $00, $1E, $32, $18, $1A, $1C, $1C, $1E, $20,
  $2D, $32, $36, $20, $29, $30, $23, $29, $2D, $1B,
  $25, $30, $16, $23, $30, $1D, $24, $2B, $11, $20,
  $30, $1E, $21, $24, $08, $13, $23, $10, $13, $18,
  $11, $13, $15, $11, $18, $23, $1C, $1E, $23, $13,
  $15, $19, $0D, $0F, $13, $14, $1B, $26, $18, $1A,
  $1E, $19, $1E, $28, $14, $17, $1C, $18, $1B, $23,
  $02, $08, $16, $18, $1B, $20, $16, $19, $1E, $02,
  $08, $19, $16, $19, $21, $02, $06, $14, $14, $16,
  $1E, $02, $06, $16, $00, $04, $14, $00, $04, $16,
  $1C, $1C, $20, $0E, $10, $1A, $12, $14, $20, $0A,
  $0C, $18, $09, $0C, $1D, $06, $09, $19, $06, $09,
  $1E, $01, $04, $12, $0C, $0E, $1A, $0C, $0E, $1D,
  $0B, $0E, $22, $06, $08, $16, $02, $06, $19, $10,
  $12, $22, $0E, $10, $1F, $04, $06, $14, $04, $06,
  $16, $02, $04, $14, $20, $21, $25, $11, $12, $1E,
  $04, $06, $18, $02, $04, $16, $00, $02, $16, $18,
  $18, $1C, $1A, $1B, $23, $15, $16, $21, $09, $0A,
  $18, $06, $07, $14, $0E, $0E, $18, $0B, $0C, $1B,
  $09, $0A, $16, $06, $08, $29, $08, $0A, $36, $06,
  $08, $36, $05, $07, $32, $06, $09, $39, $04, $06,
  $36, $03, $05, $30, $02, $05, $3A, $18, $18, $23,
  $12, $13, $1C, $02, $02, $12, $0B, $0C, $3A, $08,
  $0A, $39, $04, $06, $39, $1A, $1A, $1C, $1E, $1E,
  $20, $1A, $1A, $20, $1C, $1C, $23, $18, $18, $1E,
  $16, $16, $1C, $18, $18, $20, $16, $16, $1E, $06,
  $06, $09, $11, $11, $1A, $14, $14, $1F, $0E, $0E,
  $1B, $0C, $0C, $18, $18, $18, $39, $06, $06, $16,
  $04, $04, $14, $04, $04, $16, $1F, $1E, $23, $0B,
  $0B, $0F, $11, $10, $18, $15, $14, $1C, $09, $07,
  $14, $08, $06, $18, $06, $04, $16, $04, $03, $12,
  $0E, $0C, $16, $0E, $0C, $18, $0E, $0C, $1B, $16,
  $14, $1F, $10, $0E, $19, $1A, $18, $22, $11, $0E,
  $1E, $13, $10, $1A, $14, $12, $18, $1A, $18, $20,
  $18, $16, $1E, $19, $16, $21, $0D, $0A, $16, $15,
  $12, $1C, $1A, $18, $1E, $1E, $1C, $23, $18, $16,
  $1C, $24, $23, $25, $11, $0D, $16, $16, $12, $1A,
  $1E, $1C, $20, $1C, $1A, $1E, $1D, $1A, $20, $1A,
  $16, $1E, $18, $14, $1C, $1A, $18, $1C, $22, $20,
  $23, $19, $11, $1E, $17, $14, $19, $1C, $18, $1E,
  $15, $10, $18, $23, $1B, $27, $1A, $16, $1C, $1E,
  $15, $21, $28, $1E, $2A, $1D, $16, $1E, $21, $16,
  $22, $20, $1C, $20, $1E, $1A, $1E, $1F, $12, $1E,
  $1C, $1B, $1C, $1E, $18, $1D, $1B, $15, $19, $25,
  $1A, $22, $23, $14, $1E, $27, $26, $27, $1C, $18,
  $1B, $2D, $1E, $27, $2C, $16, $23, $26, $0E, $1C,
  $1D, $1D, $1D, $20, $1C, $1E, $35, $15, $22, $24,
  $1E, $21, $2F, $07, $16, $22, $1F, $20, $22, $1C,
  $1E, $1F, $18, $1A, $1F, $09, $0D, $21, $1A, $1B,
  $1E, $1B, $1B, $39, $38, $38, $2D, $2D, $2D, $32,
  $32, $32, $28, $28, $28, $1E, $1E, $1E
  );

implementation

constructor TSprite.Create;
begin
  sprFileName := '';
  spr_initial_offs := 0;
  spriteFileID := 0;
  SetLength(index, 0);
  sprFileName := '';
end;

constructor TSprite.Create(FileName: AnsiString);
var
  FS: TFileStream;
begin
  curdir := ExtractFilePath(FileName);
  sprFileName := FileName;
  FS := TFileStream.Create(FileName, fmOpenRead);
  spr_initial_offs := 0;
  ReadFromStream(FS);
  FS.Free;
end;

constructor TSprite.Create(FS: TFileStream);
begin
  curdir := ExtractFilePath(FS.FileName);
  spr_initial_offs := FS.Position;
  ReadFromStream(FS);
end;


destructor TSprite.Free;
begin
//
end;

function TSprite.ReadPalette(FS: TFileStream): Boolean;
var
  dtapath, dtaname: AnsiString;
  dta: TFileStream;
  gss: GameSetupStructBase;
  buf: array [0..39] of AnsiChar;
  DtaVer, stlen, aa: Integer;
  pcol: TRGBQuad;
begin
  Result := False;
  dtapath := ExtractFilePath(FS.FileName);
  if FileExists(dtapath + 'ac2game.dta') then
    dtaname := dtapath + 'ac2game.dta'
  else if FileExists(dtapath + 'game28.dta') then
    dtaname := dtapath + 'game28.dta'
  else
    Exit;
  dta := TFileStream.Create(dtaname, fmOpenRead);
  dta.Read(buf, 30);
  buf[30] := #0;
  if buf <> game_file_sig then
  begin
    Exit;
  end;
  dta.Read(DtaVer, 4);
  if (DtaVer <> kGameVersion_321) and (DtaVer <> kGameVersion_301) and (DtaVer <> kGameVersion_272) then
  begin
    dta.Free;
    Exit;
  end;
  dta.Read(stlen, 4);
  dta.Seek(stlen, soFromCurrent);
  dta.Read(GSS, SizeOf(GameSetupStructBase));
  dta.Free;

  GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 255);
  pal.palVersion := $300;
  pal.palNumEntries := 256;
  for aa := 0 to 255 do
  begin
    pcol := TRGBQuad(GSS.DefPal[aa]);
    pal.palPalEntry[aa].peRed := pcol.rgbBlue * 4;
    pal.palPalEntry[aa].peGreen := pcol.rgbGreen * 4;
    pal.palPalEntry[aa].peBlue := pcol.rgbRed * 4;
  end;
  Result := True;
end;

procedure TSprite.ReadFromStream(FS: TFileStream);
var
  buff: array of AnsiChar;
  aa, compsize: Integer;
  coldep: Byte;
begin
  FS.Read(vers, 2);
  if (vers < 4) or (vers > 6) then
  begin
    error := -1;
    Exit;
  end;
  SetLength(buff, 13);
  FS.Read(PChar(buff)^, 13);
  if AnsiString(buff) <> spriteFileSig then
  begin
    error := -1;
    Exit;
  end;
  SetLength(buff, 0);
  if vers = 4 then
    spritesAreCompressed := False
  else if vers = 5 then
    spritesAreCompressed := True
  else if vers = 6 then
  begin
    FS.Read(spritesAreCompressed, 1);
    FS.Read(spriteFileID, 4);
  end;
  // Skip palette
  if vers < 5 then
    FS.Read(pal4ver, 256*3);
  // Read game pallete
  if not ReadPalette(FS) then
    GenerateDefaultPalette;
  FS.Read(numspri, 2);
  // if there is a sprite index file, use it
  if loadSpriteIndexFile(spriteFileID, spr_initial_offs, numspri) then
  begin
    error := 0;
    Exit;
  end;
  // failed, delete the index file because it's invalid
  SetLength(index, numspri + 1);
  // no sprite index file, manually index it
  for aa := 0 to numspri do
  begin
    FS.Read(index[aa].coldep, 2);
    if index[aa].coldep = 0 then begin
      index[aa].rspritewidths := 0;
      index[aa].rspriteheights := 0;
      index[aa].offsets := 0;
    end else begin
      index[aa].offsets := FS.Position - 2;
      FS.Read(index[aa].rspritewidths, 2);
      FS.Read(index[aa].rspriteheights, 2);
      if spritesAreCompressed then begin
        FS.Read(compsize, 4);
        FS.Seek(compsize, soFromCurrent);
      end else
        FS.Seek(index[aa].rspritewidths * index[aa].rspriteheights * index[aa].coldep, soFromCurrent);
    end;
  end;
end;

function TSprite.loadSpriteIndexFile(expectedFileID: Integer; spr_initial_offs: Integer; numspri: Short): Boolean;
var
  spridx: TFileStream;
  buff: array of AnsiChar;
  fileVersion, fid, numspri_index, numspri_index2, numsprits, aa: Integer;
begin
  if not FileExists(curdir + 'sprindex.dat') then
  begin
    Result := False;
    Exit;
  end;
  spridx := TFileStream.Create(curdir + 'sprindex.dat', fmOpenRead);
  SetLength(buff, 8);
  spridx.Read(PChar(buff)^, 8);
  if AnsiString(buff) <> spindexid then
  begin
    Result := False;
    spridx.Free;
    Exit;
  end;
  SetLength(buff, 0);
  spridx.Read(fileVersion, 4);
  if (fileVersion < 1) or (fileVersion > 2) then
  begin
    Result := False;
    spridx.Free;
    Exit;
  end;
  if fileVersion >= 2 then
  begin
    spridx.Read(fid, 4);
    if fid <> expectedFileID then
    begin
      Result := False;
      spridx.Free;
      Exit;
    end;
  end;
  spridx.Read(numspri_index, 4);
  spridx.Read(numspri_index2, 4);
  if numspri_index2 <> numspri_index + 1 then
  begin
    Result := False;
    spridx.Free;
    Exit;
  end;
  if numspri_index <> numspri then
  begin
    Result := False;
    spridx.Free;
    Exit;
  end;
  numsprits := numspri + 1;
  SetLength(index, numsprits + 1);
  for aa := 0 to numsprits - 1 do
    spridx.Read(index[aa].rspritewidths, 2);
  for aa := 0 to numsprits - 1 do
    spridx.Read(index[aa].rspriteheights, 2);
  for aa := 0 to numsprits - 1 do
    spridx.Read(index[aa].offsets, 4);
  for aa := 0 to numsprits - 1 do
  begin
    if index[aa].offsets <> 0 then
      inc(index[aa].offsets, spr_initial_offs)
    else begin
      // no sprite ... blank it out
      index[aa].rspritewidths := 0;
      index[aa].rspriteheights := 0;
      index[aa].offsets := 0;
    end;
  end;
  spridx.Free;
  Result := True;
end;

procedure TSprite.CopyStream(InS, OutS: TFileStream; Size: uInt);
var
  buf: array of Byte;
  need: uInt;
const
  bufsize = 20 * 1024 * 1024; // Copy 20 MB
begin
  if Size <= 0 then
    Exit;
  need := Size;
  if Size < bufsize then
  begin
    SetLength(buf, Size);
    InS.Read(PChar(buf)^, Size);
    OutS.Write(PChar(buf)^, Size);
    SetLength(buf, 0);
  end else begin
    while need > bufsize do
    begin
      SetLength(buf, bufsize);
      InS.Read(PChar(buf)^, bufsize);
      OutS.Write(PChar(buf)^, bufsize);
      SetLength(buf, 0);
      need := need - bufsize;
    end;
    if (need < bufsize) and (need <> 0) then
    begin
      SetLength(buf, need);
      InS.Read(PChar(buf)^, need);
      OutS.Write(PChar(buf)^, need);
      SetLength(buf, 0);
    end;
  end;
end;

procedure TSprite.GenerateSpriteFromDir(Dir: AnsiString; FileName: AnsiString);
var
  dat, spr, spridx: TFileStream;
  idxver, aa, bb, cc, numspri_index, numspri_index2, nvar: Integer;
  coldep: Word;
  getspr: AnsiString;
  bit: TBitmap;
  g: TRGBQuad;
  rgb: TRGBTriple;
  Row8bit:  pByteArray;
  Row16bit: pWordArray;
  Row24bit: PRGBTripleArray;
  Row32bit: PRGBQuadArray;
begin
  idxver := 1;

  if not FileExists(Dir + '\sprlib.dat') then
    Exit;
  // Читаем инфу о собираемой спрайтовой библиотеке
  dat := TFileStream.Create(Dir + '\sprlib.dat', fmOpenRead);
  dat.Read(vers, 2);
  dat.Read(numspri, 2);
  if vers = 4 then
    dat.Read(pal4ver, 256*3);
  if vers = 6 then
  begin
    dat.Read(SpritesAreCompressed, 1);
    dat.Read(spriteFileID, 4);
  end;
  dat.Free;
  if vers = 2 then
    SpritesAreCompressed := False
  else if vers = 5 then
    SpritesAreCompressed := True;

  // Начинаем собирать библиотеку
  spr := TFileStream.Create(FileName, fmCreate);
  SetLength(index, numspri + 1);
  spr.Write(vers, 2);
  spr.Write(AnsiString(spriteFileSig), 13);
  if vers = 6 then
  begin
    spr.Write(SpritesAreCompressed, 1);
    spr.Write(spriteFileID, 4);
  end;
  if vers < 5 then
    spr.Write(pal4ver, 256*3);
  spr.Write(numspri, 2);
  for aa := 0 to numspri do
  begin
    getspr := Dir + format('\spr%.5d.bmp', [aa]);
    if not FileExists(getspr) then
    begin
      index[aa].rspritewidths := 0;
      index[aa].rspriteheights := 0;
      index[aa].offsets := 0;
      nvar := 0;
      spr.Write(nvar, 2); // coldep
      Continue;
    end;
    index[aa].offsets := spr.Position;
    bit := TBitmap.Create;
    bit.LoadFromFile(getspr);
    index[aa].rspritewidths := bit.Width;
    index[aa].rspriteheights := bit.Height;
    if bit.PixelFormat = pf8bit then coldep := 1
    else if bit.PixelFormat = pf16bit then coldep := 2
    else if bit.PixelFormat = pf24bit then coldep := 3
    else if bit.PixelFormat = pf32bit then coldep := 4;
    spr.Write(coldep, 2);
    spr.Write(index[aa].rspritewidths, 2);
    spr.Write(index[aa].rspriteheights, 2);
    // Чтение Bitmap в Image.Data
    Image.Width := index[aa].rspritewidths;
    Image.Height := index[aa].rspriteheights;
    SetLength(Image.Data, index[aa].rspriteheights);
    for bb := 0 to index[aa].rspriteheights - 1 do
      SetLength(Image.Data[bb], index[aa].rspritewidths);
    for bb := 0 to index[aa].rspriteheights - 1 do
    begin
      if coldep = 1 then
        Row8bit := bit.ScanLine[bb]
      else if coldep = 2 then
        Row16bit := bit.ScanLine[bb]
      else if coldep = 3 then
        Row24bit := bit.ScanLine[bb]
      else if coldep = 4 then
        Row32bit := bit.ScanLine[bb];
      for cc := 0 to index[aa].rspritewidths - 1 do
      begin
        if coldep = 1 then begin
          Image.Data[bb][cc] := Byte(Row8bit[cc]);
        end else if coldep = 2 then begin
          Image.Data[bb][cc] := Word(Row16bit[cc]);
        end else if coldep = 3 then begin
          rgb := Row24bit[cc];
//          Integer(Image.Data[bb][cc]) := rgb;
        end else if coldep = 4 then begin
          Image.Data[bb][cc] := Integer(Row32bit[cc]);
        end;
      end;
    end;
    bit.Free;
    // Запись упакованных и не упакованных данных
    if SpritesAreCompressed then begin
        if coldep = 1 then begin
          cpackbitl(spr);
        end else if coldep = 2 then begin
          cpackbit16(spr);
        end else begin
          cpackbit32(spr);
        end;
    end else begin
      for bb := 0 to index[aa].rspriteheights - 1 do
      begin
        for cc := 0 to index[aa].rspritewidths - 1 do
        begin
          FillChar(g, SizeOf(TRGBQuad), 0);
          if coldep = 1 then begin
            spr.Write(Image.Data[bb][cc], 1);
          end else if coldep = 2 then begin
            spr.Write(Image.Data[bb][cc], 2);
          end else if coldep = 3 then begin
            spr.Write(Image.Data[bb][cc], 3);
          end else if coldep = 4 then begin
            spr.Write(Image.Data[bb][cc], 4);
          end;
        end;
      end;
    end;
  end;
  spr.Free;

  // Собираем файл спрайтовых индексов
  spridx := TFileStream.Create(ExtractFilePath(FileName) + 'sprindex.dat', fmCreate);
  spridx.Write(AnsiString(spindexid), 8);
  if spriteFileID <> 0 then
    idxver := 2;
  spridx.Write(idxver, 4);
  if idxver = 2 then
    spridx.Write(spriteFileID, 4);
  numspri_index := numspri;
  numspri_index2 := numspri + 1;
  spridx.Write(numspri_index, 4);
  spridx.Write(numspri_index2, 4);
  for aa := 0 to numspri do
    spridx.Write(index[aa].rspritewidths, 2);
  for aa := 0 to numspri do
    spridx.Write(index[aa].rspriteheights, 2);
  for aa := 0 to numspri do
    spridx.Write(index[aa].offsets, 4);
  spridx.Free;
end;

procedure TSprite.ExtractAllSprites(Path: AnsiString);
var
  aa: Integer;
  newspr: AnsiString;
  dat: TFileStream;
begin
  ForceDirectories(Path);
  dat := TFileStream.Create(Path + 'sprlib.dat', fmCreate);
  dat.Write(vers, 2);
  dat.Write(numspri, 2);
  if vers = 4 then
    dat.Write(pal4ver, 256*3);
  if vers = 6 then
  begin
    dat.Write(SpritesAreCompressed, 1);
    dat.Write(spriteFileID, 4);
  end;
  dat.Free;
  for aa := 0 to numspri do
    ExtractByIndex(aa, Path);
end;

procedure TSprite.CheckExtract(idx: Integer);
begin
  ExtractByIndex(idx, '');
  ExtractByIndexRAW(idx, '');
end;

function TSprite.GetSpriteColDep(idx: Integer): Byte;
var
  spr: TFileStream;
begin
  spr := TFileStream.Create(sprFileName, fmOpenRead);
  Result := GetSpriteColDep(spr, idx);
  spr.Free;
end;


function TSprite.GetSpriteColDep(FS: TFileStream; idx: Integer): Byte;
begin
  if (idx < 0) or (idx > numspri) then
    Exit;
  if index[idx].offsets = 0 then
    Exit;
  FS.Seek(index[idx].offsets, soFromBeginning);
  FS.Read(Result, 2);
end;

procedure TSprite.ExtractByIndexRAW(idx: Integer; Path: AnsiString);
var
  spr: TFileStream;
begin
  spr := TFileStream.Create(sprFileName, fmOpenRead);
  ExtractByIndexRAW(spr, idx, Path);
  spr.Free;
end;

procedure TSprite.ExtractByIndexRAW(FS: TFileStream; idx: Integer; Path: AnsiString);
var
  newspr: TFileStream;
  sprname: AnsiString;
  coldep, wdd, htt: Short;
begin
  if (idx < 0) or (idx > numspri) then
    Exit;
  if index[idx].offsets = 0 then
    Exit;
  sprname := format(Path+'spr%.5d.raw', [idx]);
  if Path <> '' then
    ForceDirectories(Path);
  FS.Seek(index[idx].offsets, soFromBeginning);
  FS.Read(coldep, 2);
  FS.Read(wdd, 2);
  FS.Read(htt, 2);
  newspr := TFileStream.Create(sprname, fmCreate);
  CopyStream(FS, newspr, wdd * htt * coldep);
  newspr.Free;
end;

function TSprite.IsAlphaSprite(idx: Integer): Boolean;
var
  spr: TFileStream;
begin
  spr := TFileStream.Create(sprFileName, fmOpenRead);
  Result := IsAlphaSprite(spr, idx);
  spr.Free;
end;

function TSprite.IsAlphaSprite(FS: TFileStream; idx: Integer): Boolean;
var
  aa, bb: Integer;
  coldep, htt, wdd: Word;
  g: TRGBQuad;
begin
  Result := False;
  if (idx < 0) or (idx > numspri) then
    Exit;
  if index[idx].offsets = 0 then
    Exit;
  FS.Seek(index[idx].offsets, soFromBeginning);
  FS.Read(coldep, 2);
  FS.Read(wdd, 2);
  FS.Read(htt, 2);
  if coldep <> 4 then
    Exit;
  for aa := 0 to htt - 1 do
    for bb := 0 to wdd - 1 do
    begin
      FS.Read(g, 4);
      if g.rgbReserved > 0 then
      begin
        Result := True;
        Break;
      end;
    end;
end;

procedure TSprite.ExtractByIndex(idx: Integer; Path: AnsiString);
var
  spr: TFileStream;
begin
  spr := TFileStream.Create(sprFileName, fmOpenRead);
  ExtractByIndex(spr, idx, Path);
  spr.Free;
end;

procedure TSprite.ExtractByIndex(FS: TFileStream; idx: Integer; Path: AnsiString);
var
  coldep, wdd, htt: Short;
  aa, bb, col, packsize: Integer;
  bit: TBitmap;
  g: TRGBQuad;
  rgb: TRGBTriple;
  Row8bit:  pByteArray;
  Row16bit: pWordArray;
  Row24bit: PRGBTripleArray;
  Row32bit: PRGBQuadArray;
begin
  if (idx < 0) or (idx > numspri) then
    Exit;
  if index[idx].offsets = 0 then
    Exit;
  if Path <> '' then
    ForceDirectories(Path);
  FS.Seek(index[idx].offsets, soFromBeginning);
  FS.Read(coldep, 2);
  FS.Read(wdd, 2);
  FS.Read(htt, 2);
  index[idx].rspritewidths := wdd;
  index[idx].rspriteheights := htt;
  index[idx].coldep := coldep;

  Image.Bit := coldep * 8;
  Image.Width := wdd;
  Image.Height := htt;
  SetLength(Image.Data, Image.Height);
  for aa := 0 to Image.Height - 1 do
    SetLength(Image.Data[aa], Image.Width);

  // Read Sprite data
  if spritesAreCompressed then begin
    FS.Read(packsize, 4);
    if coldep = 1 then
      cunpackbitl(FS)
    else if coldep = 2 then
      cunpackbit16(FS)
    else
      cunpackbit32(FS);
  end else begin
    for aa := 0 to Image.Height - 1 do
      for bb := 0 to Image.Width - 1 do
        FS.Read(Image.Data[aa][bb], coldep);
  end;

  // Create Bitmap
  bit := TBitmap.Create;
  bit.Width := wdd;
  bit.Height := htt;
  if coldep = 1 then
  begin
    bit.PixelFormat := pf8bit;
    bit.Palette := CreatePalette(pal^);;
  end else if coldep = 2 then begin
      bit.PixelFormat := pf16bit;
  end else if coldep = 3 then begin
      bit.PixelFormat := pf24bit;
  end else if coldep = 4 then begin
      bit.PixelFormat := pf32bit;
  end;
  // Write Bitmap data
  for aa := 0 to htt - 1 do
  begin
    if coldep = 1 then
      Row8bit := bit.ScanLine[aa]
    else if coldep = 2 then
      Row16bit := bit.ScanLine[aa]
    else if coldep = 3 then
      Row24bit := bit.ScanLine[aa]
    else if coldep = 4 then
      Row32bit := bit.ScanLine[aa];
    for bb := 0 to wdd - 1 do
    begin
      FillChar(g, SizeOf(TRGBQuad), 0);
      if coldep = 1 then begin
        if Image.Data[aa][bb] < 256 then
          Row8bit[bb] := Byte(Image.Data[aa][bb]);
      end else if coldep = 2 then begin
        Row16bit[bb] := Image.Data[aa][bb];
      end else if coldep = 3 then begin
        Row24bit[bb].rgbtBlue := (Image.Data[aa][bb] or $00FFFF) shr 16;
        Row24bit[bb].rgbtGreen := (Image.Data[aa][bb] or $FF00FF) shr 8;
        Row24bit[bb].rgbtRed := Image.Data[aa][bb] or $FFFF00;
      end else if coldep = 4 then begin
        Integer(g) := Image.Data[aa][bb];
        Row32bit[bb] := g;
      end;
    end;
  end;
  bit.SaveToFile(Path + format('spr%.5d.bmp', [idx]));
  bit.Free;
  SetLength(Image.Data, 0);
end;

procedure TSprite.WriteXML(FileName: AnsiString);
var
  Root, Game, Sprites, SpriteFolder, Sprites2, Sprite: IXMLNode;
  XMLDocument1: TXMLDocument;
  aa: Integer;
begin
  XMLDocument1 := TXMLDocument.Create(nil);
  XMLDocument1.Active := true;
  XMLDocument1.Version := '1.0';
  XMLDocument1.Encoding := 'windows-1251';
  XMLDocument1.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull, doAutoPrefix, doNamespaceDecl];
  Root := XMLDocument1.AddChild('AGSEditorDocument');
  Root.Attributes['Version'] := '3.0.3.2';
  Root.Attributes['VersionIndex'] := '6';
  Root.Attributes['EditorVersion'] := '3.2.1';
  Game := Root.AddChild('Game');
  Sprites := Game.AddChild('Sprites');
  SpriteFolder := Sprites.AddChild('SpriteFolder');
  SpriteFolder.Attributes['Name'] := 'Main';
  Sprites2 := Sprites.AddChild('Sprites');
  for aa := 0 to numspri - 1 do
  begin
    Sprite := Sprites2.AddChild('Sprite');
    Sprite.Attributes['Slot'] := IntToStr(aa);
    Sprite.Attributes['Width'] := IntToStr(index[aa].rspritewidths);
    Sprite.Attributes['Height'] := IntToStr(index[aa].rspriteheights);
    index[aa].coldep := GetSpriteColDep(aa);
    Sprite.Attributes['ColorDepth'] := IntToStr(index[aa].coldep*8);
    Sprite.Attributes['Resolution'] := 'LowRes';
    if IsAlphaSprite(aa) then
      Sprite.Attributes['AlphaChannel'] := 'True'
    else
      Sprite.Attributes['AlphaChannel'] := 'False'
  end;
  XMLDocument1.SaveToFile(FileName);
  XMLDocument1.Free;
end;

procedure TSprite.cpackbitl(FS: TFileStream);
var
  aa, bb, cnt, i, j, jmax, nvar, begoffs, endoffs: Integer;
begin
  begoffs := FS.Position;
  nvar := 0;
  FS.Write(nvar, 4);
  for aa := 0 to Image.Height - 1 do
  begin
    cnt := 0;
    while cnt < Image.Width do
    begin
      i := cnt;
      j := i + 1;
      jmax := i + 126;
      if jmax >= Image.Width then
        jmax := Image.Width - 1;
      if i = Image.Width - 1 then
      begin
        nvar := 0;
        FS.Write(nvar, 1);
        FS.Write(Image.Data[aa][i], 1);
        inc(cnt);
      end else if Image.Data[aa][i] = Image.Data[aa][j] then begin
        while (j < jmax) and (Image.Data[aa][j] = Image.Data[aa][j+1]) do
          inc(j);
        nvar := i - j;
        FS.Write(nvar, 1);
        FS.Write(Image.Data[aa][i], 1);
        cnt := cnt + j - i + 1;
      end else begin
        while (j < jmax) and (Image.Data[aa][j] <> Image.Data[aa][j+1]) do
          inc(j);
        nvar := j - i;
        FS.Write(nvar, 1);
        for bb := 0 to j - i do
          FS.Write(Image.Data[aa][i+bb], 1);
        cnt := cnt + j - i + 1;
      end;
    end;
  end;
  endoffs := FS.Position;
  nvar := endoffs - begoffs - 4;
  FS.Seek(begoffs, soFromBeginning);
  FS.Write(nvar, 4);
  FS.Seek(endoffs, soFromBeginning);
end;

procedure TSprite.cpackbit16(FS: TFileStream);
var
  aa, bb, cnt, i, j, jmax, nvar, begoffs, endoffs: Integer;
begin
  begoffs := FS.Position;
  nvar := 0;
  FS.Write(nvar, 4);
  for aa := 0 to Image.Height - 1 do
  begin
    cnt := 0;
    while cnt < Image.Width do
    begin
      i := cnt;
      j := i + 1;
      jmax := i + 126;
      if jmax >= Image.Width then
        jmax := Image.Width - 1;
      if i = Image.Width - 1 then
      begin
        nvar := 0;
        FS.Write(nvar, 1);
        FS.Write(Image.Data[aa][i], 2);
        inc(cnt);
      end else if Image.Data[aa][i] = Image.Data[aa][j] then begin
        while (j < jmax) and (Image.Data[aa][j] = Image.Data[aa][j+1]) do
          inc(j);
        nvar := i - j;
        FS.Write(nvar, 1);
        FS.Write(Image.Data[aa][i], 2);
        cnt := cnt + j - i + 1;
      end else begin
        while (j < jmax) and (Image.Data[aa][j] <> Image.Data[aa][j+1]) do
          inc(j);
        nvar := j - i;
        FS.Write(nvar, 1);
        for bb := 0 to j - i do
          FS.Write(Image.Data[aa][i+bb], 2);
        cnt := cnt + j - i + 1;
      end;
    end;
  end;
  endoffs := FS.Position;
  nvar := endoffs - begoffs - 4;
  FS.Seek(begoffs, soFromBeginning);
  FS.Write(nvar, 4);
  FS.Seek(endoffs, soFromBeginning);
end;

procedure TSprite.cpackbit32(FS: TFileStream);
var
  aa, bb, cnt, i, j, jmax, nvar, begoffs, endoffs: Integer;
begin
  begoffs := FS.Position;
  nvar := 0;
  FS.Write(nvar, 4);
  for aa := 0 to Image.Height - 1 do
  begin
    cnt := 0;
    while cnt < Image.Width do
    begin
      i := cnt;
      j := i + 1;
      jmax := i + 126;
      if jmax >= Image.Width then
        jmax := Image.Width - 1;
      if i = Image.Width - 1 then
      begin
        nvar := 0;
        FS.Write(nvar, 1);
        FS.Write(Image.Data[aa][i], 4);
        inc(cnt);
      end else if Image.Data[aa][i] = Image.Data[aa][j] then begin
        while (j < jmax) and (Image.Data[aa][j] = Image.Data[aa][j+1]) do
          inc(j);
        nvar := i - j;
        FS.Write(nvar, 1);
        FS.Write(Image.Data[aa][i], 4);
        cnt := cnt + j - i + 1;
      end else begin
        while (j < jmax) and (Image.Data[aa][j] <> Image.Data[aa][j+1]) do
          inc(j);
        nvar := j - i;
        FS.Write(nvar, 1);
        for bb := 0 to j - i do
          FS.Write(Image.Data[aa][i+bb], 4);
        cnt := cnt + j - i + 1;
      end;
    end;
  end;
  endoffs := FS.Position;
  nvar := endoffs - begoffs - 4;
  FS.Seek(begoffs, soFromBeginning);
  FS.Write(nvar, 4);
  FS.Seek(endoffs, soFromBeginning);
end;

procedure TSprite.cunpackbitl(FS: TFileStream);
var
  aa: Integer;
  cx, ch, i, n: Int8;
begin
  for aa := 0 to Image.Height - 1 do
  begin
    n := 0;
    while n < Image.Width do
    begin
      FS.Read(cx, 1);
      if FS.Position = FS.Size then
        Break;
      if cx = -127 then
        cx := 0;
      if cx < 0 then
      begin
        i := 1 - cx;
        ch := 0;
        FS.Read(ch, 1);
        while i <> 0 do
        begin
          if n > Image.Width - 1 then
            Break;
          Image.Data[aa][n] := ch;
          inc(n);
          dec(i);
        end;
      end else begin
        i := cx + 1;
        while i <> 0 do
        begin
          if n > Image.Width - 1 then
            Break;
          FS.Read(Image.Data[aa][n], 1);
          inc(n);
          dec(i);
        end;
      end;
    end;
  end;
end;

procedure TSprite.cunpackbit16(FS: TFileStream);
var
  aa: Integer;
  i, n: Int16;
  cx: Int8;
  ch: uShort;
begin
  for aa := 0 to Image.Height - 1 do
  begin
    n := 0;
    while n < Image.Width do
    begin
      FS.Read(cx, 1);
      if FS.Position = FS.Size then
        Break;
      if cx = -127 then
        cx := 0;
      if cx < 0 then
      begin
        i := 1 - cx;
        ch := 0;
        FS.Read(ch, 2);
        while i <> 0 do
        begin
          if n > Image.Width - 1 then
            Break;
          Image.Data[aa][n] := ch;
          inc(n);
          dec(i);
        end;
      end else begin
        i := cx + 1;
        while i <> 0 do
        begin
          if n > Image.Width - 1 then
            Break;
          FS.Read(Image.Data[aa][n], 2);
          inc(n);
          dec(i);
        end;
      end;
    end;
  end;
end;

procedure TSprite.cunpackbit32(FS: TFileStream);
var
  aa: Integer;
  cx: Int8;
  ch, i, n: uInt;
begin
  for aa := 0 to Image.Height - 1 do
  begin
    n := 0;
    while n < Image.Width do
    begin
      FS.Read(cx, 1);
      if FS.Position = FS.Size then
        Break;
      if cx = -127 then
        cx := 0;
      if cx < 0 then
      begin
        i := 1 - cx;
        ch := 0;
        FS.Read(ch, 4);
        while i <> 0 do
        begin
          if n > Image.Width - 1 then
            Break;
          Image.Data[aa][n] := ch;
          inc(n);
          dec(i);
        end;
      end else begin
        i := cx + 1;
        while i <> 0 do
        begin
          if n > Image.Width - 1 then
            Break;
          FS.Read(Image.Data[aa][n], 4);
          inc(n);
          dec(i);
        end;
      end;
    end;
  end;
end;

procedure TSprite.GenerateDefaultPalette;
var
  aa: Integer;
begin
  GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 255);
  pal.palVersion := $300;
  pal.palNumEntries := 256;
  for aa := 0 to 255 do
    SetPaletteColor(aa, defpalette[aa*3] * 4, defpalette[aa*3+1] * 4, defpalette[aa*3+2] * 4);
end;

procedure TSprite.SetPaletteColor(idx: Integer; R,G,B: Int8);
begin
  pal.palPalEntry[idx].peRed := R;
  pal.palPalEntry[idx].peGreen := G;
  pal.palPalEntry[idx].peBlue := B;
  pal.palPalEntry[idx].peFlags := 0;
end;

end.
