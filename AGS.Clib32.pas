unit AGS.Clib32;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, System.SysUtils, System.Types;

const
  HeadSig: array [0..4] of AnsiChar   = ('C', 'L', 'I', 'B', #$1A);
  TailSig: array [0..11] of AnsiChar  = ('C','L','I','B',#1,#2,#3,#4,'S','I','G','E');

  SingleFilePswLen  = 13;

  MaxAssets         = 10000;
  MaxMultifiles     = 25;
  MaxAssetFileLen   = 100;
  MaxDataFileLen    = 50;
  V10LibFileLen     = 20;
  V10AssetFileLen   = 25;

  EncryptionString:   array [0..10] of AnsiChar = ('M','y',#1,#$de,#4,'J','i','b','z','l','e');
  EncryptionRandSeed  = 9338638;

  clib32copyright = 'CLIB32 v1.21 (c) 1995,1996,1998,2001,2007 Chris Jones';

type
  MFLError = (kMFLNoError = 0, kMFLErrNoLibSig = -1, kMFLErrLibVersion = -2, kMFLErrNoLibBase = -3, kMFLErrLibAssetCount = -4);

  AssetInfo = record
    FileName: AnsiString;
    LibUid:   Integer;
    Offset:   Integer;
    Size:     Integer;
  end;

  AssetLibInfo = record
    BaseFileName: AnsiString;
    LibFileNames: array of AnsiString;
    AssetInfos:   array of AssetInfo;
  end;

  MultiFileLib = record
    data_filenames: array [0..MAXMULTIFILES] of array [0..19] of AnsiChar;
    num_data_files: Integer;
    filenames: array [0..MaxAssets] of array [0..24] of AnsiChar;
    offset: array [0..MaxAssets] of uInt;
    length: array [0..MaxAssets] of uInt;
    file_datafile: array [0..MaxAssets] of Byte;
    num_files: Integer;
  end;

  MultiFileLibNew = record
    data_filenames: array [0..MAXMULTIFILES] of array [0..49] of AnsiChar;
    data_absoffs: array [0..MAXMULTIFILES] of uInt;
    num_data_files: Integer;
    filenames: array [0..MaxAssets] of array [0..99] of AnsiChar;
    offset: array [0..MaxAssets] of uInt;
    length: array [0..MaxAssets] of uInt;
    file_datafile: array [0..MaxAssets] of Byte;
    num_files: Integer;
  end;

  TClib32 = class(TObject)
    private
      function ReadByte(inp: TFileStream): Byte;
      function ReadInt8(inp: TFileStream): ShortInt;
      function ReadInt16(inp: TFileStream): SmallInt;
      function ReadInt32(inp: TFileStream): Integer;
      procedure ReadArray(data: Pointer; size, count: Integer; inp: TFileStream);
      function GetNextPseudoRand: Integer;
      procedure ReadEncArray(data: Pointer; size, count: Integer; inp: TFileStream);
      function ReadEncInt8(inp: TFileStream): ShortInt;
      function ReadEncInt32(inp: TFileStream): Integer;
      procedure ReadEncString(data: Pointer; size: Integer; inp: TFileStream);

      function ReadHeader(var lib: AssetLibInfo; inp: TFileStream): MFLError;
      function ReadSigsAndVersion(inp: TFileStream; var p_lib_version: Integer; var p_abs_offset: LongInt): MFLError;
      function ReadSingleFileLib(var lib: AssetLibInfo; inp: TFileStream; lib_version: Integer): MFLError;
      function ReadMultiFileLib(var lib: AssetLibInfo; inp: TFileStream; lib_version: Integer): MFLError;
      function ReadV10(var lib: AssetLibInfo; inp: TFileStream; lib_version: Integer): MFLError;
      function ReadV20(var lib: AssetLibInfo; inp: TFileStream): MFLError;
      function ReadV21(var lib: AssetLibInfo; inp: TFileStream): MFLError;
      procedure DecryptText(data: Pointer);

      function fgetc: AnsiChar;
      procedure fputc(Ch: AnsiChar);
      function getw: Integer;
      procedure putw(int: Integer);
      function strncmp(Str1, Str2: AnsiString; Count: Integer): Boolean;
      procedure init_pseudo_rand_gen(Seed: Integer);
      function get_pseudo_rand: Integer;
      procedure fgetnulltermstring(data: Pointer; dataSize: Integer);
      procedure fread_data_enc(data: Pointer; dataSize: Integer);
      procedure fwrite_data_enc(data: Pointer; dataSize: Integer);
      procedure fgetstring_enc(data: Pointer; dataSize: Integer);
      procedure fputstring_enc(data: Pointer; dataSize: Integer);
      function getw_enc: Integer;
      procedure putw_enc(int: Integer);
      procedure write_new_new_enc_format_clib;
      procedure strcpy(var arr: array of AnsiChar; str: AnsiString);
      procedure CopyStream(InS, OutS: TFileStream; Size: uInt);
    public
      constructor Create; overload;
      constructor Create(FileName: AnsiString); overload;
      constructor CreateOpened(FileName: AnsiString);
      procedure ChangeFile(FileName: AnsiString);
      destructor Free; overload;
      procedure Close;
      procedure WriteFiles;
      procedure WriteToFile(FileName: AnsiString);
      procedure WriteGameEngine(FileName: AnsiString);
      procedure AddFile(FileName: AnsiString);
      function OpenFile(FileName: AnsiString): Boolean;
      function GetFileStream: TFileStream;
      procedure ExtractFile(FileName: AnsiString; ToFile: AnsiString);
      procedure ExtractAllFiles(DirPath: AnsiString);
      procedure ExtractGameEngine(FileName: AnsiString);
      function GetNumFiles: Integer;
  end;

var
  assetLib: AssetLibInfo;
  abs_offset: LongInt;
  rand_val: Integer;

  ef: TFileStream;
  clbuff: array [0..19] of AnsiChar;
  lib_version: Byte;
  mflib: MultiFileLibNew;
  _last_rand: Integer;
  clibname: AnsiString;
  num_addfiles: Integer;
  file_paths: array [0..MaxAssets] of AnsiString;

implementation

uses
  Main;

function TCLib32.ReadByte(inp: TFileStream): Byte;
var
  res: Byte;
begin
  inp.ReadBuffer(res, SizeOf(Byte));
  Result := res;
end;

function TClib32.ReadInt8(inp: TFileStream): ShortInt;
var
  res: ShortInt;
begin
  inp.ReadBuffer(res, SizeOf(ShortInt));
  Result := res;
end;

function TClib32.ReadInt16(inp: TFileStream): SmallInt;
var
  res: SmallInt;
begin
  inp.ReadBuffer(res, SizeOf(SmallInt));
  Result := res;
end;

function TClib32.ReadInt32(inp: TFileStream): Integer;
var
  res: Integer;
begin
  inp.ReadBuffer(res, SizeOf(Integer));
  Result := res;
end;

procedure TClib32.ReadArray(data: Pointer; size, count: Integer; inp: TFileStream);
var
  new_size: Integer;
begin
  new_size := size * count;
  inp.ReadBuffer(data^, new_size);
end;

function TClib32.GetNextPseudoRand: Integer;
begin
  rand_val := rand_val * Long(214013) + Long(2531011);
  Result := (rand_val shr 16) and $7fff;
end;

procedure TClib32.ReadEncArray(data: Pointer; size, count: Integer; inp: TFileStream);
var
  len, i: UInt;
begin
  ReadArray(data, size, count, inp);
  len := size * count;
  for i := 0 to len - 1 do
    PByte(data)[i] := PByte(data)[i] - GetNextPseudoRand;
end;

function TClib32.ReadEncInt8(inp: TFileStream): ShortInt;
var
  res: ShortInt;
begin
  inp.ReadBuffer(res, SizeOf(ShortInt));
  Result := res - GetNextPseudoRand;
end;

function TClib32.ReadEncInt32(inp: TFileStream): Integer;
var
  val: Integer;
begin
  ReadEncArray(Pointer(@val), SizeOf(Integer), 1, inp);
  Result := val;
end;

procedure TClib32.ReadEncString(data: Pointer; size: Integer; inp: TFileStream);
var
  i: Integer;
begin
  for i := 0 to size - 1 do
  begin
    PByte(data)[i] := Byte(ReadByte(inp)) - GetNextPseudoRand;
    if PByte(data)[i] = 0 then break;
  end;
end;

function TClib32.fgetc: AnsiChar;
begin
  ef.Read(Result, 1);
end;

procedure TClib32.fputc(Ch: AnsiChar);
begin
  ef.Write(Ch, 1);
end;

function TClib32.getw: Integer;
begin
  ef.Read(Result, 4);
end;

procedure TClib32.putw(int: Integer);
begin
  ef.Write(int, 4);
end;

function TClib32.strncmp(Str1, Str2: AnsiString; Count: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Count do
    if Str1[i] <> Str2[i] then
    begin
      Result := False;
      Break;
    end;
end;

constructor TClib32.Create;
begin
  clibname := '';
  FillChar(mflib, SizeOf(mflib), #0);
end;

constructor TClib32.Create(FileName: AnsiString);
begin
  clibname := FileName;
  ef := TFileStream.Create(FileName, fmOpenRead);
  ReadHeader(assetLib, ef);
  ef.Free;
end;

constructor TClib32.CreateOpened(FileName: AnsiString);
begin
  clibname := FileName;
  ef := TFileStream.Create(FileName, fmOpenRead);
  ReadHeader(assetLib, ef);
end;

destructor TClib32.Free;
begin
//  ef.Free;
end;

procedure TClib32.Close;
begin
  ef.Free;
end;

procedure TClib32.ChangeFile(FileName: AnsiString);
begin

end;

function TClib32.ReadHeader(var lib: AssetLibInfo; inp: TFileStream): MFLError;
var
  lib_version, i: Integer;
  err: MFLError;
begin
  err := ReadSigsAndVersion(inp, lib_version, abs_offset);
  if err <> kMFLNoError then
  begin
    Result := err;
    Exit;
  end;

  if lib_version >= 10 then
  begin
    // read newer clib versions (versions 10+)
    err := ReadMultiFileLib(lib, inp, lib_version);
  end else
  begin
    err := ReadSingleFileLib(lib, inp, lib_version);
  end;
  // apply absolute offset for the assets contained in base data file
  // (since only base data file may be EXE file, other clib parts are always on their own)
  if abs_offset > 0 then
  begin
    for i := 0 to Length(lib.AssetInfos) do
      if lib.AssetInfos[i].LibUid = 0 then
        inc(lib.AssetInfos[i].Offset, abs_offset);
  end;
  Result := err;
end;

function TClib32.ReadSigsAndVersion(inp: TFileStream; var p_lib_version: Integer; var p_abs_offset: LongInt): MFLError;
var
  abs_offset: LongInt;
  lib_version: Byte;
  sig: array [0..12] of AnsiChar;
begin
  abs_offset  := 0;
  inp.ReadBuffer(sig, 5);
  if not strncmp(sig, HeadSig, 5) then
  begin
    inp.Seek(-Length(TailSig), soFromEnd);
    inp.ReadBuffer(sig, Length(TailSig));
    if not strncmp(sig, TailSig, Length(TailSig)) then
    begin
      Result := kMFLErrNoLibSig;
      Exit;
    end;
    inp.Seek(-Length(TailSig) - SizeOf(LongInt), soFromEnd);
    abs_offset := ReadInt32(inp);
    inp.Seek(abs_offset + Length(HeadSig), soFromBeginning);
  end;

  lib_version := ReadByte(inp);
  MainForm.WriteLog('Offset :' + IntToHex(inp.Position, 8));
  if (lib_version <> 6) and (lib_version <> 10) and (lib_version <> 11) and
    (lib_version <> 15) and (lib_version <> 20) and (lib_version <> 21) then
  begin
    Result := kMFLErrLibVersion; // unsupported version
    Exit;
  end;

  p_lib_version := lib_version;
  p_abs_offset := abs_offset;
  Result := kMFLNoError;
end;

function TClib32.ReadSingleFileLib(var lib: AssetLibInfo; inp: TFileStream; lib_version: Integer): MFLError;
var
  passwmodifier: Integer;
  asset_count: Word;
begin
  inp.ReadBuffer(passwmodifier, SizeOf(Byte));
  inp.Seek(8, soFromCurrent); // unused byte
  SetLength(lib.LibFileNames, 1); // only one library part
end;

function TClib32.ReadMultiFileLib(var lib: AssetLibInfo; inp: TFileStream; lib_version: Integer): MFLError;
begin
  if ReadByte(inp) <> 0 then
  begin
    Result := kMFLErrNoLibBase;  // not first datafile in chain
    Exit;
  end;
  if lib_version >= 21 then
  begin
    Result := ReadV21(lib, inp);
    Exit;
  end else if lib_version = 20 then
  begin
    Result := ReadV20(lib, inp);
    Exit;
  end;
  // read older clib format (versions 10 to 19), the ones with shorter filenames
  Result := ReadV10(lib, inp, lib_version);
end;

function TClib32.ReadV10(var lib: AssetLibInfo; inp: TFileStream; lib_version: Integer): MFLError;
var
  mf_count, asset_count, i: Integer;
  fn_buf: array [1..V10AssetFileLen] of AnsiChar;
begin
  mf_count := ReadInt32(inp);
  SetLength(lib.LibFileNames, mf_count);
  // filenames for all clib parts; filenames are only 20 chars long in this format version
  for i := 0 to mf_count - 1 do
  begin
    SetLength(lib.LibFileNames[i], V10LibFileLen);
    inp.ReadBuffer(PAnsiChar(lib.LibFileNames[i])^, V10LibFileLen);
  end;

  // number of files in clib
  asset_count := ReadInt32(inp);
  if asset_count > MaxAssets then
  begin
    Result := kMFLErrLibAssetCount; // too many files in clib, return error code
    Exit;
  end;
  // read information on clib contents
  SetLength(lib.AssetInfos, asset_count);
  for i := 0 to asset_count - 1 do
  begin
    inp.ReadBuffer(fn_buf, V10AssetFileLen);
    if lib_version >= 11 then
      DecryptText(@fn_buf);
    lib.AssetInfos[i].FileName := fn_buf;
  end;
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].Offset := ReadInt32(inp);
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].Size := ReadInt32(inp);
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].LibUid := ReadInt8(inp);
  Result := kMFLNoError;
end;

function TClib32.ReadV20(var lib: AssetLibInfo; inp: TFileStream): MFLError;
var
  mf_count, asset_count, i: Integer;
  len: Word;
  fn_buf: array [1..MaxAssetFileLen] of AnsiChar;
begin
  // number of clib parts
  mf_count := ReadInt32(inp);
  // filenames for all clib parts
  SetLength(lib.LibFileNames, mf_count);
  for i := 0 to mf_count - 1 do
  begin
    SetLength(lib.LibFileNames[i], MaxDataFileLen);
    inp.ReadBuffer(PAnsiChar(lib.LibFileNames[i])^, MaxDataFileLen);
  end;
  // number of files in clib
  asset_count := ReadInt32(inp);
  if asset_count > MaxAssets then
  begin
    Result := kMFLErrLibAssetCount; // too many files in clib, return error code
    Exit;
  end;
  // read information on clib contents
  SetLength(lib.AssetInfos, asset_count);
  for i := 0 to asset_count - 1 do
  begin
    len := ReadInt16(inp);
    len := len mod 5;
    inp.ReadBuffer(fn_buf, len);
    DecryptText(@fn_buf);
    lib.AssetInfos[i].FileName := fn_buf;
  end;
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].Offset := ReadInt32(inp);
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].Size := ReadInt32(inp);
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].LibUid := ReadInt8(inp);
  Result := kMFLNoError;
end;

function TClib32.ReadV21(var lib: AssetLibInfo; inp: TFileStream): MFLError;
var
  mf_count, asset_count, i: Integer;
  fn_buf: array [0..MaxDataFileLen] of AnsiChar;
begin
  rand_val := ReadInt32(inp) + EncryptionRandSeed;
  mf_count := ReadEncInt32(inp);
  SetLength(lib.LibFileNames, mf_count);
  for i := 0 to mf_count - 1 do
  begin
    ReadEncString(@fn_buf, MaxDataFileLen, inp);
    lib.LibFileNames[i] := fn_buf;
  end;
  asset_count := ReadEncInt32(inp);
  if asset_count > MaxAssets then
  begin
    Result := kMFLErrLibAssetCount;
    Exit;
  end;
  // read information on clib contents
  SetLength(lib.AssetInfos, asset_count);
  for i := 0 to asset_count - 1 do
  begin
    ReadEncString(@fn_buf, MaxAssetFileLen, inp);
    lib.AssetInfos[i].FileName := fn_buf;
  end;
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].Offset := ReadEncInt32(inp);
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].Size := ReadEncInt32(inp);
  for i := 0 to asset_count - 1 do
    lib.AssetInfos[i].LibUid := ReadEncInt8(inp);
  Result := kMFLNoError;
end;

procedure TCLib32.DecryptText(data: Pointer);
var
  adx, toenc: Integer;
begin
  adx := 0; toenc := 0;
  repeat
    PByte(data)[toenc] := PByte(data)[toenc] - Byte(EncryptionString[adx]);
    if PByte(data)[toenc] = 0 then
      break;
    inc(adx); inc(toenc);
    if adx > 10 then
      adx := 0;
  until false;
end;

procedure TClib32.WriteToFile(FileName: AnsiString);
begin
//  if FileExists(FileName) then
  clibname := ExtractFileName(FileName);
//  strcpy(mflib.data_filenames[0], clibname);
//  mflib.num_data_files := 1;
//  mflib.num_files := 0;
  num_addfiles := 0;
  ef := TFileStream.Create(FileName, fmCreate);
end;

procedure TClib32.init_pseudo_rand_gen(Seed: Integer);
begin
  _last_rand := Seed;
end;

function TClib32.get_pseudo_rand: Integer;
begin
  _last_rand := _last_rand * Long(214013) + Long(2531011);
  Result := (_last_rand shr 16) and $7fff;
end;

procedure TClib32.fgetnulltermstring(data: Pointer; dataSize: Integer);
var
  i: Integer;
begin
  for i := 0 to dataSize - 1 do
  begin
    PAnsiChar(data)[i] := fgetc;
    if PByte(data)[i] = 0 then
      Break;
  end;
end;

procedure TClib32.fread_data_enc(data: Pointer; dataSize: Integer);
var
  i: Integer;
begin
  ef.Read(data^, dataSize);
  for i := 0 to dataSize - 1 do
    PByte(data)[i] := PByte(data)[i] - get_pseudo_rand;
end;

procedure TClib32.fwrite_data_enc(data: Pointer; dataSize: Integer);
var
  i: Integer;
  buff: Byte;
begin
  for i := 0 to dataSize - 1 do
  begin
    buff := PByte(data)[i] + get_pseudo_rand;
    ef.Write(buff, 1);
  end;
end;

procedure TClib32.fgetstring_enc(data: Pointer; dataSize: Integer);
var
  i: Integer;
begin
  for i := 0 to dataSize - 1 do
  begin
    PByte(data)[i] := Byte(fgetc) - get_pseudo_rand;
    if PByte(data)[i] = 0 then break;
  end;
end;

procedure TClib32.fputstring_enc(data: Pointer; dataSize: Integer);
var
  i: Integer;
begin
  for i := 0 to dataSize - 1 do
  begin
    fputc(AnsiChar(PByte(data)[i] + get_pseudo_rand));
    if PByte(data)[i] = 0 then break;
  end;
end;

function TClib32.getw_enc: Integer;
var
  numberRead: Integer;
begin
  numberRead := 0;
  fread_data_enc(@numberRead, 4);
  Result := numberRead;
end;

procedure TClib32.putw_enc(int: Integer);
begin
  fwrite_data_enc(@int, 4);
end;

procedure TClib32.strcpy(var arr: array of AnsiChar; str: AnsiString);
var
  i: Integer;
begin
  for i := 1 to Length(str) do
  begin
    arr[i-1] := str[i];
    arr[i] := #0;
  end;
end;

procedure TClib32.write_new_new_enc_format_clib;
var
  aa, bb, last: Integer;
  foff: Boolean;
begin
//  randSeed := random(-1);
  randSeed := Trunc(Now);
  init_pseudo_rand_gen(randSeed);
  putw(randSeed - EncryptionRandSeed);
  putw_enc(mflib.num_data_files);
  for aa := 0 to mflib.num_data_files - 1 do
    fputstring_enc(@mflib.data_filenames[aa][0], 50);
  putw_enc(mflib.num_files);
  for aa := 0 to mflib.num_files - 1 do
    fputstring_enc(@mflib.filenames[aa][0], 100);
  // Подсчет отступлений
  foff := False;
  for bb := 0 to mflib.num_files - 1 do
  begin
    if (not foff) and (file_paths[bb] <> '') then
    begin
      mflib.offset[bb] := (ef.Position + 9 * mflib.num_files) - mflib.data_absoffs[mflib.file_datafile[bb]];
      last := bb;
      foff := True;
    end else if foff and (file_paths[bb] <> '') then
    begin
      mflib.offset[bb] := mflib.offset[last] + mflib.length[last];
      last := bb;
    end else
      mflib.offset[bb] := mflib.offset[bb] - mflib.data_absoffs[mflib.file_datafile[bb]];
  end;
  for aa := 0 to mflib.num_files - 1 do
    fwrite_data_enc(@mflib.offset[aa], 4);
  for aa := 0 to mflib.num_files - 1 do
    fwrite_data_enc(@mflib.length[aa], 4);
  for aa := 0 to mflib.num_files - 1 do
    fwrite_data_enc(@mflib.file_datafile[aa], 1);
end;



procedure TClib32.WriteFiles;
var
  i, aa: Integer;
  tmp: TFileStream;
begin
  ef.Write(HeadSig, 5);
  lib_version := 21;
  fputc(AnsiChar(lib_version));
  if lib_version >= 10 then
  begin
    fputc(#0);
    if lib_version >= 21 then begin
      write_new_new_enc_format_clib;
    end else if lib_version = 20 then begin
//      write_new_new_format_clib;
    end else begin

    end;
    for i := 0 to mflib.num_files - 1 do
    begin
      if file_paths[i] <> '' then
      begin
        tmp := TFileStream.Create(file_paths[i], fmOpenRead);
        CopyStream(tmp, ef, mflib.length[i]);
        tmp.Free;
      end;
    end;
    for aa := 0 to mflib.num_data_files - 1 do
      if mflib.data_filenames[aa] = clibname then
        Break;
    putw(mflib.data_absoffs[0]);
    ef.Write(TailSig, 12);
  end;
end;

procedure TClib32.CopyStream(InS, OutS: TFileStream; Size: uInt);
var
  buf: array of Byte;
  need: uInt;
const
  bufsize = 20 * 1024 * 1024;
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

function TClib32.OpenFile(FileName: AnsiString): Boolean;
var
  i: Integer;
  curlib: AnsiString;
begin
  Result := False;
  for i := 0 to Length(assetLib.AssetInfos) - 1 do
  begin
    if strncmp(LowerCase(assetLib.AssetInfos[i].FileName), LowerCase(FileName), Length(FileName)) then
    begin
      Result := True;
      if assetLib.AssetInfos[i].LibUid = 0 then
        curlib := clibname
      else
        curlib := ExtractFilePath(clibname) + assetLib.LibFileNames[assetLib.AssetInfos[i].LibUid];
      ef := TFileStream.Create(curlib, fmOpenRead);
      ef.Seek(assetLib.AssetInfos[i].Offset, soFromBeginning);
      Break;
    end;
  end;
end;

function TClib32.GetFileStream: TFileStream;
begin
  Result := ef;
end;

procedure TClib32.ExtractFile(FileName: AnsiString; ToFile: AnsiString);
var
  i: Integer;
  found: Boolean;
  nf: TFileStream;
  curlib: AnsiString;
begin
  found := False;
  for i := 0 to Length(assetLib.AssetInfos) - 1 do
  begin
    if strncmp(LowerCase(assetLib.AssetInfos[i].FileName), LowerCase(FileName), Length(FileName)) then
    begin
      Found := True;
      Break;
    end;
  end;
  if Found then
  begin
    if assetLib.AssetInfos[i].LibUid <> 0 then
      curlib := ExtractFilePath(clibname) + assetLib.LibFileNames[assetLib.AssetInfos[i].LibUid]
    else
      curlib := clibname;
    ef := TFileStream.Create(curlib, fmOpenRead);
    nf := TFileStream.Create(ToFile, fmCreate);
    ef.Seek(mflib.offset[i], soFromBeginning);
    nf.CopyFrom(ef, assetLib.AssetInfos[i].Size);
//    CopyStream(ef, nf, assetLib.AssetInfos[i].Size);
    nf.Free;
    ef.Free;
  end;
end;

procedure TClib32.ExtractAllFiles(DirPath: AnsiString);
var
  i: Integer;
  curlib: AnsiString;
  nf: TFileStream;
begin
  for i := 0 to Length(assetLib.AssetInfos) - 1 do
  begin
    ForceDirectories(DirPath);
    if assetLib.AssetInfos[i].LibUid <> 0 then
      curlib := ExtractFilePath(clibname) + assetLib.LibFileNames[assetLib.AssetInfos[i].LibUid]
    else
      curlib := clibname;
    ef := TFileStream.Create(curlib, fmOpenRead);
    nf := TFileStream.Create(DirPath + assetLib.AssetInfos[i].FileName, fmCreate);
    ef.Seek(assetLib.AssetInfos[i].Offset, soFromBeginning);
    CopyStream(ef, nf, assetLib.AssetInfos[i].Size);
    nf.Free;
    ef.Free;
  end;
end;

procedure TClib32.ExtractGameEngine(FileName: AnsiString);
var
  ge: TFileStream;
begin
  ef := TFileStream.Create(clibname, fmOpenRead);
  ge := TFileStream.Create(FileName, fmCreate);
  ef.Seek(0, soFromBeginning);
  CopyStream(ef, ge, abs_offset);
  ge.Free;
  ef.Free;
end;

procedure TClib32.AddFile(FileName: AnsiString);
var
  Found: Boolean;
  i, curf, curd: Integer;
  tmp: TFileStream;
begin
  // Check if new library
  Found := False;
  for i := 0 to mflib.num_data_files - 1 do
    if mflib.data_filenames[i] = LowerCase(clibname) then
    begin
      Found := True;
      Break;
    end;
  if Found then
  begin
    curd := i;
  end else begin
    inc(mflib.num_data_files);
    curd := mflib.num_data_files - 1;
//    SetLength(mflib.data_filenames, mflib.num_data_files);
    strcpy(mflib.data_filenames[curd], LowerCase(clibname));
  end;

//  strcpy(mflib.data_filenames[0], clibname);

  Found := False;
  for i := 0 to mflib.num_files - 1 do
    if mflib.filenames[i] = ExtractFileName(FileName) then
    begin
      Found := True;
      Break;
    end;
  if Found then
  begin
    curf := i;
  end else begin
    inc(mflib.num_files);
    curf := mflib.num_files - 1;
  end;
  mflib.offset[curf] := 0;
  tmp := TFileStream.Create(FileName, fmOpenRead);
  mflib.length[curf] := tmp.Size;
  tmp.Free;
  mflib.file_datafile[curf] := curd;
  strcpy(mflib.filenames[curf], LowerCase(ExtractFileName(FileName)));
//  SetLength(file_paths, mflib.num_files);
  file_paths[curf] := FileName;
  inc(num_addfiles);
end;

procedure TClib32.WriteGameEngine(FileName: AnsiString);
var
  ge: TFileStream;
  aa, curd: Integer;
begin
  ge := TFileStream.Create(FileName, fmOpenRead);
  ef.Seek(0, soFromBeginning);
  ge.Seek(0, soFromBeginning);
  CopyStream(ge, ef, ge.Size);
  aa := 0;
  for aa := 0 to mflib.num_data_files - 1 do
    if mflib.data_filenames[aa] = clibname then
      Break;
  mflib.data_absoffs[aa] := ef.Position;
  ge.Free;
end;

function TClib32.GetNumFiles: Integer;
begin
  Result := mflib.num_files;
end;

end.
