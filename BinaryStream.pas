unit BinaryStream;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Rtti,
  System.Generics.Collections;

type
  TBinaryStream = class helper for TStream
    private
      function Swap32(Value: Cardinal): Cardinal;
    public
      // Помощь при чтении файла
      function ReadBool: Boolean;
      function ReadBool4: Boolean;
      function ReadByte: Byte; overload;
      function ReadByte(Offset: Integer): Byte; overload;
      function ReadInt8: ShortInt;
      function Read7BitEncodedInt: Integer;
      function ReadInt16: SmallInt;
      function ReadUInt16: Word;
      function ReadInt24: Integer;
      function ReadUInt24: Cardinal;
      function ReadInt32: Integer;
      function ReadUInt32: Cardinal; overload;
      function ReadUInt32(Offset: Integer): Cardinal; overload;
      function ReadInt64: Int64;
      function ReadUInt64: UInt64;
      function ReadFloat: Single;
      function ReadDouble: Extended;
      function ReadGUID: TGUID;
      function ReadAnsiString: string; overload;
      function ReadAnsiString(Size: Cardinal): string; overload;
      function ReadCP866String: string; overload;
      function ReadCP866String(Size: Cardinal): string; overload;
      function ReadSJISString: string; overload;
      function ReadSJISString(Size: Cardinal): string; overload;
      function ReadUTF8String: string; overload;
      function ReadUTF8String(Size: Cardinal): string; overload;
      function ReadUnicodeString: string; overload;
      function ReadUnicodeString(Size: Cardinal): string; overload;
      function ReadUTF8Char: TArray<Byte>;
      // Помощь при записи файла
      procedure WriteBool(Value: Boolean);
      procedure WriteBool4(Value: Boolean);
      procedure WriteByte(Value: Byte);
      procedure Write7BitEncodedInt(Value: Integer);
      procedure WriteInt16(Value: SmallInt);
      procedure WriteUInt16(Value: Word);
      procedure WriteInt24(Value: Integer);
      procedure WriteUInt24(Value: Cardinal);
      procedure WriteInt32(Value: Integer);
      procedure WriteUInt32(Value: Cardinal);
      procedure WriteInt64(Value: Int64);
      procedure WriteUInt64(Value: UInt64);
      procedure WriteFloat(Value: Single);
      procedure WriteDouble(Value: Extended);
      procedure WriteGUID(Value: TGUID);
      procedure WriteAnsiString(Value: string); overload;
      procedure WriteAnsiString(Value: string; Count: Cardinal); overload;
      procedure WriteCP866String(Value: string);
      procedure WriteUnicodeString(Value: string); overload;
      procedure WriteUnicodeString(Value: string; Count: Cardinal); overload;
      procedure WriteUTF8String(Value: string); overload;
      procedure WriteUTF8String(Value: string; Count: Integer); overload;
      procedure WriteSJISString(Value: string);
      // прочее
      procedure Skip(Value: Integer);
      // Стримы
      procedure MemoryStreamCreate(var AStream: TMemoryStream; Value: Pointer; Count: Int64);
      procedure MemoryStreamCreateOff(var AStream: TMemoryStream; Value: Pointer; Offset: Int64; Count: Int64);
      procedure MemoryStreamGet(AStream: TMemoryStream; Value: Pointer; Count: Int64);
  end;

var
  BigEndian: Boolean;

implementation

function TBinaryStream.Swap32(Value: Cardinal): Cardinal;
begin
  Result := Swap(Value shr 16) or (Swap(Value) shl 16);
end;

function TBinaryStream.ReadBool: Boolean;
begin
  Result := Boolean(ReadByte);
end;

function TBinaryStream.ReadBool4: Boolean;
begin
  Result := Boolean(ReadUInt32);
end;

function TBinaryStream.ReadByte: Byte;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryStream.ReadByte(Offset: Integer): Byte;
var
  curOff: UInt64;
begin
  curOff := Position;
  Position := Offset;
  ReadBuffer(Result, SizeOf(Result));
  Position := curOff;
end;

function TBinaryStream.ReadInt8: ShortInt;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryStream.Read7BitEncodedInt: Integer;
var
  bitsRead, value: Cardinal;
begin
  Result := 0;
  bitsRead := 0;
  Value := $80;

  while (value and $80) <> 0 do begin
    value := ReadByte;
    Result := Cardinal(Result) or (value and $7f) shl bitsRead;
    Inc(bitsRead, 7);
  end;
end;

function TBinaryStream.ReadInt16: SmallInt;
begin
  ReadBuffer(Result, SizeOf(Result));
  if BigEndian then Result := Swap(Result);
end;

function TBinaryStream.ReadUInt16: Word;
begin
  ReadBuffer(Result, SizeOf(Result));
  if BigEndian then Result := Swap(Result);
end;

function TBinaryStream.ReadInt24: Integer;
begin
  Result := 0;
  ReadBuffer(Result, 3);
  if BigEndian then Result := Swap32(Result);
end;

function TBinaryStream.ReadUInt24: Cardinal;
begin
  Result := 0;
  ReadBuffer(Result, 3);
  if BigEndian then Result := Swap32(Result);
end;

function TBinaryStream.ReadInt32: Integer;
begin
  ReadBuffer(Result, SizeOf(Result));
  if BigEndian then Result := Swap32(Result);
end;

function TBinaryStream.ReadUInt32: Cardinal;
begin
  ReadBuffer(Result, SizeOf(Result));
  if BigEndian then Result := Swap32(Result);
end;

function TBinaryStream.ReadUInt32(Offset: Integer): Cardinal;
var
  curOff: UInt64;
begin
  curOff := Position;
  Position := Offset;
  ReadBuffer(Result, SizeOf(Result));
  Position := curOff;
  if BigEndian then Result := Swap32(Result);
end;

function TBinaryStream.ReadInt64: Int64;
begin
  ReadBuffer(Result, SizeOf(Result));
  if BigEndian then Result := Swap(Result);
end;

function TBinaryStream.ReadUInt64: UInt64;
begin
  ReadBuffer(Result, SizeOf(Result));
  if BigEndian then Result := Swap(Result);
end;

function TBinaryStream.ReadFloat: Single;
begin
  ReadBuffer(Result, SizeOf(Result));
//  if bBigEndian then Result := Swap(Result);
end;

function TBinaryStream.ReadDouble: Extended;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
//  if bBigEndian then Result := Swap(Result);
end;

function TBinaryStream.ReadGUID: TGUID;
begin
  ReadBuffer(Result, SizeOf(Result));
//  if bBigEndian then Result := Swap(Result);
end;

function TBinaryStream.ReadAnsiString: string;
var
  Buf: TList<Byte>;
  bByte: Byte;
begin
  Result := '';
  Buf := TList<Byte>.Create;
  while true do
  begin
    if Position >= Size then Break;
    ReadBuffer(bByte, 1);
    if bByte = 0 then Break;
    Buf.Add(bByte);
  end;
  Result := TEncoding.ANSI.GetString(Buf.ToArray);
  Buf.Free;
end;

function TBinaryStream.ReadAnsiString(Size: Cardinal): string;
var
  Buf: TArray<Byte>;
begin
  SetLength(Buf, Size);
  ReadBuffer(Buf[0], Size);
  Result := TEncoding.ANSI.GetString(Buf);
  SetLength(Buf, 0);
end;

function TBinaryStream.ReadCP866String: string;
var
  Buf: TList<Byte>;
  bByte: Byte;
begin
  Result := '';
  Buf := TList<Byte>.Create;
  while true do
  begin
    if Position >= Size then Break;
    ReadBuffer(bByte, 1);
    if bByte = 0 then Break;
    Buf.Add(bByte);
  end;
  Result := TEncoding.GetEncoding(866).GetString(Buf.ToArray);
  Buf.Free;
end;

function TBinaryStream.ReadCP866String(Size: Cardinal): string;
var
  Buf: TArray<Byte>;
begin
  SetLength(Buf, Size);
  ReadBuffer(Buf[0], Size);
  Result := TEncoding.GetEncoding(866).GetString(Buf);
  SetLength(Buf, 0);
end;

function TBinaryStream.ReadSJISString: string;
var
  Buf: TList<Byte>;
  bByte: Byte;
begin
  Result := '';
  Buf := TList<Byte>.Create;
  while true do
  begin
    if Position >= Size then Break;
    ReadBuffer(bByte, 1);
    if bByte = 0 then Break;
    Buf.Add(bByte);
  end;
  Result := TEncoding.GetEncoding(932).GetString(Buf.ToArray);
  Buf.Free;
end;

function TBinaryStream.ReadSJISString(Size: Cardinal): string;
var
  Buf: TArray<Byte>;
begin
  SetLength(Buf, Size);
  ReadBuffer(Buf[0], Size);
  Result := TEncoding.GetEncoding(932).GetString(Buf);
  SetLength(Buf, 0);
end;

function TBinaryStream.ReadUTF8String: string;
var
  Buf: TList<Byte>;
  bByte: Byte;
begin
  Result := '';
  Buf := TList<Byte>.Create;
  while true do
  begin
    if Position >= Size then Break;
    ReadBuffer(bByte, 1);
    if bByte = 0 then Break;
    Buf.Add(bByte);
  end;
  Result := TEncoding.UTF8.GetString(Buf.ToArray);
  Buf.Free;
end;

function TBinaryStream.ReadUTF8String(Size: Cardinal): string;
var
  Buf: TArray<Byte>;
begin
  SetLength(Buf, Size);
  ReadBuffer(Buf[0], Size);
  Result := TEncoding.UTF8.GetString(Buf);
  SetLength(Buf, 0);
end;

function TBinaryStream.ReadUnicodeString: string;
var
  Buf: TList<Byte>;
  bByte, bByte2: Byte;
begin
  Result := '';
  Buf := TList<Byte>.Create;
  while true do
  begin
    if Position + 1 >= Size then Break;
    ReadBuffer(bByte, 1);
    ReadBuffer(bByte2, 1);
    if (bByte = 0) and (bByte2 = 0) then Break;
    Buf.Add(bByte);
    Buf.Add(bByte2);
  end;
  Result := TEncoding.Unicode.GetString(Buf.ToArray);
  Buf.Free;
end;

function TBinaryStream.ReadUnicodeString(Size: Cardinal): string;
var
  Buf: TArray<Byte>;
begin
  SetLength(Buf, Size * 2);
  ReadBuffer(Buf[0], Size * 2);
  Result := TEncoding.Unicode.GetString(Buf);
  SetLength(Buf, 0);
end;

function TBinaryStream.ReadUTF8Char: TArray<Byte>;
var
  Val, Num: Byte;
begin
  Num := 0;
  Val := ReadByte;
  if Val < $C0 then Num := 1
  else if (Val >= $C0) and (Val <= $DF) then Num := 2
  else if (Val >= $E0) and (Val <= $EF) then Num := 3
  else if (Val >= $F0) and (Val <= $F7) then Num := 4;

  SetLength(Result, Num);
  Result[0] := Val;
  if Num > 1 then
    ReadBuffer(Result[1], Num - 1);
end;

procedure TBinaryStream.WriteBool(Value: Boolean);
begin
  WriteByte(Byte(Value))
end;

procedure TBinaryStream.WriteBool4(Value: Boolean);
begin
  WriteInt32(Integer(Value));
end;

procedure TBinaryStream.WriteByte(Value: Byte);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TBinaryStream.Write7BitEncodedInt(Value: Integer);
var
  v: LongWord;
begin
  v := Value;
  while v >= $80 do begin
    WriteByte(v or $80);
    v := v shr 7;
  end;
  WriteByte(v);
end;

procedure TBinaryStream.WriteInt16(Value: SmallInt);
var
  NewVal: SmallInt;
begin
  if BigEndian then NewVal := Swap(Value)
  else NewVal := Value;
  WriteBuffer(NewVal, SizeOf(NewVal));
end;

procedure TBinaryStream.WriteUInt16(Value: Word);
var
  NewVal: Word;
begin
  if BigEndian then NewVal := Swap(Value)
  else NewVal := Value;
  WriteBuffer(NewVal, SizeOf(NewVal));
end;

procedure TBinaryStream.WriteInt24(Value: Integer);
begin
  WriteBuffer(Value, 3);
end;

procedure TBinaryStream.WriteUInt24(Value: Cardinal);
begin
  WriteBuffer(Value, 3);
end;

procedure TBinaryStream.WriteInt32(Value: Integer);
var
  NewVal: Integer;
begin
  if BigEndian then NewVal := Swap32(Value)
  else NewVal := Value;
  WriteBuffer(NewVal, SizeOf(NewVal));
end;

procedure TBinaryStream.WriteUInt32(Value: Cardinal);
var
  NewVal: Cardinal;
begin
  if BigEndian then NewVal := Swap32(Value)
  else NewVal := Value;
  WriteBuffer(NewVal, SizeOf(NewVal));
end;

procedure TBinaryStream.WriteInt64(Value: Int64);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TBinaryStream.WriteUInt64(Value: UInt64);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TBinaryStream.WriteFloat(Value: Single);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TBinaryStream.WriteDouble(Value: Extended);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TBinaryStream.WriteGUID(Value: TGUID);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TBinaryStream.WriteAnsiString(Value: string);
var
  arrByte: TArray<Byte>;
begin
  arrByte := TEncoding.ANSI.GetBytes(Value);
  WriteBuffer(arrByte, Length(arrByte));
  WriteByte(0);
end;

procedure TBinaryStream.WriteAnsiString(Value: string; Count: Cardinal);
var
  arrByte: TArray<Byte>;
begin
  arrByte := TEncoding.ANSI.GetBytes(Copy(Value, 1, Count));
  WriteBuffer(arrByte, Length(arrByte));
end;

procedure TBinaryStream.WriteCP866String(Value: string);
var
  arrByte: TArray<Byte>;
begin
  arrByte := TEncoding.GetEncoding(866).GetBytes(Value);
  WriteBuffer(arrByte, Length(arrByte));
  WriteByte(0);
end;

procedure TBinaryStream.WriteUnicodeString(Value: string);
var
  arrByte: TArray<Byte>;
begin
  arrByte := TEncoding.Unicode.GetBytes(Value);
  WriteBuffer(arrByte[0], Length(arrByte));
  WriteUInt16(0);
end;

procedure TBinaryStream.WriteUnicodeString(Value: string; Count: Cardinal);
var
  arrByte: TArray<Byte>;
begin
  arrByte := TEncoding.Unicode.GetBytes(Copy(Value, 1, Count));
  WriteBuffer(arrByte[0], Length(arrByte));
end;

procedure TBinaryStream.WriteUTF8String(Value: string);
var
  arrByte: TArray<Byte>;
begin
  arrByte := TEncoding.UTF8.GetBytes(Value);
  WriteBuffer(arrByte, Length(arrByte));
  WriteByte(0);
end;

procedure TBinaryStream.WriteUTF8String(Value: string; Count: Integer);
var
  arrByte: TArray<Byte>;
begin
  arrByte := TEncoding.UTF8.GetBytes(Copy(Value, 1, Count));
  WriteBuffer(arrByte, Length(arrByte));
end;

procedure TBinaryStream.WriteSJISString(Value: string);
var
  arrByte: TArray<Byte>;
begin
  arrByte := TEncoding.GetEncoding(932).GetBytes(Value);
  WriteBuffer(arrByte[0], Length(arrByte));
  WriteByte(0);
end;

// =============================================================================

procedure TBinaryStream.Skip(Value: Integer);
begin
  Seek(Value, soFromCurrent);
end;

procedure TBinaryStream.MemoryStreamCreate(var AStream: TMemoryStream; Value: Pointer; Count: Int64);
begin
  AStream := TMemoryStream.Create;
  AStream.WriteBuffer(Value, Count);
  AStream.Seek(0, soFromBeginning);
end;

procedure TBinaryStream.MemoryStreamCreateOff(var AStream: TMemoryStream; Value: Pointer; Offset: Int64; Count: Int64);
begin
  AStream := TMemoryStream.Create;
  AStream.WriteBuffer(PAnsiString(Value)^[Offset], Count);
  AStream.Seek(0, soFromBeginning);
end;

procedure TBinaryStream.MemoryStreamGet(AStream: TMemoryStream; Value: Pointer; Count: Int64);
begin
  AStream.Seek(0, soFromBeginning);
  AStream.ReadBuffer(Value^, Count);
  AStream.Free;
end;

end.
