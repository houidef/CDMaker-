unit mbpCommonLib;

interface

uses mbpDeviceTypes;

{$I mbpDEFINES.INC}

function msf2lba(h, m, s, f: Integer): Integer;
function cew(w: Integer): Integer;
function cedw(dw: cardinal): cardinal;
function BitTest(Bits,Bit : integer) : byte; register; assembler;
function BitTestAndSet(Bits,Bit : integer) : byte; register; assembler;
function BitTestAndReset(Bits,Bit : integer) : byte; register; assembler;
function GetBitsValue(Bits, Pos, Len: Byte): Byte;
procedure CvtEndians4(src: PAnsiChar; dest: PAnsiChar);
procedure CvtEndians2(src: PAnsiChar; dest: PAnsiChar);
function Sectors(bytes: int64): LongWord;
function Sectors2352(bytes: int64): LongWord;

function IfThen(Expr: Boolean; const IfTrue: Integer; const IfFalse: Integer): Integer;

implementation

function IfThen(Expr: Boolean; const IfTrue: Integer; const IfFalse: Integer): Integer;
begin
  if Expr then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function msf2lba(h, m, s, f: Integer): Integer;
begin
  result := ((h * 60 * 60 * 75) + (m * 60 * 75)+(s * 75) + (f));
end;

// Byte Order conversion functions
function cew(w: Integer): Integer;
begin
  result := ((w shl 8) and $ff00) or ((w shr 8) and $00ff);
end;

function cedw(dw: cardinal): cardinal;
begin
  result := ((dw shl 24) and $ff000000) or ((dw shl  8) and $00ff0000) or ((dw shr  8) and $0000ff00) or ((dw shr 24) and $000000ff);
end;

procedure CvtEndians4(src: PAnsiChar; dest: PAnsiChar);
var
  t1, t3: AnsiChar;
begin
  t1 := src[1];
  t3 := src[3];
  dest[1] := src[2];
  dest[3] := src[0];
  dest[0] := t3;
  dest[2] := t1;  
end;

procedure CvtEndians2(src: PAnsiChar; dest: PAnsiChar);
var
  s1: AnsiChar;
begin
  s1 := src[0];
  dest[0] := src[1];
  dest[1] := s1;
end;

// Bit Manipulation Functions

function BitTest(Bits,Bit : integer) : byte; register; assembler;
asm
  bt eax,edx
  setc al
end;

function BitTestAndSet(Bits,Bit : integer) : byte; register; assembler;
asm
  bts eax,edx
  setc al
end;

function BitTestAndReset(Bits,Bit : integer) : byte; register; assembler;
asm
  btr eax,edx
  setc al
end;

function GetBitsValue(Bits, Pos, Len: Byte): Byte;
var
  b: byte;
  i: integer;
begin
  b := 0;
  Write(Bits,':');
  for i:=7 downto 0 do
    write(BitTest(Bits, i));
  //Writeln;
  //Write(Bits,':');
  for i:=Pos+Len-1 downto Pos do
  begin
    b := b shl 1;
    b := b + BitTest(Bits, i);
    write(BitTest(Bits, i));
  end;
  result := b;
  //Writeln;
  //Writeln('    76543210');
end;
(*******************************************************************************

*******************************************************************************)
function Sectors(bytes: int64): LongWord;
begin
  if (bytes = 0) then
  begin
    Result := 0;
    exit;
  end
  else
  begin
    if (bytes mod 2048 = 0) then
      Result := Trunc(bytes / 2048)
    else
      Result := Succ(Trunc(bytes / 2048));
  end;
end;

function Sectors2352(bytes: int64): LongWord;
begin
  if (bytes = 0) then
  begin
    Result := 0;
    exit;
  end
  else
  begin
    if (bytes mod 2352 = 0) then
      Result := Trunc(bytes / 2352)
    else
      Result := Succ(Trunc(bytes / 2352));
  end;
end;

end.
