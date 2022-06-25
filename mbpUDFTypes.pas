{$I mbpDEFINES.INC}
//---------------------------------------------------------------------------

unit mbpUDFTypes;
//---------------------------------------------------------------------------

interface

type
  Pint8 = ^int8;
  int8 = Shortint;

  Pint16 = ^int16;
  int16 = Smallint;

  Pint32 = ^int32;
  int32 = Integer;

  PUint8 = ^Uint8;
  Uint8 = Byte;

  PUint16 = ^Uint16;
  Uint16 = Word;

  PUint32 = ^Uint32;
  Uint32 = Cardinal;

  PUint64 = ^Uint64;
  Uint64 = Int64;

  PUshort = ^Ushort;
  Ushort = WORD;

  Punicode_t = ^unicode_t;
  unicode_t = WideChar;

  PUchar = ^Uchar;
  Uchar = AnsiChar;

  dstring = AnsiChar;
//---------------------------------------------------------------------------

implementation

end.
