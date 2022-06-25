{$I mbpDEFINES.INC}
//---------------------------------------------------------------------------

unit mbpUDFUnicode;
//---------------------------------------------------------------------------

interface

uses
  mbpUDFTypes;

function CompressUnicode(numberOfChars, compID: Integer; unicode: Punicode_t; UDFCompressed: PAnsiChar): Integer;
function UncompressUnicode(numberOfBytes: Integer; UDFCompressed: PAnsiChar; unicode: Punicode_t): Integer;
function OSTACompressedUnicodeStrings_Matched(String1: PAnsiChar; String1Length: Integer; String2: PAnsiChar; String2Length: Integer): Boolean;
function UnicodeStringMatchedPattern(SourcePattern, TargetString: PWideChar): Boolean;
//---------------------------------------------------------------------------

implementation

uses
  SysUtils,
  mbpSysUtilsW, mbpUDFUtils;

{
  DESCRIPTION:
  Takes a AnsiString of unicode wide characters and returns an OSTA CS0
  compressed unicode AnsiString. The unicode MUST be in the byte order of
  the compiler in order to obtain correct results. Returns an error
  if the compression ID is invalid.

  NOTE:
  This routine assumes the implementation already knows, by
  the local environment, how many bits are appropriate and therefore does
  no checking to test if the input characters fit into that number of
  bits or not.

  RETURN VALUE:
  The total number of bytes in the compressed OSTA CS0 AnsiString,
  including the compression ID.
  A -1 is returned if the compression ID is invalid.
}
function CompressUnicode
(
numberOfChars: Integer; // (Input) number of unicode characters.
compID: Integer;        // (Input) compression ID to be used.
unicode: Punicode_t;    // (Input) unicode characters to compress.
UDFCompressed: PAnsiChar    // (Output) compressed AnsiString, as bytes.
): Integer;
var
  byteIndex, unicodeIndex: Integer;
begin
  if ((compID <> 8) and (compID <> 16)) then
  begin
    byteIndex := -1; // Unsupported compression ID!
  end
  else
  begin
    // Place compression code in first byte.
    UDFCompressed[0] := AnsiChar(compID);

    byteIndex := 1;
    unicodeIndex := 0;
    while (unicodeIndex < numberOfChars) do
    begin
      if (compID = 16) then
      begin
        // First, place the high bits of the character into the byte stream.
        UDFCompressed[byteIndex] := AnsiChar((Integer(PWideChar(unicode)[unicodeIndex]) and $FF00) shr 8);
        Inc(byteIndex);
      end;
      // Then place the low bits into the stream.
      UDFCompressed[byteIndex] := AnsiChar(Integer(PWideChar(unicode)[unicodeIndex]) and $00FF);
      Inc(byteIndex);
      Inc(unicodeIndex);
    end;
  end;

  Result := byteIndex;
end;
//---------------------------------------------------------------------------

function OSTACompressedUnicodeStrings_Matched(String1: PAnsiChar; String1Length: Integer; String2: PAnsiChar; String2Length: Integer): Boolean;
begin
  if (String1Length <> String2Length) then
  begin
    Result := False;
    Exit;
  end;

  Result := CompareMem(String1, String2, String1Length);
end;
//---------------------------------------------------------------------------

{
  DESCRIPTION:
  Takes an OSTA CS0 compressed unicode name, and converts it to Unicode.
  The Unicode output will be in the byte order that the local compiler uses
  for 16-bit values.

  NOTE:
  This routine only performs error checking on the compID.
  It is up to the user to ensure that the unicode buffer is large enough,
  and that the compressed unicode name is correct.

  RETURN VALUE:
  The number of unicode characters which were uncompressed.
  A -1 is returned if the compression ID is invalid.
}
function UncompressUnicode
(
numberOfBytes: Integer; // (Input) number of bytes read from media.
UDFCompressed: PAnsiChar;   // (Input) bytes read from media.
unicode: Punicode_t     // (Output) uncompressed unicode characters.
): Integer;
var
  compID: Cardinal;
  returnValue, unicodeIndex, byteIndex: Integer;
begin
  // Use UDFCompressed to store current byte being read.
  compID := Ord(UDFCompressed[0]);

  // First check for valid compID.
  if ((compID <> 8) and (compID <> 16)) then
  begin
    returnValue := -1;
  end
  else
  begin
    unicodeIndex := 0;
    byteIndex := 1;

    // Loop through all the bytes.
    while (byteIndex < numberOfBytes) do
    begin
      if (compID = 16) then
      begin
        // Move the first byte to the high bits of the unicode AnsiChar.
        PWideChar(unicode)[unicodeIndex] := WideChar(Integer(UDFCompressed[byteIndex]) shl 8);
        Inc(byteIndex);
      end
      else
      begin
        PWideChar(unicode)[unicodeIndex] := #0;
      end;

      if (byteIndex < numberOfBytes) then
      begin
        // Then the next byte to the low bits.
        PWideChar(unicode)[unicodeIndex] := WideChar(Integer(PWideChar(unicode)[unicodeIndex]) or Integer(UDFCompressed[byteIndex]));
        Inc(byteIndex);
      end;

      Inc(unicodeIndex);
    end;

    returnValue := unicodeIndex;
  end;

  Result := returnValue;
end;

function UnicodeStringMatchedPattern(SourcePattern, TargetString: PWideChar): Boolean;
var
  FirstAsteriskIndex, FirstQuestionMarkIndex: PWideChar;
begin
  FirstAsteriskIndex := StrScanW(SourcePattern, '*');
  FirstQuestionMarkIndex := StrScanW(SourcePattern, '?');

  if ((FirstAsteriskIndex <> nil) or (FirstQuestionMarkIndex <> nil)) then
  begin
    if (StrCompW(SourcePattern, '*') = 0) then
    begin
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  end
  else // no wildcard found in source pattern
  begin
    Result := (StrCompW(SourcePattern, TargetString) = 0);
  end;
end;

end.
