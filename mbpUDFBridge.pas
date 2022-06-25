{$I mbpDEFINES.INC}
//---------------------------------------------------------------------------

unit mbpUDFBridge;
//---------------------------------------------------------------------------

interface

uses
  mbpTreeTypes, mbpUDFTypes;

const
  BlockSize = 2048;
  BlocksInFixedPacket = 32;
  FixedPacketSize = (BlocksInFixedPacket * BlockSize); // 64 KB
//---------------------------------------------------------------------------

function PrepareUDFBridge_1(var CDFiles: TCDFiles; VolumeRecognitionSequence_LSAWRTSession: Uint32): Boolean;
function PrepareUDFBridge_2(var CDFiles: TCDFiles; NextAvailableLSAWRTSession: Uint32): Boolean;
function PrepareUDFBridge_3(var CDFiles: TCDFiles; LastLSAWRTSession: Uint64; ISODataPortionStarting_LSAWRTSession: Uint32): Boolean;
function BuildVRS(Buffer: PAnsiChar): Boolean;
function BuildAVDP(var CDFiles: TCDFiles; Buffer: PAnsiChar; AVDP_LSAWRTSession: Uint64): Boolean;
function BuildVDSs(var CDFiles: TCDFiles; Buffer: PAnsiChar; VolumeLabel: PWideChar): Boolean;
function BuildLVIS(var CDFiles: TCDFiles; Buffer: PAnsiChar): Boolean;
function BuildFSDS(var CDFiles: TCDFiles; Buffer: PAnsiChar): Boolean;
function BuildPacket(var CDFiles: TCDFiles; Packet: PAnsiChar; PacketLength_LBS: Uint8): Boolean;
//---------------------------------------------------------------------------

implementation

uses
  Windows, Math, mbpECMA_167, mbpUDFTime, mbpUDF_CRC, mbpOSTA_UDF_150, mbpUDFUnicode;

const
  ImplementationIdentifier: array [0..22] of AnsiChar = '*BinaryMagic MagicUDF';
//---------------------------------------------------------------------------

{++

Notes:

    1. 260 more blocks are required apart from what is computed from this
       function.
    2. Out of these 260 block, 259 blocks are to be allocated from start of
       image, while 1 block is the last valid logical block of session.

--}
function ParseDiscLayout(var CDFiles: TCDFiles): Boolean;
var
  ptrParentDirEntry: pDirEntry;
  ptrSubFileEntry: pFileEntry;
  UDFParentDirInfo: PUDFDirectoryInformation;
  UDFSubFileInfo: PUDFFileInformation;
  UDFBridgeInfo: PUDFBridgeInformation;
  EnumerateDirectory, DiscLayoutExhausted: Boolean;
  FileIDLength, PaddingLength: Uint8;
  FIDLength: Uint16;
  DirData_LBS: Uint64;
begin
  DiscLayoutExhausted := False;
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

  ptrParentDirEntry := CDFiles.RootDir;
  ptrSubFileEntry := ptrParentDirEntry.files;
  UDFParentDirInfo := @ptrParentDirEntry.UDFDirInfo;
  EnumerateDirectory := True;

  while (True) do
  begin
    // 1st pass
    if (EnumerateDirectory) then
    begin
      UDFParentDirInfo.FileLinkCount := 1;
      UDFParentDirInfo.CurrentFileEntry := nil;
      UDFParentDirInfo.InformationLength := 40;

      while (ptrSubFileEntry <> nil) do
      begin
        if ((ptrSubFileEntry.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then // dir
        begin
          Inc(UDFParentDirInfo.FileLinkCount);
          Inc(UDFBridgeInfo.NumberOfDirectories);
        end
        else // file
        begin
          Inc(UDFBridgeInfo.NumberOfFiles);
        end;

        FileIDLength := (2 * Length(ptrSubFileEntry.LongNameWIN32)) + 1;
        PaddingLength := 4 * ((FileIDLength + 38 + 3) div 4) - (FileIDLength + 38);
        FIDLength := SizeOf(fileIdentDesc) + FileIDLength + PaddingLength;
        Inc(UDFParentDirInfo.InformationLength, FIDLength);

        ptrSubFileEntry := ptrSubFileEntry.NextFile;
      end;

      UDFParentDirInfo.DirICB_LSOffsetWRTSession := UDFBridgeInfo.NextAvailableLSOffsetWRTSession;
      Inc(UDFBridgeInfo.NextAvailableLSOffsetWRTSession);
      Inc(UDFBridgeInfo.NumberOfBlocksRequiredForFileDirStructures);

      UDFParentDirInfo.DirData_LSOffsetWRTSession := UDFBridgeInfo.NextAvailableLSOffsetWRTSession;
      DirData_LBS := Ceil(UDFParentDirInfo.InformationLength / BlockSize);
      Inc(UDFBridgeInfo.NextAvailableLSOffsetWRTSession, DirData_LBS);
      Inc(UDFBridgeInfo.NumberOfBlocksRequiredForFileDirStructures, DirData_LBS);

      ptrSubFileEntry := ptrParentDirEntry.files;
    end;

    // 2nd pass
    while (True) do
    begin
      if (ptrSubFileEntry = nil) then
      begin
        if (ptrParentDirEntry.parent = ptrParentDirEntry) then
        begin
          DiscLayoutExhausted := True;

          Break;
        end;

        ptrParentDirEntry := ptrParentDirEntry.parent;
        UDFParentDirInfo := @ptrParentDirEntry.UDFDirInfo;
        ptrSubFileEntry := UDFParentDirInfo.CurrentFileEntry.NextFile;
        EnumerateDirectory := False;

        Break;
      end;

      if ((ptrSubFileEntry.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then // dir
      begin
        UDFParentDirInfo.CurrentFileEntry := ptrSubFileEntry;
        ptrParentDirEntry := ptrSubFileEntry.DirRecord;
        UDFParentDirInfo := @ptrParentDirEntry.UDFDirInfo;
        ptrSubFileEntry := ptrParentDirEntry.files;
        EnumerateDirectory := True;

        Break;
      end
      else // file
      begin
        UDFSubFileInfo := @ptrSubFileEntry.UDFFileInfo;

        UDFSubFileInfo.FileICB_LSOffsetWRTSession := UDFBridgeInfo.NextAvailableLSOffsetWRTSession;
        Inc(UDFBridgeInfo.NextAvailableLSOffsetWRTSession);
        Inc(UDFBridgeInfo.NumberOfBlocksRequiredForFileDirStructures);

        ptrSubFileEntry := ptrSubFileEntry.NextFile;
      end;
    end;

    if (DiscLayoutExhausted) then
    begin
      Break;
    end;
  end;

  Result := True;
end;
//---------------------------------------------------------------------------

function PrepareUDFBridge_1(var CDFiles: TCDFiles; VolumeRecognitionSequence_LSAWRTSession: Uint32): Boolean;
var
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

  udf_time_to_stamp(@UDFBridgeInfo.UDFTimeStamp, Now_UTC, 0);

  UDFBridgeInfo.NumberOfDirectories := 1; // root dir
  UDFBridgeInfo.NumberOfFiles := 0;
  UDFBridgeInfo.ICB_UniqueID := 16;
  UDFBridgeInfo.NumberOfBlocksRequiredForFileDirStructures := 0;

  UDFBridgeInfo.VolumeRecognitionSequence_LSAWRTSession := VolumeRecognitionSequence_LSAWRTSession;
  UDFBridgeInfo.MainVolumeDescriptorSequenceExtent_LSAWRTSession := 32;
  UDFBridgeInfo.ReserveVolumeDescriptorSequenceExtent_LSAWRTSession := UDFBridgeInfo.MainVolumeDescriptorSequenceExtent_LSAWRTSession + 16;
  UDFBridgeInfo.LogicalVolumeIntegrityDescriptor_LSAWRTSession := UDFBridgeInfo.MainVolumeDescriptorSequenceExtent_LSAWRTSession + 32;
  UDFBridgeInfo.AnchorVolumeDescriptorPointer_LSAWRTSession := 256;
  UDFBridgeInfo.PartitionStarting_LSA := 256 + 1;
  UDFBridgeInfo.FileSetDescriptor_LSAWRTSession := UDFBridgeInfo.AnchorVolumeDescriptorPointer_LSAWRTSession + 1;

  Result := True;
end;
//---------------------------------------------------------------------------

function PrepareUDFBridge_2(var CDFiles: TCDFiles; NextAvailableLSAWRTSession: Uint32): Boolean;
var
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

  UDFBridgeInfo.RootDirectoryICB_LSAWRTSession := NextAvailableLSAWRTSession;
  UDFBridgeInfo.NextAvailableLSOffsetWRTSession := NextAvailableLSAWRTSession;

  ParseDiscLayout(CDFiles);

  UDFBridgeInfo.BackTrack := False;
  UDFBridgeInfo.IsPartialFID := False;
  UDFBridgeInfo.IsDiscLayoutExhausted := False;
  UDFBridgeInfo.CurrentDirEntry := CDFiles.RootDir;
  UDFBridgeInfo.Status := WritingDirICB;

  Result := True;
end;
//---------------------------------------------------------------------------

function PrepareUDFBridge_3(var CDFiles: TCDFiles; LastLSAWRTSession: Uint64; ISODataPortionStarting_LSAWRTSession: Uint32): Boolean;
var
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

  UDFBridgeInfo.LastAnchorVolumeDescriptorPointer_LSAWRTSession := LastLSAWRTSession;
  UDFBridgeInfo.LengthOfPartitionPortionInSession_LBS := (LastLSAWRTSession - 1) - (UDFBridgeInfo.AnchorVolumeDescriptorPointer_LSAWRTSession + 1) + 1;
  UDFBridgeInfo.ISODataPortionStarting_LSAWRTSession := ISODataPortionStarting_LSAWRTSession;

  Result := True;
end;
//---------------------------------------------------------------------------

function FillTag(TagIdentifier: int32; Data: PAnsiChar; DataSize, TagLocation: int32): Tag;
var
  tg: Tag;
  TagCheckSum: Uint8;
  i: int32;
begin
  TagCheckSum := 0;

  FillChar(tg, 16, 0);

  tg.tagIdent := TagIdentifier;
  tg.descVersion := 2; // according to ECMA 167-2
  tg.tagSerialNum := 1; // different for each volume initialization
  tg.descCRCLength := DataSize;
  tg.descCRC := CalculateCRC(Data, DataSize);
  tg.tagLocation := TagLocation;

  for i := 0 to Pred(16) do
  begin
    if (i = 4) then
    begin
      Continue;
    end;
    Inc(TagCheckSum, Ord((PAnsiChar(@tg) + i)^));
  end;
  tg.tagChecksum := TagCheckSum;

  Result := tg;
end;
//---------------------------------------------------------------------------

function BuildVRS(Buffer: PAnsiChar): Boolean; // Volume Recognition Sequence
begin
  FillChar(Buffer^, 3 * BlockSize, 0);

  // Beginning Extending Area Descriptor @ sector 19
  Move('BEA01'#1, Buffer[1], Length('BEA01'#1));
  // Volume Structure Descriptor @ sector 20
  Move('NSR02'#1, Buffer[(1 * BlockSize) + 1], Length('NSR02'#1));
  // Terminating Extending Area Descriptor @ sector 21
  Move('TEA01'#1, Buffer[(2 * BlockSize) + 1], Length('TEA01'#1));

  Result := True;
end;
//---------------------------------------------------------------------------

function BuildAVDP(var CDFiles: TCDFiles; Buffer: PAnsiChar; AVDP_LSAWRTSession: Uint64): Boolean;
var
  AVDP: PanchorVolDescPtr;
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;
  AVDP := PanchorVolDescPtr(Buffer);
  FillChar(Buffer^, BlockSize, 0);

  AVDP.mainVolDescSeqExt.extLength := 16 * BlockSize;
  AVDP.mainVolDescSeqExt.extLocation := UDFBridgeInfo.SessionStarting_LSA + UDFBridgeInfo.MainVolumeDescriptorSequenceExtent_LSAWRTSession;
  AVDP.reserveVolDescSeqExt.extLength := 16 * BlockSize;
  AVDP.reserveVolDescSeqExt.extLocation :=  UDFBridgeInfo.SessionStarting_LSA + UDFBridgeInfo.ReserveVolumeDescriptorSequenceExtent_LSAWRTSession;
  AVDP.descTag := FillTag(TAG_IDENT_AVDP, PAnsiChar(AVDP) + 16, SizeOf(anchorVolDescPtr) - SizeOf(Tag), UDFBridgeInfo.SessionStarting_LSA + AVDP_LSAWRTSession);

  Result := True;
end;
//---------------------------------------------------------------------------

function BuildVDSs(var CDFiles: TCDFiles; Buffer: PAnsiChar; VolumeLabel: PWideChar): Boolean; // Volume Descriptor Sequences
var
  PacketStarting_LSA: Uint32;
  BlockNoInPacket, i: Uint8;
  PVD: primaryVolDesc;
  LVD: LogicalVolDesc;
  PD: partitionDesc;
  USD: UnallocSpaceDesc;
  IUVD: impUseVolDesc;
  TD: TerminatingDesc;
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

  { writing Main/Reserve Volume Descriptor Sequence extents }
  FillChar(UDFBridgeInfo.VolumeIdentifier, SizeOf(UDFBridgeInfo.VolumeIdentifier), 0);
  UDFBridgeInfo.UDFVolumeIdentifierLength := CompressUnicode(Length(VolumeLabel), 16, Punicode_t(VolumeLabel), @UDFBridgeInfo.VolumeIdentifier[0]);

  FillChar(Buffer^, FixedPacketSize, 0);

  PacketStarting_LSA := UDFBridgeInfo.SessionStarting_LSA + UDFBridgeInfo.MainVolumeDescriptorSequenceExtent_LSAWRTSession;

  for i := 0 to Pred(2) do
  begin
    if (i <> 0) then
    begin
      BlockNoInPacket := 16;
    end
    else
    begin
      BlockNoInPacket := 0;
    end;

    //
    // writing Primary Volume Descriptor
    //
    FillChar(PVD, SizeOf(primaryVolDesc), 0);

    PVD.volDescSeqNum := 1;
    Move(UDFBridgeInfo.VolumeIdentifier, PVD.volIdent, SizeOf(UDFBridgeInfo.VolumeIdentifier));
    PVD.volIdent[31] := AnsiChar(UDFBridgeInfo.UDFVolumeIdentifierLength); // Length of dstring
    PVD.volSeqNum := 1;
    PVD.maxVolSeqNum := 1;
    PVD.interchangeLvl := 2;
    PVD.maxInterchangeLvl := 3;
    PVD.charSetList := 1;
    PVD.maxCharSetList := 1;
    ///
    PVD.volSetIdent[0] := AnsiChar(8);
    PVD.volSetIdent[1] := '0';
    PVD.volSetIdent[2] := 'C';
    PVD.volSetIdent[3] := 'F';
    PVD.volSetIdent[4] := 'E';
    PVD.volSetIdent[5] := '9';
    PVD.volSetIdent[6] := '0';
    PVD.volSetIdent[7] := 'C';
    PVD.volSetIdent[8] := 'B';
    PVD.volSetIdent[127] := AnsiChar(9);
    ///

    PVD.descCharSet.charSetType := UDF_CHAR_SET_TYPE;
    Move(UDF_CHAR_SET_INFO, PVD.descCharSet.charSetInfo, Length(UDF_CHAR_SET_INFO));

    PVD.explanatoryCharSet.charSetType := UDF_CHAR_SET_TYPE;
    Move(UDF_CHAR_SET_INFO, PVD.explanatoryCharSet.charSetInfo, Length(UDF_CHAR_SET_INFO));

    PVD.recordingDateAndTime := UDFBridgeInfo.UDFTimeStamp;

    PVD.impIdent.flags := 0;
    Move(ImplementationIdentifier, PVD.impIdent.ident, SizeOf(ImplementationIdentifier));
    PVD.impIdent.identSuffix[0] := AnsiChar(6);

    PVD.descTag := FillTag(TAG_IDENT_PVD, PAnsiChar(@PVD) + SizeOf(Tag), SizeOf(primaryVolDesc) - SizeOf(Tag), PacketStarting_LSA + BlockNoInPacket);
    PprimaryVolDesc(Buffer + (BlockNoInPacket * BlockSize))^ := PVD;

    //
    // writing Logical Volume Descriptor
    //
    FillChar(LVD, SizeOf(LogicalVolDesc), 0);

    LVD.volDescSeqNum := 2;

    LVD.descCharSet.charSetType := UDF_CHAR_SET_TYPE;
    Move(UDF_CHAR_SET_INFO, LVD.descCharSet.charSetInfo, Length(UDF_CHAR_SET_INFO));

    Move(UDFBridgeInfo.VolumeIdentifier, LVD.logicalVolIdent, SizeOf(UDFBridgeInfo.VolumeIdentifier));
    LVD.logicalVolIdent[127] := AnsiChar(UDFBridgeInfo.UDFVolumeIdentifierLength); // Length of dstring

    LVD.logicalBlockSize := BlockSize;

    LVD.domainIdent.flags := 0;
    Move(UDF_ID_COMPLIANT, LVD.domainIdent.ident, Length(UDF_ID_COMPLIANT));
    LVD.domainIdent.identSuffix[0] := AnsiChar($50);
    LVD.domainIdent.identSuffix[1] := AnsiChar($01);
    LVD.domainIdent.identSuffix[2] := AnsiChar(3); // Hard Write-Protect and Soft Write-Protect

    // Logical Volume Contents Use field specifies extent of first File Set Descriptor Sequence
    LVD.logicalVolContentsUse.extLength := 2 * BlockSize;
    LVD.logicalVolContentsUse.extLocation.logicalBlockNum := UDFBridgeInfo.FileSetDescriptor_LSAWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA);
    LVD.logicalVolContentsUse.extLocation.partitionReferenceNum := 0;

    LVD.mapTableLength := SizeOf(PartitionMapType1);
    LVD.numPartitionMaps := 1;

    LVD.impIdent.flags := 0;
    Move(ImplementationIdentifier, LVD.impIdent.ident, SizeOf(ImplementationIdentifier));
    LVD.impIdent.identSuffix[0] := AnsiChar(6);

    LVD.integritySeqExt.extLength := 4 * BlockSize; // minimum of 8K bytes required
    LVD.integritySeqExt.extLocation := UDFBridgeInfo.SessionStarting_LSA + UDFBridgeInfo.LogicalVolumeIntegrityDescriptor_LSAWRTSession;

    // Type 1 Partition Map
    LVD.PartitionMapT1.PartitionMapType := 1;
    LVD.PartitionMapT1.PartitionMapLength := 6;
    LVD.PartitionMapT1.VolumeSequenceNumber := 1;
    LVD.PartitionMapT1.PartitionNumber := 0;

    LVD.descTag := FillTag(TAG_IDENT_LVD, PAnsiChar(@LVD) + SizeOf(Tag), SizeOf(LogicalVolDesc) - SizeOf(Tag), PacketStarting_LSA + BlockNoInPacket + 1);
    PLogicalVolDesc(Buffer + ((BlockNoInPacket + 1) * BlockSize))^ := LVD;

    //
    // writing Partition Descriptor
    //
    FillChar(PD, SizeOf(partitionDesc), 0);

    PD.volDescSeqNum := 3;
    PD.partitionFlags := 1;
    PD.partitionNumber := 0;

    PD.partitionContents.flags := 0;
    Move('+NSR02', PD.partitionContents.ident, Length('+NSR02'));
    PD.partitionContents.identSuffix[0] := AnsiChar($50);
    PD.partitionContents.identSuffix[1] := AnsiChar($01);
    PD.partitionContents.identSuffix[2] := AnsiChar(6);

    PD.accessType := 1; // Read-only
    PD.partitionStartingLocation := UDFBridgeInfo.PartitionStarting_LSA;
    PD.partitionLength := UDFBridgeInfo.SessionStarting_LSA + UDFBridgeInfo.LengthOfPartitionPortionInSession_LBS;

    PD.impIdent.flags := 0;
    Move(ImplementationIdentifier, PD.impIdent.ident, SizeOf(ImplementationIdentifier)); 
    PD.impIdent.identSuffix[0] := AnsiChar(6);

    PD.descTag := FillTag(TAG_IDENT_PD, PAnsiChar(@PD) + SizeOf(Tag), SizeOf(partitionDesc) - SizeOf(Tag), PacketStarting_LSA + BlockNoInPacket + 2);
    PpartitionDesc(Buffer + ((BlockNoInPacket + 2) * BlockSize))^ := PD;

    //
    // writing Unallocated Space Descriptors
    //
    FillChar(USD, SizeOf(UnallocSpaceDesc), 0);

    USD.volDescSeqNum := 4;
    USD.numAllocDescs := 0;
    USD.descTag := FillTag(TAG_IDENT_USD, PAnsiChar(@USD) + SizeOf(Tag), SizeOf(UnallocSpaceDesc) - SizeOf(Tag), PacketStarting_LSA + BlockNoInPacket + 3);
    PUnallocSpaceDesc(Buffer + ((BlockNoInPacket + 3) * BlockSize))^ := USD;

    //
    // writing Implementation Use Volume Descriptor
    //
    FillChar(IUVD, SizeOf(impUseVolDesc), 0);

    IUVD.volDescSeqNum := 5;

    IUVD.impIdent.flags := 0;
    Move(UDF_ID_LV_INFO, IUVD.impIdent.ident, Length(UDF_ID_LV_INFO));
    IUVD.impIdent.identSuffix[0] := AnsiChar($50);
    IUVD.impIdent.identSuffix[1] := AnsiChar($01);
    IUVD.impIdent.identSuffix[2] := AnsiChar(6);

    IUVD.impUse.LVICharSet.charSetType := 0;
    Move(UDF_CHAR_SET_INFO, IUVD.impUse.LVICharSet.charSetInfo, Length(UDF_CHAR_SET_INFO));
    Move(UDFBridgeInfo.VolumeIdentifier, IUVD.impUse.LVId, SizeOf(UDFBridgeInfo.VolumeIdentifier));
    IUVD.impUse.LVId[127] := AnsiChar(UDFBridgeInfo.UDFVolumeIdentifierLength);
    IUVD.impUse.impIdent.flags := 0;
    Move(ImplementationIdentifier, IUVD.impUse.impIdent.ident, SizeOf(ImplementationIdentifier));
    IUVD.impUse.impIdent.identSuffix[0] := AnsiChar(6);

    IUVD.descTag := FillTag(TAG_IDENT_IUVD, PAnsiChar(@IUVD) + SizeOf(Tag), SizeOf(impUseVolDesc) - SizeOf(Tag), PacketStarting_LSA + BlockNoInPacket + 4);
    PimpUseVolDesc(Buffer + ((BlockNoInPacket + 4) * BlockSize))^ := IUVD;

    //
    // writing Terminating Descriptor
    //
    FillChar(TD, SizeOf(TerminatingDesc), 0);

    TD.descTag := FillTag(TAG_IDENT_TD, PAnsiChar(@TD) + SizeOf(Tag), SizeOf(TerminatingDesc) - SizeOf(Tag), PacketStarting_LSA + BlockNoInPacket + 5);
    PTerminatingDesc(Buffer + ((BlockNoInPacket + 5) * BlockSize))^ := TD;
  end;

  Result := True;
end;
//---------------------------------------------------------------------------

function BuildLVIS(var CDFiles: TCDFiles; Buffer: PAnsiChar): Boolean; // Logical Volume Integrity Sequence
var
  LVID: LogicalVolIntegrityDesc;
  LVHD: PlogicalVolHeaderDesc;
  impUse: PLogicalVolIntegrityDescImpUse;
  TD: TerminatingDesc;
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;
  LVHD := PlogicalVolHeaderDesc(@LVID.logicalVolContentsUse[0]);
  impUse := PLogicalVolIntegrityDescImpUse(@LVID.impUse[0]);

  FillChar(Buffer^, 4 * BlockSize, 0);
  FillChar(LVID, SizeOf(LogicalVolIntegrityDesc), 0);

  LVID.recordingDateAndTime := UDFBridgeInfo.UDFTimeStamp;
  LVID.integrityType := 1; // Close

  LVHD.uniqueID := 16 + UDFBridgeInfo.NumberOfDirectories + UDFBridgeInfo.NumberOfFiles;

  LVID.numOfPartitions := 1;
  LVID.lengthOfImpUse := 48;
  LVID.sizeTable := UDFBridgeInfo.SessionStarting_LSA + UDFBridgeInfo.LengthOfPartitionPortionInSession_LBS;

  impUse.numDirs := UDFBridgeInfo.NumberOfDirectories;
  impUse.numFiles := UDFBridgeInfo.NumberOfFiles;
  impUse.minUDFReadRev := $150;
  impUse.minUDFWriteRev := $150;
  impUse.maxUDFWriteRev := $150;
  impUse.impIdent.flags := 0;
  Move(ImplementationIdentifier, impUse.impIdent.ident, SizeOf(ImplementationIdentifier));
  impUse.impIdent.identSuffix[0] := AnsiChar(6);

  LVID.descTag := FillTag(TAG_IDENT_LVID, PAnsiChar(@LVID) + SizeOf(Tag), SizeOf(LogicalVolIntegrityDesc) - SizeOf(Tag), UDFBridgeInfo.SessionStarting_LSA + UDFBridgeInfo.LogicalVolumeIntegrityDescriptor_LSAWRTSession);

  PLogicalVolIntegrityDesc(Buffer)^ := LVID;

  // writing Terminating Descriptor
  FillChar(TD, SizeOf(TerminatingDesc), 0);
  TD.descTag := FillTag(TAG_IDENT_TD, PAnsiChar(@TD) + SizeOf(Tag), SizeOf(TerminatingDesc) - SizeOf(Tag), UDFBridgeInfo.SessionStarting_LSA + UDFBridgeInfo.LogicalVolumeIntegrityDescriptor_LSAWRTSession + 1);

  PTerminatingDesc(Buffer + BlockSize)^ := TD;

  Result := True;
end;
//---------------------------------------------------------------------------

function BuildFSDS(var CDFiles: TCDFiles; Buffer: PAnsiChar): Boolean; // File Set Descriptor Sequence
var
  FSD: FileSetDesc;
  TD: TerminatingDesc;
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

  FillChar(FSD, SizeOf(FileSetDesc), 0);

  FSD.recordingDateAndTime := UDFBridgeInfo.UDFTimeStamp;
  FSD.interchangeLvl := 3;
  FSD.maxInterchangeLvl := 3;
  FSD.charSetList := 1;
  FSD.maxCharSetList := 1;
  FSD.fileSetNum := 0;
  FSD.fileSetDescNum := 0;

  FSD.logicalVolIdentCharSet.charSetType := 0;
  Move(UDF_CHAR_SET_INFO, FSD.logicalVolIdentCharSet.charSetInfo, Length(UDF_CHAR_SET_INFO));

  Move(UDFBridgeInfo.VolumeIdentifier, FSD.logicalVolIdent, SizeOf(UDFBridgeInfo.VolumeIdentifier));
  FSD.logicalVolIdent[127] := AnsiChar(UDFBridgeInfo.UDFVolumeIdentifierLength);

  FSD.fileSetCharSet.charSetType := 0;
  Move(UDF_CHAR_SET_INFO, FSD.fileSetCharSet.charSetInfo, Length(UDF_CHAR_SET_INFO));

  Move(UDFBridgeInfo.VolumeIdentifier, FSD.fileSetIdent, SizeOf(UDFBridgeInfo.VolumeIdentifier));
  FSD.fileSetIdent[31] := AnsiChar(UDFBridgeInfo.UDFVolumeIdentifierLength);

  FSD.rootDirectoryICB.extLength := BlockSize;
  FSD.rootDirectoryICB.extLocation.logicalBlockNum := CDFiles.RootDir.UDFDirInfo.DirICB_LSOffsetWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA);
  FSD.rootDirectoryICB.extLocation.partitionReferenceNum := 0;

  FSD.domainIdent.flags := 0;
  Move(UDF_ID_COMPLIANT, FSD.domainIdent.ident, Length(UDF_ID_COMPLIANT));
  FSD.domainIdent.identSuffix[0] := AnsiChar($50);
  FSD.domainIdent.identSuffix[1] := AnsiChar($01);

  FSD.descTag := FillTag(TAG_IDENT_FSD, PAnsiChar(@FSD) + SizeOf(Tag), SizeOf(FileSetDesc) - SizeOf(Tag), UDFBridgeInfo.FileSetDescriptor_LSAWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA));

  PFileSetDesc(Buffer)^ := FSD;

  // writing Terminating Descriptor
  FillChar(TD, SizeOf(TerminatingDesc), 0);
  TD.descTag := FillTag(TAG_IDENT_TD, PAnsiChar(@TD) + SizeOf(Tag), SizeOf(TerminatingDesc) - SizeOf(Tag), UDFBridgeInfo.FileSetDescriptor_LSAWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA) + 1);

  PTerminatingDesc(Buffer + BlockSize)^ := TD;

  Result := True;
end;
//---------------------------------------------------------------------------

function BuildICB(var CDFiles: TCDFiles; IsDir: Boolean; FileDirEntry: Pointer; Buffer: PAnsiChar): Boolean;
var
  fe: pFileEntry;
  de: pDirEntry;
  ICB: PUDFFileEntry;
  SAD: Pshort_ad;
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  de := nil;
  fe := nil;

  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

  if (IsDir) then
  begin
    de := PDirEntry(FileDirEntry);
  end
  else
  begin
    fe := PFileEntry(FileDirEntry);
  end;

  ICB := PUDFFileEntry(Buffer);
  SAD := Pshort_ad(@ICB.AllocationDescriptors[0]);

  FillChar(ICB^, SizeOf(UDFFileEntry), 0);

  ICB.icbTag.strategyType := 4;
  ICB.icbTag.numEntries := 1;
  if (IsDir) then
  begin
    ICB.icbTag.fileType := 4;
  end
  else
  begin
    ICB.icbTag.fileType := 5;
  end;
  //ICB.icbTag.flags := 528; // 1000010000; Non-relocatable, Contiguous

  ICB.uid := $FFFFFFFF;
  ICB.gid := $FFFFFFFF;
  ICB.permissions := 5285; // 001010010100101
  if (IsDir) then
  begin
    ICB.fileLinkCount := de.UDFDirInfo.FileLinkCount;
    ICB.informationLength := de.UDFDirInfo.InformationLength;
    ICB.logicalBlocksRecorded := Ceil(de.UDFDirInfo.InformationLength / BlockSize);
  end
  else
  begin
    ICB.fileLinkCount := 1;
    ICB.informationLength := fe.FileSize;
    ICB.logicalBlocksRecorded := Ceil(fe.FileSize / BlockSize);
  end;
  ICB.accessTime := UDFBridgeInfo.UDFTimeStamp;
  ICB.modificationTime := UDFBridgeInfo.UDFTimeStamp;
  ICB.attrTime := UDFBridgeInfo.UDFTimeStamp;
  ICB.checkpoint := 1;

  ICB.impIdent.flags := 0;
  Move(ImplementationIdentifier, ICB.impIdent.ident, SizeOf(ImplementationIdentifier));
  ICB.impIdent.identSuffix[0] := AnsiChar(6);
  if ((IsDir) and (de = CDFiles.RootDir)) then
  begin
    ICB.uniqueID := 0;
  end
  else
  begin
    ICB.uniqueID := UDFBridgeInfo.ICB_UniqueID;
    Inc(UDFBridgeInfo.ICB_UniqueID);
  end;
  ICB.lengthAllocDescs := SizeOf(ICB.AllocationDescriptors);

  SAD.extLength := ICB.informationLength;
  if (IsDir) then
  begin
    SAD.extPosition := de.UDFDirInfo.DirData_LSOffsetWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA);

    ICB.descTag := FillTag(TAG_IDENT_FE, PAnsiChar(ICB) + SizeOf(Tag), SizeOf(UDFFileEntry) - SizeOf(Tag), de.UDFDirInfo.DirICB_LSOffsetWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA));
  end
  else
  begin
    if (ICB.informationLength <> 0) then
    begin
      if (fe.Imported) then
      begin
        SAD.extPosition := fe.Address - UDFBridgeInfo.PartitionStarting_LSA;
      end
      else
      begin
        SAD.extPosition := (fe.Address + UDFBridgeInfo.ISODataPortionStarting_LSAWRTSession) + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA);
      end;
    end
    else
    begin
      SAD.extPosition := 0;
    end;

    ICB.descTag := FillTag(TAG_IDENT_FE, PAnsiChar(ICB) + SizeOf(Tag), SizeOf(UDFFileEntry) - SizeOf(Tag), fe.UDFFileInfo.FileICB_LSOffsetWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA));
  end;

  Result := True;
end;
//---------------------------------------------------------------------------

function BuildFID(var CDFiles: TCDFiles; IsDir: Boolean; FileDirEntry: Pointer; ParentDirEntry: pDirEntry; Buffer: PAnsiChar): Uint16;
var
  FID: PfileIdentDesc;
  fe: pFileEntry;
  de: pDirEntry;
  FileIDLength, FIDPaddingLength: Uint8;
  FIDLength: Uint16;
  UDFBridgeInfo: PUDFBridgeInformation;
begin
  de := nil;
  fe := nil;

  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;
  FID := PfileIdentDesc(Buffer);

  if (IsDir) then
  begin
    de := PDirEntry(FileDirEntry);

    if (de = ParentDirEntry.parent) then
    begin
      FileIDLength := 0;
    end
    else
    begin
      FileIDLength := (2 * Length(de.LongName)) + 1;
    end
  end
  else
  begin
    fe := PFileEntry(FileDirEntry);

    FileIDLength := (2 * Length(fe.LongNameWIN32)) + 1;
  end;

  FIDPaddingLength := 4 * ((FileIDLength + 38 + 3) div 4) - (FileIDLength + 38);
  FIDLength := SizeOf(fileIdentDesc) + FileIDLength + FIDPaddingLength;

  FillChar(FID^, FIDLength, 0);

  FID.fileVersionNum := 1;
  if (IsDir) then
  begin
    FID.fileCharacteristics := FID.fileCharacteristics or 2; // 00000010

    if (de = ParentDirEntry.parent) then
    begin
      FID.fileCharacteristics := FID.fileCharacteristics or 8; // 00001000
    end;
  end;
  FID.lengthFileIdent := FileIDLength;

  FID.icb.extLength := BlockSize;
  if (IsDir) then
  begin
    FID.icb.extLocation.logicalBlockNum := de.UDFDirInfo.DirICB_LSOffsetWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA);
  end
  else
  begin
    FID.icb.extLocation.logicalBlockNum := fe.UDFFileInfo.FileICB_LSOffsetWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA);
  end;
  FID.icb.extLocation.partitionReferenceNum := 0;

  if (IsDir) then
  begin
    if (de <> ParentDirEntry.parent) then
    begin
      CompressUnicode(Length(de.LongName), 16, Punicode_t(de.LongName), PAnsiChar(FID) + SizeOf(fileIdentDesc));
    end;
  end
  else
  begin
    CompressUnicode(Length(fe.LongNameWIN32), 16, Punicode_t(fe.LongNameWIN32), PAnsiChar(FID) + SizeOf(fileIdentDesc));
  end;

  FID.descTag := FillTag(TAG_IDENT_FID, PAnsiChar(FID) + SizeOf(Tag), FIDLength - SizeOf(Tag), ParentDirEntry.UDFDirInfo.DirData_LSOffsetWRTSession + (UDFBridgeInfo.SessionStarting_LSA - UDFBridgeInfo.PartitionStarting_LSA) + UDFBridgeInfo.BlockOffsetWritingDirData);

  Result := FIDLength;
end;
//---------------------------------------------------------------------------

function BuildPacket(var CDFiles: TCDFiles; Packet: PAnsiChar; PacketLength_LBS: Uint8): Boolean;
var
  CurrentUDFDirInfo: PUDFDirectoryInformation;
  UDFBridgeInfo: PUDFBridgeInformation;
  BlockOffsetWithinPacket: Uint8;
  ByteOffsetWithinBlock, FIDLength: Uint16;
begin
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

  if (UDFBridgeInfo.IsDiscLayoutExhausted) then
  begin
    Result := False;
    Exit;
  end;

  CurrentUDFDirInfo := @UDFBridgeInfo.CurrentDirEntry.UDFDirInfo;
  BlockOffsetWithinPacket := 0;
  ByteOffsetWithinBlock := 0;

  while (BlockOffsetWithinPacket < PacketLength_LBS) do
  begin
    if (UDFBridgeInfo.Status = WritingDirICB) then
    begin
      BuildICB(CDFiles, True, UDFBridgeInfo.CurrentDirEntry, Packet + (BlockOffsetWithinPacket * BlockSize));
      Inc(BlockOffsetWithinPacket);

      UDFBridgeInfo.Status := WritingDirData;
      UDFBridgeInfo.BlockOffsetWritingDirData := 0;
      CurrentUDFDirInfo.CurrentFileEntry := UDFBridgeInfo.CurrentDirEntry.files;

      Continue;
    end
    else
    if (UDFBridgeInfo.Status = WritingDirData) then
    begin
      if (CurrentUDFDirInfo.CurrentFileEntry = UDFBridgeInfo.CurrentDirEntry.files) then
      begin
        Inc(ByteOffsetWithinBlock, BuildFID(CDFiles, True, UDFBridgeInfo.CurrentDirEntry.parent, UDFBridgeInfo.CurrentDirEntry, Packet + (BlockOffsetWithinPacket * BlockSize)));
      end;

      while (BlockOffsetWithinPacket <= (PacketLength_LBS - 1)) do
      begin
        if (CurrentUDFDirInfo.CurrentFileEntry = nil) then
        begin
          UDFBridgeInfo.BackTrack := True;

          Break;
        end;

        if (UDFBridgeInfo.IsPartialFID) then
        begin
          UDFBridgeInfo.IsPartialFID := False;
          Move(UDFBridgeInfo.FIDContainer[UDFBridgeInfo.PartialFIDLengthRecovered], Packet^, UDFBridgeInfo.PartialFIDLengthToRecover);
          Inc(ByteOffsetWithinBlock, UDFBridgeInfo.PartialFIDLengthToRecover);
          CurrentUDFDirInfo.CurrentFileEntry := CurrentUDFDirInfo.CurrentFileEntry.NextFile;

          Continue;
        end;

        if ((CurrentUDFDirInfo.CurrentFileEntry.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then
        begin
          FIDLength := BuildFID(CDFiles, True, CurrentUDFDirInfo.CurrentFileEntry.DirRecord, UDFBridgeInfo.CurrentDirEntry, @UDFBridgeInfo.FIDContainer[0]);
        end
        else
        begin
          FIDLength := BuildFID(CDFiles, False, CurrentUDFDirInfo.CurrentFileEntry, UDFBridgeInfo.CurrentDirEntry, @UDFBridgeInfo.FIDContainer[0]);
        end;

        if ((BlockOffsetWithinPacket < (PacketLength_LBS - 1)) or ((ByteOffsetWithinBlock + FIDLength) <= BlockSize)) then
        begin
          Move(UDFBridgeInfo.FIDContainer, Packet[(BlockOffsetWithinPacket * BlockSize) + ByteOffsetWithinBlock], FIDLength);

          Inc(ByteOffsetWithinBlock, FIDLength);
          if (ByteOffsetWithinBlock >= BlockSize) then
          begin
            ByteOffsetWithinBlock := ByteOffsetWithinBlock mod BlockSize;
            Inc(BlockOffsetWithinPacket);
            Inc(UDFBridgeInfo.BlockOffsetWritingDirData);
          end;
          CurrentUDFDirInfo.CurrentFileEntry := CurrentUDFDirInfo.CurrentFileEntry.NextFile;

          Continue;
        end
        else
        begin
          UDFBridgeInfo.IsPartialFID := True;
          UDFBridgeInfo.PartialFIDLengthRecovered := BlockSize - ByteOffsetWithinBlock;
          UDFBridgeInfo.PartialFIDLengthToRecover := FIDLength - UDFBridgeInfo.PartialFIDLengthRecovered;
          Move(UDFBridgeInfo.FIDContainer, Packet[(BlockOffsetWithinPacket * BlockSize) + ByteOffsetWithinBlock], UDFBridgeInfo.PartialFIDLengthRecovered);
          Inc(ByteOffsetWithinBlock, UDFBridgeInfo.PartialFIDLengthRecovered);

          Break;
        end;
      end;

      if (UDFBridgeInfo.BackTrack) then
      begin
        CurrentUDFDirInfo.CurrentFileEntry := UDFBridgeInfo.CurrentDirEntry.files;
        if (CurrentUDFDirInfo.CurrentFileEntry <> nil) then
        begin
          UDFBridgeInfo.BackTrack := False;

          if ((CurrentUDFDirInfo.CurrentFileEntry.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then
          begin
            UDFBridgeInfo.CurrentDirEntry := CurrentUDFDirInfo.CurrentFileEntry.DirRecord;
            CurrentUDFDirInfo := @UDFBridgeInfo.CurrentDirEntry.UDFDirInfo;
            UDFBridgeInfo.Status := WritingDirICB;
          end
          else
          begin
            UDFBridgeInfo.Status := WritingFileICB;
          end;
        end;
      end;

      if (BlockOffsetWithinPacket <> PacketLength_LBS) then
      begin
        Inc(BlockOffsetWithinPacket);
        Inc(UDFBridgeInfo.BlockOffsetWritingDirData);
        ByteOffsetWithinBlock := 0;
      end;
    end
    else
    if (UDFBridgeInfo.Status = WritingFileICB) then
    begin
      BuildICB(CDFiles, False, CurrentUDFDirInfo.CurrentFileEntry, Packet + (BlockOffsetWithinPacket * BlockSize));
      Inc(BlockOffsetWithinPacket);

      CurrentUDFDirInfo.CurrentFileEntry := CurrentUDFDirInfo.CurrentFileEntry.NextFile;

      if (CurrentUDFDirInfo.CurrentFileEntry <> nil) then
      begin
        if ((CurrentUDFDirInfo.CurrentFileEntry.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then
        begin
          UDFBridgeInfo.CurrentDirEntry := CurrentUDFDirInfo.CurrentFileEntry.DirRecord;
          CurrentUDFDirInfo := @UDFBridgeInfo.CurrentDirEntry.UDFDirInfo;
          UDFBridgeInfo.Status := WritingDirICB;
        end
        else
        begin
          UDFBridgeInfo.Status := WritingFileICB;
        end;

        Continue;
      end
      else
      begin
        UDFBridgeInfo.BackTrack := True;
      end;
    end;

    if (UDFBridgeInfo.BackTrack) then
    begin
      UDFBridgeInfo.BackTrack := False;
      repeat
        if (UDFBridgeInfo.CurrentDirEntry = UDFBridgeInfo.CurrentDirEntry.parent) then
        begin
          UDFBridgeInfo.IsDiscLayoutExhausted := True;

          Break;
        end;

        UDFBridgeInfo.CurrentDirEntry := UDFBridgeInfo.CurrentDirEntry.parent;
        CurrentUDFDirInfo := @UDFBridgeInfo.CurrentDirEntry.UDFDirInfo;
        CurrentUDFDirInfo.CurrentFileEntry := CurrentUDFDirInfo.CurrentFileEntry.NextFile;
      until (CurrentUDFDirInfo.CurrentFileEntry <> nil);

      if (UDFBridgeInfo.IsDiscLayoutExhausted) then
      begin
        Break;
      end;

      if ((CurrentUDFDirInfo.CurrentFileEntry.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then
      begin
        UDFBridgeInfo.CurrentDirEntry := CurrentUDFDirInfo.CurrentFileEntry.DirRecord;
        CurrentUDFDirInfo := @UDFBridgeInfo.CurrentDirEntry.UDFDirInfo;
        UDFBridgeInfo.Status := WritingDirICB;
      end
      else
      begin
        UDFBridgeInfo.Status := WritingFileICB;
      end;
    end;
  end;

  Result := True;
end;
//---------------------------------------------------------------------------

end. 
