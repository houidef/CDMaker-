//---------------------------------------------------------------------------

unit mbpOSTA_UDF_150;
//---------------------------------------------------------------------------

interface

uses
  mbpUDFTypes, mbpECMA_167;

{$I mbpDEFINES.INC}
  
const
  // OSTA CS0 Charspec
  UDF_CHAR_SET_TYPE = 0;
  UDF_CHAR_SET_INFO = 'OSTA Compressed Unicode';

  // Entity Identifier's Identifiers
  UDF_ID_DEVELOPER     = '*Linux UDFFS';
  UDF_ID_COMPLIANT     = '*OSTA UDF Compliant';
  UDF_ID_LV_INFO	     = '*UDF LV Info';
  UDF_ID_FREE_EA	     = '*UDF FreeEASpace';
  UDF_ID_FREE_APP_EA   = '*UDF FreeAppEASpace';
  UDF_ID_DVD_CGMS	     = '*UDF DVD CGMS Info';
  UDF_ID_OS2_EA	       = '*UDF OS/2 EA';
  UDF_ID_OS2_EA_LENGTH = '*UDF OS/2 EALength';
  UDF_ID_MAC_VOLUME    = '*UDF Mac VolumeInfo';
  UDF_ID_MAC_FINDER    = '*UDF Mac FinderInfo';
  UDF_ID_MAC_UNIQUE    = '*UDF Mac UniqueIDTable';
  UDF_ID_MAC_RESOURCE  = '*UDF Mac ResourceFork';
  UDF_ID_VIRTUAL	     = '*UDF Virtual Partition';
  UDF_ID_SPARABLE	     = '*UDF Sparable Partition';
  UDF_ID_ALLOC	       = '*UDF Virtual Alloc Tbl';
  UDF_ID_SPARING	     =  '*UDF Sparing Table';

  // Identifier Suffix
  IS_DF_HARD_WRITE_PROTECT = $01;
  IS_DF_SOFT_WRITE_PROTECT = $02;

  AD_IU_EXT_ERASED = $0001;

  // Real-Time Files
  ICBTAG_FILE_TYPE_REALTIME = $F9;

  ICBTAG_FILE_TYPE_VAT15 = $00;
  ICBTAG_FILE_TYPE_VAT20 = $F8;

  // UDF Defined System Stream
  UDF_ID_UNIQUE_ID = '*UDF Unique ID Mapping Data';
  UDF_ID_NON_ALLOC = '*UDF Non-Allocatable Space';
  UDF_ID_POWER_CAL = '*UDF Power Cal Table';
  UDF_ID_BACKUP	   = '*UDF Backup';

  // Operating System Identifiers
  UDF_OS_CLASS_UNDEF = $00;
  UDF_OS_CLASS_DOS   = $01;
  UDF_OS_CLASS_OS2   = $02;
  UDF_OS_CLASS_MAC   = $03;
  UDF_OS_CLASS_UNIX  = $04;
  UDF_OS_CLASS_WIN9X = $05;
  UDF_OS_CLASS_WINNT = $06;
  UDF_OS_CLASS_OS400 = $07;
  UDF_OS_CLASS_BEOS  = $08;
  UDF_OS_CLASS_WINCE = $09;

  UDF_OS_ID_UNDEF	  = $00;
  UDF_OS_ID_DOS	    = $00;
  UDF_OS_ID_OS2	    = $00;
  UDF_OS_ID_MAC	    = $00;
  UDF_OS_ID_UNIX	  = $00;
  UDF_OS_ID_AIX	    = $01;
  UDF_OS_ID_SOLARIS = $02;
  UDF_OS_ID_HPUX	  = $03;
  UDF_OS_ID_IRIX	  = $04;
  UDF_OS_ID_LINUX	  = $05;
  UDF_OS_ID_MKLINUX = $06;
  UDF_OS_ID_FREEBSD = $07;
  UDF_OS_ID_WIN9X	  = $00;
  UDF_OS_ID_WINNT	  = $00;
  UDF_OS_ID_OS400	  = $00;
  UDF_OS_ID_BEOS	  = $00;
  UDF_OS_ID_WINCE	  = $00;
//---------------------------------------------------------------------------

type
  UDFIdentSuffix = packed record
    UDFRevision: Uint16;
    OSClass: Uchar;
    OSIdentifier: Uchar;
    reserved: array [0..3] of Uchar;
  end;

  impIdentSuffix = packed record
    OSClass: Uchar;
    OSIdentifier: Uchar;
    reserved: array [0..5] of Uchar;
  end;

  appIdentSuffix = packed record
    impUse: array [0..7] of Uchar;
  end;

  PLogicalVolIntegrityDescImpUse = ^LogicalVolIntegrityDescImpUse;
  LogicalVolIntegrityDescImpUse = packed record
    impIdent: regid;
    numFiles: Uint32;
    numDirs: Uint32;
    minUDFReadRev: Uint16;
    minUDFWriteRev: Uint16;
    maxUDFWriteRev: Uint16;
  end;

  impUseVolDescImpUse = packed record
    LVICharset: charspec;
    logicalVolIdent: array [0..127] of dstring;
    LVInfo1: array [0..35] of dstring;
    LVInfo2: array [0..35] of dstring;
    LVInfo3: array [0..35] of dstring;
    impIdent: regid;
    impUse: array [0..127] of Uchar;
  end;

  udfPartitionMap2 = packed record
    partitionMapType: Uchar;
    partitionMapLength: Uchar;
    reserved1: array [0..1] of Uchar;
    partIdent: regid;
    volSeqNum: Uint16;
    partitionNum: Uint16;
  end;

  // Virtual Partition Map
  virtualPartitionMap = packed record
    partitionMapType: Uchar;
    partitionMapLength: Uchar;
    reserved1: array [0..1] of Uchar;
    partIdent: regid;
    volSeqNum: Uint16;
    partitionNum: Uint16;
    reserved2: array [0..23] of Uchar;
  end;

  // Sparable Partition Map
  SparablePartitionMapType2 = packed record
    PartitionMapType: Uint8;
    PartitionMapLength: Uint8;
    Reserved1: array [0..1] of Uint8;
    PartitionTypeIdentifier: regid;
    VolumeSequenceNumber: Uint16;
    PartitionNumber: Uint16;
    PacketLength: Uint16;
    NumberOfSparingTables: Uint8;
    Reserved2: Uint8;
    SizeOfEachSparingTable: Uint32;
    LocationsOfSparingTables: array [0..1] of Uint32;
    Pad: array [0..7] of Uint8;
  end;

  // Logical Volume Descriptor
  PLogicalVolDesc = ^LogicalVolDesc;
  LogicalVolDesc = packed record
    descTag: Tag;
    volDescSeqNum: Uint32;
    descCharSet: charspec;
    logicalVolIdent: array [0..127] of dstring;
    logicalBlockSize: Uint32;
    domainIdent: regid;
    logicalVolContentsUse: long_ad;
    mapTableLength: Uint32;
    numPartitionMaps: Uint32;
    impIdent: regid;
    impUse: array [0..127] of Uchar;
    integritySeqExt: extent_ad;
    PartitionMapT1: PartitionMapType1;
  end;

  // Virtual Allocation Table
  virtualAllocationTable15 = packed record
    VirtualSector: PUint32;
    ident: regid;
    previousVATICB: Uint32;
  end;

  // Virtual Allocation Table
  virtualAllocationTable20 = packed record
    lengthHeader: Uint16;
    lengthImpUse: Uint16;
    logicalVolIdent: array [0..127] of dstring;
    previousVatICBLoc: Uint32;
    numFIDSFiles: Uint32;
    numFIDSDirectories: Uint32;
    minReadRevision: Uint16;
    minWriteRevision: Uint16;
    maxWriteRevision: Uint16;
    reserved: Uint16;
    impUse1stByte: Uchar;
  end;

  // Sparing Table
  PMapEntryInSparingTable = ^MapEntryInSparingTable;
  MapEntryInSparingTable = packed record
    OriginalLocation: Uint32;
    MappedLocation: Uint32;
  end;

  PSparingTable = ^SparingTable;
  SparingTable = packed record
    DescriptorTag: Tag;
    SparingIdentifier: regid;
    ReallocationTableLength: Uint16;
    Reserved: Uint16;
    SequenceNumber: Uint32;
    // Map Entries ...
  end;

  allocDescImpUse = packed record
    flags: Uint16;
    impUse: array [0..3] of Uchar;
  end;

  freeEaSpace = packed record
    headerChecksum: Uint16;
    freeEASpace1stByte: Uchar;
  end;

  DVDCopyrightImpUse = packed record
    headerChecksum: Uint16;
    CGMSInfo: Uchar;
    dataType: Uchar;
    protectionSystemInfo: array [0..3] of Uchar;
  end;

  freeAppEASpace = packed record
    headerChecksum: Uint16;
    freeEASpace1stByte: Uchar;
  end;
//---------------------------------------------------------------------------

implementation

end.
