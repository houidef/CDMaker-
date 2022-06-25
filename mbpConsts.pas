unit mbpConsts;

interface

uses Messages;

const
  ERR_NO_ERROR           = 0;
  ERR_NOT_PREPARED       = 1;
  ERR_CANT_OPEN_FILE     = 2;
  ERR_CANT_CREATE_FILE   = 3;
  ERR_ABORTED_BY_USER    = 4;
  ERR_TRIAL_VERSION      = 5;

  ERR_ASPI_NOT_INIT      = 8;

  CS_IDLE                = 0;
  CS_WRITING             = 1;
  CS_ERASING             = 2;
  CS_BACKGROUNDFORMAT    = 3;
  CS_CLOSINGTRACK        = 4;
  CS_CLOSINGDISC         = 5;
  CS_PREPARING           = 6;
  CS_ABORTING            = 7;
  CS_IMPORTINGSESSION    = 8;
  CS_BURNERROR_ABORTING  = 9;
  CS_VERIFYING           = 10;
  CS_GEN_SM_REFERENCES   = 11;

  DC_READ_CDR            = 1;
  DC_READ_CDRW           = 2;
  DC_READ_DVDRAM         = 3;
  DC_READ_DVDR           = 4;
  DC_READ_DVDRW          = 5;
  DC_READ_DVDPLUSR       = 6;
  DC_READ_DVDPLUSRW      = 7;
  DC_READ_DVDPLUSRDL     = 8;
  DC_READ_HDDVDROM       = 9;
  DC_READ_HDDVDR         = 10;
  DC_READ_HDDVDRAM       = 11;
  DC_READ_BDROM          = 12;
  DC_READ_BDR            = 13;
  DC_READ_BDRE           = 14;


  DC_WRITE_CDR           = 21;
  DC_WRITE_CDRW          = 22;
  DC_WRITE_DVDRAM        = 23;
  DC_WRITE_DVDR          = 24;
  DC_WRITE_DVDRW         = 25;
  DC_WRITE_DVDPLUSR      = 26;
  DC_WRITE_DVDPLUSRW     = 27;
  DC_WRITE_DVDPLUSRDL    = 28;
  DC_WRITE_HDDVDR        = 29;
  DC_WRITE_HDDVDRAM      = 30;
  DC_WRITE_BDR           = 31;
  DC_WRITE_BDRE          = 32;

  DC_TEST_WRITE          = 41;
  DC_UNDERRUNPROTECTION  = 42;

  WM_ERASEDONE           = WM_USER + 101;
  WM_WRITEDONE           = WM_USER + 102;
  WM_VERIFYDONE          = WM_USER + 103;
  WM_REFGENDONE          = WM_USER + 104;
  
resourcestring
  ERR_0000 = 'NO ERROR';
  ERR_0001 = 'DATA NOT PREPARED FOR BURNING/WRITING';
  ERR_0002 = 'CAN''T OPEN FILE FOR READ';
  ERR_0003 = 'CAN''T CREATE FILE';
  ERR_0004 = 'ABORTED BY USER';
  ERR_0005 = 'TRIAL VERSION CAN''T CONTINUE';
  ERR_HDWR = 'DEVICE ERROR %d';

implementation

end.
