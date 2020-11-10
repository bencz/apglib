     ‚*---------------------------------------------------------------*
     ‚* STANDARDFELDER FÜR API                                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  10.09.2013                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  VARIABLEN FÜR DEN AUFRUF VON APIS            *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*

     ‚* PARAMETER-BLOCK FÜR QMHSNDPM - SNDPGMMSG                      *
       01  QMHSNDPM-PARAM.
           05 PM-MSGID         PIC  X(07)    VALUE SPACES.
           05 PM-MSGFIL        PIC  X(20)    VALUE "ULCMSGF   *LIBL".
           05 PM-MSGDATA       PIC  X(01)    VALUE " ".
           05 PM-MSGDATALEN    PIC S9(00009) VALUE +1       BINARY.
           05 PM-MSGTYP        PIC  X(10)    VALUE "*DIAG".
           05 PM-MSGSTACKENT   PIC  X(10)    VALUE "*".
           05 PM-MSGSTACKCNT   PIC S9(00009) VALUE +2       BINARY.
           05 PM-MSGKEY        PIC  X(04)    VALUE " ".

       01  QUSRUSAT-PARAM.
           05 US-DATA          PIC  X(128)   VALUE SPACES.
           05 US-DATALEN       PIC S9(00009) VALUE +128         BINARY.
           05 US-FORMAT        PIC  X(10)    VALUE "SPCA0100".
           05 US-LIB           PIC  X(10)    VALUE SPACES.
           05 OC1-STRUCT.
              10 OC1-COUNT     PIC S9(00009) VALUE +1           BINARY.
              10 OC1-KEY1      PIC S9(00009) VALUE +1           BINARY.
              10 OC1-LEN1      PIC S9(00009) VALUE +4           BINARY.
              10 OC1-SIZE1     PIC S9(00009)                    BINARY.
           05 SPCA0100-STRUCT.
              10 SPC-LEN       PIC S9(00009)                    BINARY.
              10 SPC-AVAIL     PIC S9(00009)                    BINARY.
              10 SPC-SIZE      PIC S9(00009)                    BINARY.
              10 SPC-AUTOX     PIC  X(01).
              10 SPC-INITV     PIC  X(01).
              10 SPC-LIB       PIC  X(10).

     ‚* PARAMETER-BLOCK FÜR QUILNGTX - DISPLAY LONG TEXT              *
       01  QUILNGTX-PARAM.
           05 LT-DATA          PIC  X(128)   VALUE SPACES.
           05 LT-DATALEN       PIC S9(00009) VALUE +128         BINARY.
           05 LT-MSGID         PIC  X(10)    VALUE SPACES.
           05 LT-MSGFIL        PIC  X(20)    VALUE "LFSMSGF   *LIBL".

     ‚* The following is copied from QSYSINC/QCBLLESRC.QUSEC so that  *
     ‚* the variable length field EXCEPTION-DATA can be defined as    *
     ‚* 100 bytes for exception data.                                 *
       01  QUS-EC.
           05 BYTES-PROVIDED      PIC S9(00009)  VALUE +0       BINARY.
           05 BYTES-AVAILABLE     PIC S9(00009)  VALUE +0       BINARY.
           05 EXCEPTION-ID        PIC  X(00007)  VALUE SPACES.
           05 RESERVED            PIC  X(00001)  VALUE SPACES.
           05 EXCEPTION-DATA      PIC  X(100)    VALUE SPACES.

     ‚* PARAMETER-BLOCK FÜR QMHRTVM - RTVMSG                          *
       01  QMHRTVM-PARAM.
           05 RM-MSGDTA        PIC  X(3100)  VALUE SPACES.
           05 RM-MSGLEN        PIC S9(00009) VALUE +1       BINARY.
           05 RM-MSGFMT        PIC  X(10)    VALUE SPACES.
           05 RM-MSGID         PIC  X(07)    VALUE SPACES.
           05 RM-MSGFIL        PIC  X(20)    VALUE SPACES.
           05 RM-MSGRDT        PIC  X(01)    VALUE SPACES.
           05 RM-MSGDATALEN    PIC S9(00009) VALUE +1       BINARY.
           05 RM-MSGTYP        PIC  X(10)    VALUE "*DIAG".
           05 RM-MSGSTACKENT   PIC  X(10)    VALUE "*".
           05 RM-MSGSTACKCNT   PIC S9(00009) VALUE +2       BINARY.
           05 RM-MSGKEY        PIC  X(04)    VALUE " ".

     ‚* Variables used for retrieving SEU"s user space (QSUSPC)       *
       01  USRSPC-SIZE            PIC S9(09)     VALUE +0       BINARY.
       01  USRSPC-OFFSET          PIC S9(09)     VALUE +0       BINARY.
       01  USRSPC-NAME.
           05  USRSPC-ID          PIC  X(10)     VALUE "QSUSPC    ".
           05  USRSPC-LIBRARY     PIC  X(10)     VALUE "QTEMP     ".
       01  USRSPC-FORCE-CHANGE    PIC  X(01)     VALUE "0".

     ‚* API QLGSORT
       01  SORT-HDR.
           05 SRT-BLOCKLEN     PIC S9(00009) VALUE +0           BINARY.
           05 SRT-REQTYPE      PIC S9(00009) VALUE +8           BINARY.
           05 SRT-RSVP1        PIC S9(00009) VALUE +0           BINARY.
           05 SRT-OPTIONS      PIC S9(00009) VALUE +0           BINARY.
           05 SRT-RECLEN       PIC S9(00009) VALUE +0           BINARY.
           05 SRT-RECCOUNT     PIC S9(00009) VALUE +0           BINARY.
           05 SRT-OFF2KEY      PIC S9(00009) VALUE +80          BINARY.
           05 SRT-NBROFKEYS    PIC S9(00009) VALUE +1           BINARY.
           05 SRT-OFF2NLSI     PIC S9(00009) VALUE +0           BINARY.
           05 SRT-OFF2IFL      PIC S9(00009) VALUE +0           BINARY.
           05 SRT-NBRINF       PIC S9(00009) VALUE +0           BINARY.
           05 SRT-OFF2OFL      PIC S9(00009) VALUE +0           BINARY.
           05 SRT-NBROUTF      PIC S9(00009) VALUE +0           BINARY.
           05 SRT-KEYENTLEN    PIC S9(00009) VALUE +16          BINARY.
           05 SRT-NLSSLEN      PIC S9(00009) VALUE +290         BINARY.
           05 SRT-IFELEN       PIC S9(00009) VALUE +0           BINARY.
           05 SRT-OFELEN       PIC S9(00009) VALUE +0           BINARY.
           05 SRT-OFF2NBM      PIC S9(00009) VALUE +0           BINARY.
           05 SRT-OFF2VLRA     PIC S9(00009) VALUE +0           BINARY.
           05 SRT-RSVP2        PIC S9(00009) VALUE +0           BINARY.
           05 SRT-KEYINF       OCCURS 1.
              10 KEYINF-ENTRY.
                 15 KEY-START  PIC S9(00009) VALUE +0           BINARY.
                 15 KEY-SIZE   PIC S9(00009) VALUE +0           BINARY.
                 15 KEY-TYP    PIC S9(00009) VALUE +0           BINARY.
                 15 KEY-ORDER  PIC S9(00009) VALUE +0           BINARY.
       01  SRT-RETSIZE         PIC S9(00009) VALUE +0           BINARY.
       01  SRT-SIZELIST        PIC S9(00009) VALUE +0           BINARY.
       01  SRT-NOTUSED         PIC  X(16).

     ‚* API QLGSRTIO
       01  IO-BLOCK.
           05 IOTYPE           PIC S9(00009) VALUE +0           BINARY.
           05 RSVP3            PIC S9(00009) VALUE +0           BINARY.
           05 IORECLEN         PIC S9(00009) VALUE +0           BINARY.
           05 IORECCNT         PIC S9(00009) VALUE +0           BINARY.
       01  OUT-BLOCK.
           05 OUT-RECPR        PIC S9(00009) VALUE +0           BINARY.
           05 OUT-RECAV        PIC S9(00009) VALUE +0           BINARY.
           05 FILLER           PIC S9(00009) VALUE +0           BINARY.
           05 FILLER           PIC S9(00009) VALUE +0           BINARY.

     ‚* Structure of USER SPACE (QSUSPC in QTEMP) as a Single Block.  *
       01  HEADER-SPACE.
           05  HDR-RCDLEN         PIC S9(09)     VALUE 0        BINARY.
           05  HDR-CURRRN         PIC S9(09)     VALUE 0        BINARY.
           05  HDR-CURPOS         PIC S9(09)     VALUE 0        BINARY.
           05  HDR-CCSID          PIC S9(09)     VALUE 0        BINARY.
           05  HDR-RCDSIN         PIC S9(09)     VALUE 0        BINARY.
           05  HDR-MBRNAME        PIC  X(10)     VALUE SPACES.
           05  HDR-SRCFILE        PIC  X(10)     VALUE SPACES.
           05  HDR-LIBNAME        PIC  X(10)     VALUE SPACES.
           05  HDR-MBRTYPE        PIC  X(10)     VALUE SPACES.
           05  HDR-PFKEY          PIC  X(01)     VALUE SPACES.
           05  HDR-MODE           PIC  X(01)     VALUE SPACES.
           05  HDR-SPLIT          PIC  X(01)     VALUE SPACES.
           05  FILLER             PIC  X(01)     VALUE SPACES.
           05  HDR-RTNCODE        PIC  X(01)     VALUE SPACES.
           05  FILLER             PIC  X(03)     VALUE SPACES.
           05  HDR-RCDSOUT        PIC S9(09)     VALUE 0        BINARY.
           05  HDR-SEQINS         PIC  X(07)     VALUE SPACES.
           05  FILLER             PIC  X(21)     VALUE SPACES.
     ‚* Layout of the text line area in SEU user space QTEMP/QSUSPC   *
     ‚*     (See QB3AGX00 - Source Entry Utility, Appendix E)         *
       01  TEXT-SPACE.
           05  TXT-LINECMD        PIC  X(07)     VALUE SPACES.
           05  TXT-RTNCODE        PIC  X(01)     VALUE SPACES.
           05  TXT-SEQNBR         PIC  9(06)     VALUE 0.
           05  TXT-CHGDATE        PIC  9(06)     VALUE 0.
           05  TXT-SRCLINE        PIC  X(256)    VALUE SPACES.
     ‚*€
