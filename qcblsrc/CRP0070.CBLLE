       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CRP0070.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ULC: USER DEFINED LINE COMMANDS IM SEU                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  10.09.2013                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ULCSEU (USER DEFINED LINE COMMANDS IM SEU)   *
     ‚*                  AUSFÜHREN, Z.B. AT, T, ISS, ...              *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES.
           CLASS UMLAUTE IS "Ä" "Ö" "Ü" "ä" "ö" "ü" "ß",
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚*    DATEI MIT FELDBESCHREIBUNGEN ÜBER DSPFFD
     ‚*-------------------------------------------------------------
           SELECT QADSPFFD-DATP
                  ASSIGN       TO  DATABASE-QADSPFFD
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*-------------------------------------------------------------
     ‚*    DATEI MIT ZUGRIFFSWEGEN      ÜBER DSPFD
     ‚*-------------------------------------------------------------
           SELECT QAFDACCP-DATP
                  ASSIGN       TO  DATABASE-QAFDACCP
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*-------------------------------------------------------------
     ‚*    DATEI MIT RECORD-FORMATEN    ÜBER DSPFD
     ‚*-------------------------------------------------------------
           SELECT QAFDRFMT-DATP
                  ASSIGN       TO  DATABASE-QAFDRFMT
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*-------------------------------------------------------------
     ‚*    DATEI MIT SELEKTIONEN        ÜBER DSPFD
     ‚*-------------------------------------------------------------
           SELECT QAFDSELO-DATP
                  ASSIGN       TO  DATABASE-QAFDSELO
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----PHYSISCHE DATEI FELD-BESCHREIBUNGEN
       FD  QADSPFFD-DATP
           LABEL RECORD IS STANDARD.
       01  QADSPFFD-RECP.
           COPY DDS-ALL-FORMATS OF QADSPFFD.
      /
     ‚*----PHYSISCHE DATEI ZUGRIFFSWEGE
       FD  QAFDACCP-DATP
           LABEL RECORD IS STANDARD.
       01  QAFDACCP-RECP.
           COPY DDS-ALL-FORMATS OF QAFDACCP.
      /
     ‚*----PHYSISCHE DATEI RECORD-FORMAT
       FD  QAFDRFMT-DATP
           LABEL RECORD IS STANDARD.
       01  QAFDRFMT-RECP.
           COPY DDS-ALL-FORMATS OF QAFDRFMT.
      /
     ‚*----PHYSISCHE DATEI SELEKTIONEN
       FD  QAFDSELO-DATP
           LABEL RECORD IS STANDARD.
       01  QAFDSELO-RECP.
           COPY DDS-ALL-FORMATS OF QAFDSELO.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* STANDARDFELDER APIS
           COPY WRKAPI     OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "CRP0070".
       01  PGM-TYP                        PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  RTVMBRD-REC               TYPE RTVMBRD-TYPE.
       01  RTVOBJD-REC               TYPE RTVOBJD-TYPE.
       01  CRTDTAARA-REC             TYPE CRTDTAARA-TYPE.
       01  CHGDTAARA-REC             TYPE CHGDTAARA-TYPE.
       01  RTVDTAARA-REC             TYPE RTVDTAARA-TYPE.

       01  PGM-LIB                PIC  X(10)     VALUE SPACES.
       01  SWBLK                  PIC  X(01)     VALUE "0".
       01  COMMAND-CODE           PIC  X(07)     VALUE SPACES.
       01  COMMAND-SAVE           PIC  X(07)     VALUE SPACES.
       01  WRK-FELD1              PIC  X(01)     VALUE SPACES.
       01  WRK-FELD2              PIC  X(02)     VALUE SPACES.
       01  WRK-FELD3              PIC  X(03)     VALUE SPACES.
       01  WRK-FELD5              PIC  X(05)     VALUE SPACES.
       01  WRK-FELD7              PIC  X(07)     VALUE SPACES.
       01  WRK-FELD8              PIC  X(08)     VALUE SPACES.
       01  WRK-FELD10             PIC  X(10)     VALUE SPACES.
       01  WRK-FELD12             PIC  X(12)     VALUE SPACES.
       01  WRK-FELD50             PIC  X(50)     VALUE SPACES.

       01  CHGID                  PIC  X(05)     VALUE SPACES.
       01  CHGMRK                 PIC  X(05)     VALUE SPACES.
       01  PHYFILE                PIC  X(10)     VALUE SPACES.
       01  PHYFILE2               PIC  X(10)     VALUE SPACES.
       01  RCDNAME                PIC  X(10)     VALUE SPACES.
       01  CONT-CHR               PIC  X(01)     VALUE SPACES.
       01  CONT-CHR2              PIC  X(01)     VALUE SPACES.
       01  BOXPAR1                PIC  X(05)     VALUE SPACES.
       01  BOXPAR2                PIC  X(05)     VALUE SPACES.

       01  ALL9                   PIC  X(32).
       01  UPCHAR                 PIC  X(29)
               VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ".
       01  LOCHAR                 PIC  X(29)
               VALUE "abcdefghijklmnopqrstuvwxyzäöü".

       01  HLPTXT                 PIC  X(2048)   VALUE SPACES.
       01  EXMSGID                PIC  X(07)     VALUE SPACES.

       01  LINEPRFX-LEN           PIC S9(05)     VALUE +20      COMP-3.
       01  IDX                    PIC S9(05)     VALUE +0       COMP-3.
       01  RECNO                  PIC S9(09)     VALUE +0       COMP-3.
       01  EXTNO                  PIC S9(09)     VALUE +0       COMP-3.
       01  WRKJ                   PIC S9(05)     VALUE +0       COMP-3.
       01  WRKK                   PIC S9(05)     VALUE +0       COMP-3.
       01  WRKC                   PIC S9(05)     VALUE +0       COMP-3.
       01  WRKE                   PIC S9(05)     VALUE +0       COMP-3.
       01  WRKL                   PIC S9(05)     VALUE +0       COMP-3.
       01  WRKP                   PIC S9(05)     VALUE +0       COMP-3.
       01  WRKW                   PIC S9(05)     VALUE +0       COMP-3.
       01  WRKX                   PIC S9(05)     VALUE +0       COMP-3.
       01  WRKZ                   PIC S9(05)     VALUE +0       COMP-3.
       01  BOXL                   PIC S9(05)     VALUE +0       COMP-3.
       01  BOXR                   PIC S9(05)     VALUE +0       COMP-3.
       01  CURCOL                 PIC S9(05)     VALUE +0       COMP-3.
       01  BEGCOL                 PIC S9(05)     VALUE +0       COMP-3.
       01  ENDCOL                 PIC S9(05)     VALUE +0       COMP-3.
       01  BEGSAVE1               PIC S9(05)     VALUE +0       COMP-3.
       01  ENDSAVE1               PIC S9(05)     VALUE +0       COMP-3.
       01  BEGSAVE2               PIC S9(05)     VALUE +0       COMP-3.
       01  ENDSAVE2               PIC S9(05)     VALUE +0       COMP-3.
     ‚* SETZE VERARBEITUNGSMODE DER AKTUELLEN ZEILE                   *
       01  PROCESS-MODE           PIC  X(01)     VALUE SPACE.
           88  BLOCK-MODE                        VALUE "B".
           88  SINGLE-MODE                       VALUE "S".
           88  DONT-PROCESS                      VALUE SPACE.
     ‚* SETZE UPDATE-MODUS
       01  FEHLER-STATUS          PIC  X(01)     VALUE SPACE.
           88  ERROR-FOUND                       VALUE "1".
           88  NO-UPDATE                         VALUE "1".
     ‚* SCHALTER ZUM LÖSCHEN DES ZEILEN-BEFEHLS                       *
       01  COMMAND-MODE           PIC  X(01)     VALUE SPACE.
           88  CLEAR-COMMAND                     VALUE "Y".
           88  LEAVE-COMMAND                     VALUE "N".
     ‚* SCHALTER ZUM WECHSEL VON LOWER-CASE TO UPPER-CASE             *
       01  CASE-MODE              PIC  X(01)     VALUE SPACE.
           88  UPPER-CASE                        VALUE "U".
           88  LOWER-CASE                        VALUE "L".
     ‚* SCHALTER MERKE BEDINGUNG                                      *
       01  COND-RAISED            PIC  X(01)     VALUE SPACE.
           88  COND-YES                          VALUE "Y".
           88  COND-NO                           VALUE "N".
       01  NEU-SRCLINE            PIC  X(256)    VALUE SPACES.

     ‚* AUSFÜHREN EINES BEFEHLS ÜBER CRP0070CL                        *
       01  EXC-PARAMS.
           10 EXC-COMMAND                    PIC X(10).
           10 EXC-FILE                       PIC X(10).
           10 EXC-LIB                        PIC X(10).
           10 EXC-MBR                        PIC X(10).
           10 EXC-TYPE                       PIC X(10).
           10 EXC-TEXT                       PIC X(50).
           10 EXC-TXT3000                    PIC X(3000).
           10 EXC-LNG                        PIC 9(03).
           10 EXC-RC                         PIC X(01).

     ‚* FARB-ATRIBUT-TABELLE                                          *
     ‚* HINWEIS: HEX CODES 27, 2F, 37, 3F SIND SCHWARZ                *
     ‚*          UND SIND NICHT UNTERSTÜTZT                           *
        01 AT-CODES.
     ‚*       WHITE (STANDARD)
           10 ATH                 PIC  X(03)     VALUE "H".
           10 HEX-22              PIC  X(01)     VALUE X"22".
     ‚*       GREEN
           10 ATG                 PIC  X(03)     VALUE "G".
           10 HEX-20              PIC  X(01)     VALUE X"20".
     ‚*       GREEN (NORMAL)
           10 ATN                 PIC  X(03)     VALUE "N".
           10 FILLER              PIC  X(01)     VALUE X"20".
     ‚*       REVERSE GREEN
           10 ATGR                PIC  X(03)     VALUE "GR".
           10 HEX-21              PIC  X(01)     VALUE X"21".
     ‚*       WHITE
           10 ATW                 PIC  X(03)     VALUE "W".
           10 HEX-22              PIC  X(01)     VALUE X"22".
     ‚*       WHITE (HILIGHT)
           10 ATHI                PIC  X(03)     VALUE "HI".
           10 HEX-22              PIC  X(01)     VALUE X"22".
     ‚*       REVERSE WHITE
           10 ATWR                PIC  X(03)     VALUE "WR".
           10 HEX-23              PIC  X(01)     VALUE X"23".
     ‚*       UNDERLINED GREEN
           10 ATGU                PIC  X(03)     VALUE "GU".
           10 HEX-24              PIC  X(01)     VALUE X"24".
     ‚*       UNDERLINED, REVERSE GREEN
           10 ATGRU               PIC  X(03)     VALUE "GRU".
           10 HEX-25              PIC  X(01)     VALUE X"25".
     ‚*       UNDERLINED, REVERSE WHITE
           10 ATWRU               PIC  X(03)     VALUE "WRU".
           10 HEX-26              PIC  X(01)     VALUE X"26".
     ‚*       RED
           10 ATR                 PIC  X(03)     VALUE "R".
           10 HEX-28              PIC  X(01)     VALUE X"28".
     ‚*       REVERSE RED
           10 ATRR                PIC  X(03)     VALUE "RR".
           10 HEX-29              PIC  X(01)     VALUE X"29".
     ‚*       BLINK (RED)
           10 ATBL                PIC  X(03)     VALUE "BL".
           10 HEX-2A              PIC  X(01)     VALUE X"2A".
     ‚*       BLINK REVERSE (RED)
           10 ATBLR               PIC  X(03)     VALUE "BLR".
           10 HEX-2B              PIC  X(01)     VALUE X"2B".
     ‚*       RED UNDERSCORE
           10 ATRU                PIC  X(03)     VALUE "RU".
           10 HEX-2C              PIC  X(01)     VALUE X"2C".
     ‚*       RED REVERSE UNDERSCORE
           10 ATRRU               PIC  X(03)     VALUE "RRU".
           10 HEX-2D              PIC  X(01)     VALUE X"2D".
     ‚*       BLINK UNDERSCORE (RED)
           10 ATBLU               PIC  X(03)     VALUE "BLU".
           10 HEX-2E              PIC  X(01)     VALUE X"2E".
     ‚*       TURQUOISE
           10 ATT                 PIC  X(03)     VALUE "T".
           10 HEX-30              PIC  X(01)     VALUE X"30".
     ‚*       REVERSE TURQUOISE
           10 ATTR                PIC  X(03)     VALUE "TR".
           10 HEX-31              PIC  X(01)     VALUE X"31".
     ‚*       YELLOW
           10 ATY                 PIC  X(03)     VALUE "Y".
           10 HEX-32              PIC  X(01)     VALUE X"32".
     ‚*       REVERSE YELLOW
           10 ATYR                PIC  X(03)     VALUE "YR".
           10 HEX-33              PIC  X(01)     VALUE X"33".
     ‚*       UNDERLINED TURQUOISE
           10 ATTU                PIC  X(03)     VALUE "TU".
           10 HEX-34              PIC  X(01)     VALUE X"34".
     ‚*       UNDERLINED, REVERSE TURQUOISE
           10 ATTRU               PIC  X(03)     VALUE "TRU".
           10 HEX-35              PIC  X(01)     VALUE X"35".
     ‚*       UNDERLINED YELLOW
           10 ATYU                PIC  X(03)     VALUE "YU".
           10 HEX-36              PIC  X(01)     VALUE X"36".
     ‚*       PINK (MAGENTA)
           10 ATM                 PIC  X(03)     VALUE "M".
           10 HEX-38              PIC  X(01)     VALUE X"38".
     ‚*       REVERSE PINK
           10 ATMR                PIC  X(03)     VALUE "MR".
           10 HEX-39              PIC  X(01)     VALUE X"39".
     ‚*       BLUE
           10 ATB                 PIC  X(03)     VALUE "B".
           10 HEX-3A              PIC  X(01)     VALUE X"3A".
     ‚*       BLUE REVERSE
           10 ATBR                PIC  X(03)     VALUE "BR".
           10 HEX-3B              PIC  X(01)     VALUE X"3B".
     ‚*       PINK UNDERSCORE
           10 ATMU                PIC  X(03)     VALUE "MU".
           10 HEX-3C              PIC  X(01)     VALUE X"3C".
     ‚*       PINK REVERSE UNDERSCORE
           10 ATMRU               PIC  X(03)     VALUE "MRU".
           10 HEX-3D              PIC  X(01)     VALUE X"3D".
     ‚*       BLUE UNDERSCORE
           10 ATBU                PIC  X(03)     VALUE "BU".
           10 HEX-3E              PIC  X(01)     VALUE X"3E".
     ‚*       OFF
           10 ATOFF               PIC  X(03)     VALUE "-".
           10 HEX-40              PIC  X(01)     VALUE SPACE.

        01 AT-TABELLE.
           05 AT-FOUND.
              10 AT-CMD           PIC X(03).
              10 AT-HEX           PIC X(01).
           05 AT-ANZAHL           PIC 9(03).
           05 ATX                 PIC 9(03).
           05 AT-EINTRAEGE.
              10 AT-EINTRAG       OCCURS 33 TIMES.
                 15 AT-CMD        PIC X(03).
                 15 AT-HEX        PIC X(01).

     ‚* DEFINITION DER FILETYP-TABELLE                                *
        01 FT-CODES.
           10 FILLER              PIC X(10) VALUE "RPG     ".
           10 FILLER              PIC X(04) VALUE "1R  ".
           10 FILLER              PIC X(10) VALUE "RPGLE   ".
           10 FILLER              PIC X(04) VALUE "1R  ".
           10 FILLER              PIC X(10) VALUE "SQLRPG  ".
           10 FILLER              PIC X(04) VALUE "1RS ".
           10 FILLER              PIC X(10) VALUE "SQLRPGLE".
           10 FILLER              PIC X(04) VALUE "1RS ".
           10 FILLER              PIC X(10) VALUE "CBL     ".
           10 FILLER              PIC X(04) VALUE "1C  ".
           10 FILLER              PIC X(10) VALUE "CBLLE   ".
           10 FILLER              PIC X(04) VALUE "1C  ".
           10 FILLER              PIC X(10) VALUE "SQLCBL  ".
           10 FILLER              PIC X(04) VALUE "1CS ".
           10 FILLER              PIC X(10) VALUE "SQLCBLLE".
           10 FILLER              PIC X(04) VALUE "1CS ".
           10 FILLER              PIC X(10) VALUE "PF      ".
           10 FILLER              PIC X(04) VALUE "2   ".
           10 FILLER              PIC X(10) VALUE "LF      ".
           10 FILLER              PIC X(04) VALUE "2   ".
           10 FILLER              PIC X(10) VALUE "PRTF    ".
           10 FILLER              PIC X(04) VALUE "2   ".
           10 FILLER              PIC X(10) VALUE "DSPF    ".
           10 FILLER              PIC X(04) VALUE "2   ".
           10 FILLER              PIC X(10) VALUE "MNUDDS  ".
           10 FILLER              PIC X(04) VALUE "2   ".
           10 FILLER              PIC X(10) VALUE "TXT     ".
           10 FILLER              PIC X(04) VALUE "0   ".
           10 FILLER              PIC X(10) VALUE "SQL     ".
           10 FILLER              PIC X(04) VALUE "0 S ".
           10 FILLER              PIC X(10) VALUE "REXX    ".
           10 FILLER              PIC X(04) VALUE "0   ".
           10 FILLER              PIC X(10) VALUE "CMD     ".
           10 FILLER              PIC X(04) VALUE "0P  ".
           10 FILLER              PIC X(10) VALUE "CLP     ".
           10 FILLER              PIC X(04) VALUE "0P  ".
           10 FILLER              PIC X(10) VALUE "CL      ".
           10 FILLER              PIC X(04) VALUE "0P  ".
           10 FILLER              PIC X(10) VALUE "CLLE    ".
           10 FILLER              PIC X(04) VALUE "0P  ".
           10 FILLER              PIC X(10) VALUE "C       ".
           10 FILLER              PIC X(04) VALUE "0   ".
           10 FILLER              PIC X(10) VALUE "H       ".
           10 FILLER              PIC X(04) VALUE "0   ".
           10 FILLER              PIC X(10) VALUE "PNLGRP  ".
           10 FILLER              PIC X(04) VALUE "0   ".
           10 FILLER              PIC X(10) VALUE "LAYOUT  ".
           10 FILLER              PIC X(04) VALUE "0   ".
           10 FILLER              PIC X(10) VALUE "XML     ".
           10 FILLER              PIC X(04) VALUE "0   ".

        01 FT-TABELLE.
           05 FT-FOUND.
     ‚*          TYP=MEMBERTYP
              10 TYP              PIC X(10).
     ‚*          KZ =KLASSE 0=TEXTE 1=SPRACHEN 2=DEFINITIONEN
              10 KZ               PIC X(01).
     ‚*          PGM=C:COBOL R:RPG P:PROZEDUREN
              10 PGM              PIC X(01).
     ‚*          SQL=S:SQLSTATEMENTS
              10 SQL              PIC X(01).
              10 FILLER           PIC X(01).
           05 FT-ANZAHL           PIC 9(03).
           05 FTX                 PIC 9(03).
           05 FT-EINTRAEGE.
              10 FT-EINTRAG       OCCURS 25 TIMES.
                 15 TYP           PIC X(10).
                 15 KZ            PIC X(01).
                 15 PGM           PIC X(01).
                 15 SQL           PIC X(01).
                 15 FILLER        PIC X(01).

     ‚* DEFINITION DER TABELLE DER COBOL-BEFEHLE                      *
        01 CMD-CODES.
           10 FILLER              PIC  X(12) VALUE "MOVE      ".
           10 FILLER              PIC  X(12) VALUE "PERFORM   ".
           10 FILLER              PIC  X(12) VALUE "EVALUATE  ".
           10 FILLER              PIC  X(12) VALUE "WHEN      ".
           10 FILLER              PIC  X(12) VALUE "EXIT      ".
           10 FILLER              PIC  X(12) VALUE "ADD       ".
           10 FILLER              PIC  X(12) VALUE "SUBTRACT  ".
           10 FILLER              PIC  X(12) VALUE "DIVIDE    ".
           10 FILLER              PIC  X(12) VALUE "MULTIPLY  ".
           10 FILLER              PIC  X(12) VALUE "COMPUTE   ".
           10 FILLER              PIC  X(12) VALUE "STRING    ".
           10 FILLER              PIC  X(12) VALUE "UNSTRING  ".
           10 FILLER              PIC  X(12) VALUE "INITIALIZE".
           10 FILLER              PIC  X(12) VALUE "CALL      ".
           10 FILLER              PIC  X(12) VALUE "INSPECT   ".
           10 FILLER              PIC  X(12) VALUE "SET       ".
           10 FILLER              PIC  X(12) VALUE "OPEN      ".
           10 FILLER              PIC  X(12) VALUE "START     ".
           10 FILLER              PIC  X(12) VALUE "READ      ".
           10 FILLER              PIC  X(12) VALUE "WRITE     ".
           10 FILLER              PIC  X(12) VALUE "REWRITE   ".
           10 FILLER              PIC  X(12) VALUE "DELETE    ".
           10 FILLER              PIC  X(12) VALUE "CLOSE     ".
           10 FILLER              PIC  X(12) VALUE "SORT      ".
           10 FILLER              PIC  X(12) VALUE "RELEASE'  ".
           10 FILLER              PIC  X(12) VALUE "RETURN    ".
           10 FILLER              PIC  X(12) VALUE "GO        ".
           10 FILLER              PIC  X(12) VALUE "IF        ".
           10 FILLER              PIC  X(12) VALUE "THEN      ".
           10 FILLER              PIC  X(12) VALUE "AND       ".
           10 FILLER              PIC  X(12) VALUE "OR        ".
           10 FILLER              PIC  X(12) VALUE "ELSE      ".
           10 FILLER              PIC  X(12) VALUE "CONTINUE  ".
           10 FILLER              PIC  X(12) VALUE "SELECT    ".

        01 CMD-TABELLE.
           05 CMD-ANZAHL          PIC 9(03).
           05 CMDX                PIC 9(03).
           05 CMD-EINTRAEGE.
              10 CMD-EINTRAG      OCCURS 50 TIMES.
                 15 CMD-NAME      PIC  X(12).

     ‚* DEFINITION DER TABELLE ZUM EINFÜGEN EINER COBOL-SECTION (ISS) *
        01 ISS-TEXT.
           10 FILLER              PIC  X(40)
              VALUE "*---------------------------------------".
           10 FILLER              PIC  X(40)
              VALUE "* <bitte Kommentar eingeben>            ".
           10 FILLER              PIC  X(40)
              VALUE "*---------------------------------------".
           10 FILLER              PIC  X(40)
              VALUE " neue-sektion                SECTION.   ".
           10 FILLER              PIC  X(40)
              VALUE " xxx-yyy-00.                            ".
           10 FILLER              PIC  X(40)
              VALUE "                                        ".
           10 FILLER              PIC  X(40)
              VALUE "     INITIALIZE RET-CODE.               ".
           10 FILLER              PIC  X(40)
              VALUE "                                        ".
           10 FILLER              PIC  X(40)
              VALUE " xxx-yyy-90.                            ".
           10 FILLER              PIC  X(40)
              VALUE "     EXIT.                              ".
           10 FILLER              PIC  X(40)
              VALUE "/                                       ".

        01 ISS-TABELLE.
           05 ISS-ANZAHL          PIC 9(03).
           05 ISSX                PIC 9(03).
           05 ISS-EINTRAEGE.
              10 ISS-EINTRAG      OCCURS 12 TIMES.
                 15 ISS-ZEILE     PIC  X(40).

     ‚* DEFINITION DER NACHRICHTEN-TABELLE (VARIABEL)                 *
        01 MSG-TABELLE.
           05 MSG-ANZAHL          PIC 9(03).
           05 MSGX                PIC 9(03).
           05 MSG-EINTRAEGE.
              10 MSG-EINTRAG      OCCURS 1 TO 999 TIMES
                                  DEPENDING ON MSG-ANZAHL.
                 15 MSG-ZEILE     PIC  X(72).

     ‚* DEFINITION ZWISCHENSPEICHER SOURCE-ZEILEN                     *
        01 SRC-TABELLE.
           05 SRC-ANZAHL          PIC 9(03).
           05 SRCX                PIC 9(03).
           05 SRC-EINTRAEGE.
              10 SRC-EINTRAG      OCCURS 1 TO 99  TIMES
                                  DEPENDING ON SRC-ANZAHL.
                 15 SRC-ZEILE     LIKE TXT-SRCLINE.
        01 SRC-BUFFER.
              10 SRC-BFRSPACE     OCCURS 20 TIMES
                                  LIKE TXT-SRCLINE.

     ‚* DEFINITION DER FELD-TABELLE (VARIABEL)                        *
        01 FLD-TABELLE.
           05 FLD-ANZAHL          PIC 9(03).
           05 FLDX                PIC 9(03).
           05 FLD-EINTRAEGE.
              10 FLD-EINTRAG      OCCURS 1 TO 999 TIMES
                                  DEPENDING ON FLD-ANZAHL.
                 15 WHFLDI        LIKE WHFLDI OF QADSPFFD-RECP.
                 15 WHFLDT        LIKE WHFLDT OF QADSPFFD-RECP.
                 15 WHFLDB        LIKE WHFLDB OF QADSPFFD-RECP.
                 15 WHFLDD        LIKE WHFLDD OF QADSPFFD-RECP.
                 15 WHFLDP        LIKE WHFLDP OF QADSPFFD-RECP.
                 15 WHFTXT        LIKE WHFTXT OF QADSPFFD-RECP.
                 15 WHNAME        LIKE WHNAME OF QADSPFFD-RECP.

     ‚* VARIABLEN FÜR DAS ZEILEN AUSLESEN
       01  ACT-LINE                       PIC S9(5) VALUE +0 COMP-3.
       01  END-LINE                       PIC S9(5) VALUE +0 COMP-3.

     ‚* VARIABLEN FÜR DEN BEFEHLSMERKER (BLOCKANFANG, BLOCKENDE)
       01  COMMAND-1                      PIC X(7).
       01  COMMAND-2                 LIKE COMMAND-1.
       01  ANZ-COMMAND                    PIC S9(5) VALUE +0 COMP-3.
       01  ANZ-BLK-CMD               LIKE ANZ-COMMAND.

     ‚* VARIABLEN FÜR DIE TAG-FUNKTION (T, KT, ...)
       01  TAG-VALUE                      PIC X(3).
       01  FINAL-TAG-VALUE                PIC X(5).
       01  FORCE-TAG                      PIC 9(1).

     ‚* VARIABLEN FÜR DIE AT-FUNKTION
       01  ALTES-ATTRIBUT                 PIC X(1).
       01  WORT-ANFANG-POS                PIC 9(4).
       01  WORT-ENDE-POS                  PIC 9(4).

     ‚* VARIABLEN FÜR DIE DT-FUNKTION
       01  TEXT-ID                        PIC X(7).

     ‚* DEFINITION DER DATAAREA ULCDTAARA                             *
       01  LCLDATA.
        05 LCLMRK                         PIC  X(5).
        05 LCLLIB                         PIC  X(10).

     ‚* VARIABLEN FÜR DAS MSGF-HANDLING
       01  MSG-FILE                       PIC X(10).
       01  MSG-ID                         PIC X(7).
       01  MSG-TXT                        PIC X(128).

     ‚* LÄNGEN- UND POSITIONSVARIABLEN
       01  POS                            PIC 9(4).

     ‚* SONSTIGE BERECHNUNGSVARIABLEN
       01  REST                           PIC 9(4).

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚* VON SEU ÜBERGEBENE POINTER - WERDEN NICHT VERWENDET.
       01  X-SEU-INPUT              USAGE POINTER.
       01  X-SEU-OUTPUT             USAGE POINTER.
       01  X-SEU-SOURCE             USAGE POINTER.
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-SEU-INPUT
                                X-SEU-OUTPUT
                                X-SEU-SOURCE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* INITIALISIERUNGEN
           PERFORM  INIT-VARS.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           PERFORM  COPY-PGM-INIT-SIMPLE.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
     ‚*             DEN DATENBEREICH FÜR DEN ULC ERSTELLEN
                    PERFORM CREATE-ULC-DTAARA
           END-IF.

     ‚* EINLESEN DES HEADER-BLOCKS IN QSUSPC
           PERFORM  GET-USRSPC-HEADER.

           MOVE     HDR-CURPOS TO CURCOL.
           COMPUTE  HDR-RCDSOUT = HDR-RCDSIN - 1.
           COMPUTE  END-LINE    = HDR-RCDSIN - 1.

     ‚* ERMITTELN DES MEMBERTYP (Z.B. CBLLE) WENN WIR UNS IM ANZEIGE
     ‚* MODUS BEFINDEN, DENN DANN IST DIESER NICHT GEFÜLLT UND MUSS
     ‚* GESONDERT ERMITTELT WERDEN
           IF       HDR-MODE    NOT = "U"
            AND     HDR-MBRTYPE     = SPACES
                    PERFORM  GET-MBR-TYPE
                    MOVE     SRCTYPE OF RTVMBRD-REC TO HDR-MBRTYPE
           END-IF.
           PERFORM  CHECK-MBR-TYPE.

     ‚* POSITIONIERE AUF 1. ZEILE
           MOVE     LENGTH OF HEADER-SPACE TO USRSPC-OFFSET.
           ADD      1 TO USRSPC-OFFSET.
           MOVE     1 TO RECNO.
           SET      DONT-PROCESS TO TRUE.

     ‚* ALLE VOM USER EINGEGEBEN ZEILENBEFEHLE VERARBEITEN. DAZU
     ‚* ERST EINMAL ALLE BEFEHLE AUSLESEN UND ZWISCHENSPEICHERN
           PERFORM  CHECK-USR-COMMAND.
           IF       RET-CODE = SPACES
                    PERFORM EXECUTE-USR-COMMAND
           ELSE
                    MOVE "SEU0013" TO MSG-ID
                    PERFORM SENDE-NACHRICHT
                    SET NO-UPDATE  TO TRUE
           END-IF.

     ‚* VERARBEITE FUNKTIONSTASTEN F7 (SPLIT/JOIN) UND F8 (SHIFT)
           IF       HDR-MODE = "U"
                    PERFORM VERARBEITE-PFKEYS
           ELSE
                    IF  (   HDR-PFKEY = "7"
                         OR HDR-PFKEY = "8" )
                         MOVE "SEU0004" TO MSG-ID
                         PERFORM SENDE-NACHRICHT
                         SET NO-UPDATE TO TRUE
                    END-IF
           END-IF.

     ‚* MITTEILUNG AN SEU, DASS UPDATE ERFOLGT IST
           PERFORM  UPDATE-HEADER.

     ‚* ABSCHLUSS-MELDUNG AUSGEBEN
           IF       EXMSGID NOT = SPACES
                    MOVE    EXMSGID TO MSG-ID
                    PERFORM SENDE-NACHRICHT
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT-SIMPLE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN DES BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-USR-COMMAND SECTION.
       EXC-USC-00.

           INITIALIZE COMMAND-SAVE.
           MOVE     ZEROES TO SWBLK.
           INITIALIZE WRK-FELD1.
           INITIALIZE WRK-FELD2.
           INITIALIZE WRK-FELD3.
           INITIALIZE WRK-FELD5.
           INITIALIZE WRK-FELD7.
           INITIALIZE WRK-FELD8.
           INITIALIZE WRK-FELD10.
           INITIALIZE WRK-FELD12.
           INITIALIZE WRK-FELD50.
           INITIALIZE WRK-FELD50.
           INITIALIZE CHGID.
           INITIALIZE CHGMRK.
           INITIALIZE PHYFILE.
           INITIALIZE PHYFILE2.
           INITIALIZE RCDNAME.
           INITIALIZE CONT-CHR.
           INITIALIZE BOXPAR1.
           INITIALIZE BOXPAR2.
           INITIALIZE BOXL.
           INITIALIZE BOXR.

     ‚* JETZT NOCH EINMAL JEDE ZEILE VERARBEITEN WO WIR EINEN
     ‚* BEFEHL/BLOCKBEFEHL EINGETRAGEN HABEN UND DEN BEFEHL AUSFÜHREN
           PERFORM  VARYING ACT-LINE FROM 1 BY 1
                    UNTIL ACT-LINE > END-LINE
                    PERFORM GET-NEXT-LINE
                    PERFORM GET-COMMAND
           END-PERFORM.

       EXC-USC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES TAG-BEFEHLS (T/TT)
     ‚*--------------------------------------------------------------
       EXECUTE-TAG SECTION.
       EXC-TAG-00.

     ‚* NUR IN PROGRAMMSOURCEN UND DDS-SOURCEN ERLAUBT
           IF       KZ  OF FT-FOUND NOT = "1"
            AND     KZ  OF FT-FOUND NOT = "2"
                    GO TO EXC-TAG-90
           END-IF.

     ‚* WENN MAN TTxxx EINGEGEBEN HAT, DEN ÜBERGEBENEN WERT NUN MERKEN
           IF       COMMAND-CODE      NOT = SPACE
            AND     COMMAND-CODE(1:2)     = "TT"
            AND     COMMAND-SAVE          = SPACES
                    MOVE COMMAND-CODE     TO COMMAND-SAVE
                    MOVE COMMAND-CODE(3:) TO TAG-VALUE
           END-IF.
           IF       COMMAND-CODE      NOT = SPACE
            AND     COMMAND-CODE(1:1)     = "T"
            AND     COMMAND-CODE(2:1) NOT = "T"
                    MOVE COMMAND-CODE(2:) TO TAG-VALUE
           END-IF.

     ‚* KENNZEICHNEN FÜR PROGRAMME UND DDS
           IF       COMMAND-CODE(1:2)     = "T-"
            OR      COMMAND-SAVE(1:3)     = "TT-"
                    MOVE SPACES    TO FINAL-TAG-VALUE
           ELSE
     ‚*             WENN NUR "T" EINGEGEBEN WURDE, DANN ENTWEDER DEN
     ‚*             GESPEICHERTEN WERT NEHMEN ODER AUS DER KD-VERSION
                    IF   TAG-VALUE = SPACES
                         MOVE    1 TO FORCE-TAG
                         PERFORM GET-TAGVALUE
                    END-IF
     ‚*             JETZT DEN RICHTIGEN WERT ZUSAMMENBAUEN
                    MOVE "##"      TO FINAL-TAG-VALUE(1:2)
                    MOVE TAG-VALUE TO FINAL-TAG-VALUE(3:)
     ‚*             NUR SPEICHERN DES TAG WENN AUCH GESETZT WERDEN
     ‚*             SOLL, DAMIT WIR DEN TAG NICHT MIT SPACES ÜBER-
     ‚*             SCHREIBEN WENN WIR T- VERWENDEN
                    IF   TAG-VALUE NOT = SPACES
                         MOVE    TAG-VALUE   TO LCLMRK OF LCLDATA
                         MOVE    HDR-LIBNAME TO LCLLIB OF LCLDATA
                         PERFORM UPDATE-ULC-DTAARA
                    END-IF
           END-IF.

     ‚* DEN BEFEHL IM SEU NACH DEM AUSFÜHREN WIEDER ENTFERNEN
           SET      CLEAR-COMMAND   TO TRUE.
           MOVE     FINAL-TAG-VALUE TO TXT-SRCLINE(1:5).
           PERFORM  CHANGE-LINE.

     ‚* WENN ALLES GEKLAPPT HAT, DANN MELDUNG AUSGEBEN
           IF       EXMSGID = SPACES
                    MOVE "SEU0017" TO EXMSGID
           END-IF.

       EXC-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES AT-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-AT SECTION.
       EXC-ATC-00.

     ‚* NUR IN BEKANNTEN SOURCETYPEN ERLAUBT
           IF       KZ  OF FT-FOUND NOT = "0"
            AND     KZ  OF FT-FOUND NOT = "1"
            AND     KZ  OF FT-FOUND NOT = "2"
                    MOVE "SEU0007"  TO EXMSGID
                    SET ERROR-FOUND TO TRUE
                    GO TO EXC-ATC-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* ERMITTELN DES HEX-WERTES FÜR DEN AT-BEFEHL
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-SAVE     = SPACE
                    MOVE COMMAND-CODE     TO COMMAND-SAVE
                    MOVE COMMAND-CODE(3:) TO WRK-FELD3
                    PERFORM GET-ATHEX-VALUE
           END-IF.

     ‚* WENN KEIN HEX-WERT GEFUNDEN WURDE, MUSS EIN FALSCHER
     ‚* AT-BEFEHL ÜBERGEBEN WORDEN SEIN. DANN FEHLERMELDUNG
     ‚* AUSGEBEN UND KEINE ÄNDERUNG AN DER SOURCE VORNEHMEN
           IF       AT-HEX OF AT-FOUND = "#"
                    SET ERROR-FOUND   TO TRUE
                    SET LEAVE-COMMAND TO TRUE
                    SET DONT-PROCESS  TO TRUE
                    PERFORM CHANGE-LINE
                    GO TO EXC-ATC-90
           END-IF.

     ‚* WENN EIN "-" IM BEFEHL IST, DANN SOLL DAS ATTRIBUT ENTFERNT
     ‚* WERDEN. BEI SONSTIGEN SOURCEN WIE PNLGRP (KZ = 0) KANN MAN DAS
     ‚* IMMER TUN. BEI CBLLE PROGRAMM MUSS DAS EINE KOMMENTARZEILE SEIN
           IF       COMMAND-SAVE(3:1)         = "-"
            AND     KZ  OF FT-FOUND           = "1"
            AND     TXT-SRCLINE(BEGCOL:1) NOT = "*"
                    MOVE "SEU0007" TO MSG-ID
                    PERFORM SENDE-NACHRICHT
                    SET ERROR-FOUND   TO TRUE
                    SET LEAVE-COMMAND TO TRUE
                    SET DONT-PROCESS  TO TRUE
                    PERFORM CHANGE-LINE
                    GO TO EXC-ATC-90
           END-IF.

     ‚* SOLL DIE FARBE GESETZT ODER ENTFERNT WERDEN?
           IF       COMMAND-SAVE(3:1) = "-"
                    PERFORM SET-ATTR-RMV
           ELSE
                    PERFORM SET-ATTR-ADD
           END-IF.
           PERFORM  CHANGE-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0005" TO EXMSGID
           END-IF.

       EXC-ATC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES DT-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-DT SECTION.
       EXC-DTX-00.

‚     * NUR BEI BEKANNTEN SOURCE-TYPEN ERLAUBT
           IF       KZ  OF FT-FOUND NOT = "0"
            AND     KZ  OF FT-FOUND NOT = "1"
            AND     KZ  OF FT-FOUND NOT = "2"
                    GO TO EXC-DTX-90
           END-IF.

‚     * ZEILE MIT BEFEHL BLEIBT UNVERÄNDERT
           SET      CLEAR-COMMAND TO TRUE.
           PERFORM  CHANGE-LINE.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           IF       KZ  OF FT-FOUND = "0"
            AND     (    TXT-SRCLINE(BEGCOL:2) = "/*"
                     OR  TXT-SRCLINE(BEGCOL:2) = "--")
                    ADD 2 TO BEGCOL
           END-IF.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           MOVE     WRKC TO WRKW.

     ‚* FINDE BEGINN DER DATEN
           MOVE     WRKW TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
                    GO TO EXC-DTX-90
           END-IF.
           IF       WRKC > WRKW
                    MOVE WRKC TO WRKW
           END-IF.

           INITIALIZE MSG-TXT, MSG-ID, TEXT-ID.
           MOVE     WRKW TO POS.
           SET      COND-NO TO TRUE.
           PERFORM  UNTIL POS > ( ENDCOL - 7)
                     OR   MSG-ID NOT = SPACES
                    INITIALIZE WRK-FELD8
     ‚*             IST ES EINE LFS-TEXTID AUS LGTEXTE ODER PROTEXTE?
     ‚*             SCHEMA: XXXnnnn
                    IF   TXT-SRCLINE(POS:3)     IS NOT NUMERIC
                     AND TXT-SRCLINE(POS + 3:4) IS NUMERIC
                         MOVE TXT-SRCLINE(POS:7) TO TEXT-ID
                    END-IF
     ‚*             ODER SOLL EINE MSGID AUS EINEM MSGF ANGEZEIGT
     ‚*             WERDEN? DANN IST DIE FORM ETWAS ANDERS
                    IF   TXT-SRCLINE(POS:6) = "MSGID("
                         MOVE TXT-SRCLINE(POS + 6:7) TO TEXT-ID
                    END-IF
     ‚*             NUN DIE ERMITTELTE TEXT-ID ÜBERPRÜFEN UND ALLE
     ‚*             SONDERZEICHEN ENTFERNEN
                    MOVE 1 TO WRKX
                    INITIALIZE WRKL
                    PERFORM UNTIL WRKX > LENGTH OF TEXT-ID
                             OR   TEXT-ID(WRKX:) = SPACES
                            IF   TEXT-ID(WRKX:1) < "A"
                                 MOVE SPACES TO TEXT-ID(WRKX:)
                            ELSE
                                 MOVE WRKX TO WRKL
                            END-IF
                            ADD 1 TO WRKX
                    END-PERFORM
     ‚*             AUFBEREITETE TEXT-ID ÜBERNEHMEN UND GGF. DAS MDE
     ‚*             KENNZEICHEN ERGÄNZEN
                    MOVE TEXT-ID TO MSG-ID
                    IF   WRKL = 3
                         MOVE "MDE0"    TO MSG-ID
                         MOVE TEXT-ID   TO MSG-ID(5:)
                    END-IF
                    ADD 1 TO POS
           END-PERFORM.

     ‚* BEI EINIGEN TEXTEN KÖNNEN WIR VERSUCHEN DEN TEXT AUS EINEM
     ‚* MSGF ZU LESEN
           IF       MSG-ID NOT = SPACES
            OR      TXT-SRCLINE(POS:6) = "MSGID("
     ‚*             DIE TEXTE AUS DER PROLIB LIEGEN IN DER PROMSGF.
     ‚*             ALLE ANDEREN LIEGEN IN DER LFSMSGF
                    IF   HDR-LIBNAME(1:3) = "PRO"
                         MOVE "PROMSGF" TO MSG-FILE
                    ELSE
                         MOVE "LFSMSGF" TO MSG-FILE
                    END-IF
     ‚*             EINIGE ID'S SIND IN BESONDEREN DATEIEN VORHANDEN
                    EVALUATE MSG-ID(1:3)
                        WHEN "SEU" MOVE "ULCMSGF" TO MSG-FILE
                        WHEN "CPF" MOVE "QCPFMSG" TO MSG-FILE
                    END-EVALUATE
     ‚*             JETZT DIE MELDUNG AUSLESEN
                    PERFORM GET-MESSAGE-FROM-MSGF
                    IF   RM-MSGDTA(1:128) NOT = SPACES
                         SET COND-YES TO TRUE
                         MOVE MSG-ID           TO MSG-TXT(1:7)
                         MOVE ": "             TO MSG-TXT(8:2)
                         MOVE RM-MSGDTA(1:128) TO MSG-TXT(10:)
                         MOVE "SEU0101"        TO MSG-ID
                         PERFORM SHOW-PGMTEXT
                    END-IF
           END-IF.

     ‚* KEIN TEXT GEFUNDEN
           IF       COND-NO
                    MOVE "SEU0020" TO MSG-ID
                    PERFORM SENDE-NACHRICHT
           END-IF.

       EXC-DTX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* UPDATE THE USER SPACE HEADER AREA
     ‚* TELLING SEU THAT THE UPDATE IS COMPLETE.
     ‚*--------------------------------------------------------------
       UPDATE-HEADER SECTION.
       UPD-HDR-00.

           IF       ERROR-FOUND
                    MOVE "1"               TO HDR-RTNCODE
                    MOVE "B"               TO HDR-MODE
           ELSE
                    MOVE "0"               TO HDR-RTNCODE
                    MOVE "U"               TO HDR-MODE
           END-IF.
           MOVE     +1                     TO USRSPC-OFFSET.
           MOVE     LENGTH OF HEADER-SPACE TO USRSPC-SIZE.
           MOVE     "0"                    TO USRSPC-FORCE-CHANGE.
           CALL     "QUSCHGUS" USING USRSPC-NAME
                                     USRSPC-OFFSET
                                     USRSPC-SIZE
                                     HEADER-SPACE
                                     USRSPC-FORCE-CHANGE
                                     QUS-EC
           END-CALL.

       UPD-HDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* CHANGE THE LINE.
     ‚*--------------------------------------------------------------
       CHANGE-LINE SECTION.
       CHG-LIN-00.

           IF       CLEAR-COMMAND
                    MOVE SPACE  TO TXT-LINECMD
           END-IF.

     ‚* CHANGE QSUSPC.
           COMPUTE  USRSPC-SIZE = HDR-RCDLEN
                                + LINEPRFX-LEN
           END-COMPUTE.

           MOVE     "0" TO USRSPC-FORCE-CHANGE.

           CALL     "QUSCHGUS" USING USRSPC-NAME
                                     USRSPC-OFFSET
                                     USRSPC-SIZE
                                     TEXT-SPACE
                                     USRSPC-FORCE-CHANGE
                                     QUS-EC
           END-CALL.

       CHG-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INSERT A NEW LINE
     ‚*--------------------------------------------------------------
       INSERT-LINE SECTION.
       INS-LIN-00.

           MOVE     SPACE TO TXT-LINECMD.
           INITIALIZE TEXT-SPACE.

     ‚* CHANGE QSUSPC.
           COMPUTE  USRSPC-SIZE = HDR-RCDLEN
                                + LINEPRFX-LEN
           END-COMPUTE.

           COMPUTE  USRSPC-OFFSET = LENGTH OF HEADER-SPACE + 1
                                  + ( HDR-RCDSOUT * USRSPC-SIZE)
           END-COMPUTE.
           ADD      1 TO HDR-RCDSOUT.
           MOVE     NEU-SRCLINE TO TXT-SRCLINE  OF TEXT-SPACE.
           MOVE     ZEROES TO HDR-SEQINS.

           MOVE     "0" TO USRSPC-FORCE-CHANGE.

           CALL     "QUSCHGUS" USING USRSPC-NAME
                                     USRSPC-OFFSET
                                     USRSPC-SIZE
                                     TEXT-SPACE
                                     USRSPC-FORCE-CHANGE
                                     QUS-EC
           END-CALL.

       INS-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES KZ-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-KZ SECTION.
       EXC-KZU-00.

‚     * ZEILE MIT BEFEHL BLEIBT UNVERÄNDERT
           SET      CLEAR-COMMAND TO TRUE.
           PERFORM  CHANGE-LINE.

‚     * HINZUFÜGEN EINER NEUEN ZEILE
           INITIALIZE NEU-SRCLINE.

     ‚* FÜLLZEICHEN
           MOVE     COMMAND-CODE(3:) TO WRK-FELD1.

     ‚* LEERE KOMMENTAR-ZEILE FÜR PROGRAMME UND DDS
           IF       KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    MOVE "HI" TO WRK-FELD3
                    PERFORM GET-ATHEX-VALUE
                    MOVE AT-HEX OF AT-FOUND TO NEU-SRCLINE(6:1)
                    MOVE "*" TO NEU-SRCLINE(BEGCOL:1)
                    IF   WRK-FELD1 NOT = SPACE
                         MOVE "*" TO NEU-SRCLINE(ENDCOL:1)
                    END-IF
                    IF   KZ  OF FT-FOUND = "2"
                         MOVE "A" TO NEU-SRCLINE(6:1)
                    END-IF
                    INSPECT NEU-SRCLINE(BEGCOL:ENDCOL - BEGCOL + 1)
                            REPLACING ALL SPACES BY WRK-FELD1
                    INITIALIZE CHGID
                    PERFORM GET-TAGVALUE
                    IF   CHGMRK NOT = SPACES
                         MOVE CHGMRK TO NEU-SRCLINE(1:5)
                    END-IF
           END-IF.

           IF       KZ  OF FT-FOUND = "0"
            AND     SQL OF FT-FOUND = SPACE
                    MOVE "/*" TO NEU-SRCLINE(BEGCOL:2)
                    MOVE "*/" TO NEU-SRCLINE(ENDCOL - 1:2)
                    INSPECT NEU-SRCLINE(BEGCOL + 2:
                            ENDCOL - 2 - (BEGCOL + 2) + 1)
                            REPLACING ALL SPACES BY WRK-FELD1
           END-IF.

           IF       KZ  OF FT-FOUND = "0"
            AND     SQL OF FT-FOUND = "S"
                    MOVE "--" TO NEU-SRCLINE(BEGCOL:2)
                    MOVE "--" TO NEU-SRCLINE(ENDCOL - 1:2)
                    INSPECT NEU-SRCLINE(BEGCOL + 2:
                            ENDCOL - 2 - (BEGCOL + 2) + 1)
                            REPLACING ALL SPACES BY WRK-FELD1
           END-IF.

           MOVE     1 TO EXTNO.
           PERFORM  EXTEND-USERSPACE.

           PERFORM  INSERT-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0012" TO EXMSGID
           END-IF.

       EXC-KZU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES ISS-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-ISS SECTION.
       EXC-ISS-00.

‚     * ERMITTLE TYP DES MEMBERS (NUR COBOL-PROGRAMME)
           IF       KZ  OF FT-FOUND = "1"
            AND     PGM OF FT-FOUND = "C"
                    CONTINUE
           ELSE
                    GO TO EXC-ISS-90
           END-IF.

‚     * ZEILE MIT BEFEHL BLEIBT UNVERÄNDERT
           SET      CLEAR-COMMAND TO TRUE.
           PERFORM  CHANGE-LINE.

‚     * HINZUFÜGEN VON NEUEN ZEILEN
           INITIALIZE NEU-SRCLINE.
           MOVE     "HI" TO WRK-FELD3.
           PERFORM  GET-ATHEX-VALUE.
           INITIALIZE CHGID.
           PERFORM  GET-TAGVALUE.
           MOVE     ISS-ANZAHL TO EXTNO.
           PERFORM  EXTEND-USERSPACE.

     ‚* ZEILEN AUS PROGRAMM-INTERNER TABELLE
           PERFORM  VARYING ISSX FROM 1 BY 1
                    UNTIL ISSX > ISS-ANZAHL
                    INITIALIZE NEU-SRCLINE
                    MOVE ISS-ZEILE OF ISS-EINTRAG(ISSX) TO
                         NEU-SRCLINE(BEGCOL:)
                    MOVE CHGMRK TO NEU-SRCLINE(1:5)
                    IF   NEU-SRCLINE(BEGCOL:1) = "*"
                         MOVE AT-HEX OF AT-FOUND TO
                              NEU-SRCLINE(6:1)
                    END-IF
                    IF   NEU-SRCLINE(BEGCOL:1) = "*"
                     AND NEU-SRCLINE(BEGCOL + 1:1) = "-"
                         COMPUTE WRKL = ENDCOL - BEGCOL + 1
                         INSPECT NEU-SRCLINE(BEGCOL: WRKL )
                                 REPLACING ALL SPACES BY "-"
                    END-IF
                    PERFORM INSERT-LINE
           END-PERFORM.

           IF       EXMSGID = SPACES
                    MOVE "SEU0012" TO EXMSGID
           END-IF.

       EXC-ISS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES ISK-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-ISK SECTION.
       EXC-ISK-00.

‚     * ERMITTLE TYP DES MEMBERS (NUR COBOL-PROGRAMME)
           IF       KZ  OF FT-FOUND = "1"
            AND     PGM OF FT-FOUND = "C"
                    CONTINUE
           ELSE
                    GO TO EXC-ISK-90
           END-IF.

     ‚* HINZUFÜGEN VON NEUEN ZEILEN
           INITIALIZE NEU-SRCLINE.
           MOVE     "HI" TO WRK-FELD3.
           PERFORM  GET-ATHEX-VALUE.
           INITIALIZE CHGID.
           PERFORM  GET-TAGVALUE.
           MOVE     COMMAND-CODE(4:) TO WRK-FELD3.

     ‚* ERMITTLE DATABASE-NAMEN (=1.WORD DER AKTUELLEN ZEILE)
           INITIALIZE PHYFILE.
           IF       BEGCOL > 1
            AND     TXT-SRCLINE(BEGCOL - 1:1) = SPACE
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE ZERO TO WRKC
           END-IF.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZEROES
            OR      WRKC > (ENDCOL - 2 + 1)
                    GO TO EXC-ISK-90
           END-IF.
           MOVE     WRKC TO WRKK.
           IF       WRKK > ZEROES
                    MOVE 1 TO WRKL
                    PERFORM SUCHE-LEERSTELLE
                    COMPUTE WRKL = WRKC - WRKK
                    IF  WRKL > ZEROES
                        MOVE TXT-SRCLINE(WRKK:WRKL) TO PHYFILE
                    END-IF
           END-IF.
           IF       PHYFILE = SPACES
                    GO TO EXC-ISK-90
           END-IF.

     ‚* ERMITTLE KEY-FELDER
           INITIALIZE EXC-PARAMS.
           MOVE     "DSPKEY"                    TO EXC-COMMAND.
           MOVE     PHYFILE                     TO EXC-FILE.
           MOVE     PHYFILE                     TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO EXC-ISK-80
           END-IF.

           OPEN     INPUT QAFDACCP-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISK-80
           END-IF.
           READ     QAFDACCP-DATP NEXT.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISK-80
           END-IF.

     ‚* ERMITTLE SATZ-BESCHREIBUNG
           INITIALIZE EXC-PARAMS.
           MOVE     "DSPRCD"                    TO EXC-COMMAND.
           MOVE     PHYFILE                     TO EXC-FILE.
           MOVE     PHYFILE                     TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO EXC-ISK-80
           END-IF.

           OPEN     INPUT QAFDRFMT-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISK-80
           END-IF.
           READ     QAFDRFMT-DATP NEXT.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISK-80
           END-IF.
           IF       APFATR  OF QAFDACCP-RECP(1:2) = "PF"
                    MOVE "P" TO WRK-FELD3
                    MOVE RFNAME OF QAFDRFMT-RECP TO RCDNAME
                    MOVE APFILE OF QAFDACCP-RECP TO PHYFILE
           ELSE
                    MOVE APBOLF OF QAFDACCP-RECP TO RCDNAME
                    MOVE APBOF  OF QAFDACCP-RECP TO PHYFILE
                    IF   WRK-FELD3 = SPACES
                         MOVE "L" TO WRK-FELD3
                    END-IF
           END-IF.
           MOVE     APNKYF OF QAFDACCP-RECP TO EXTNO.
           PERFORM  EXTEND-USERSPACE.
           CLOSE    QAFDACCP-DATP.

     ‚* SPEICHERE FELD-DEFINITIONEN
           INITIALIZE FLD-TABELLE.
           PERFORM  FLD-EINLESEN.

‚     * AKTUELLE ZEILE MIT INITIALIZE RECORD FÜLLEN
           SET      CLEAR-COMMAND TO TRUE.
           INITIALIZE TXT-SRCLINE.
           MOVE     CHGMRK             TO TXT-SRCLINE(1:5).
           MOVE     12 TO WRKC.
           STRING   "INITIALIZE "              DELIMITED BY SIZE
                    RCDNAME                    DELIMITED BY SPACE
                    " OF "                     DELIMITED BY SIZE
                    PHYFILE                    DELIMITED BY SPACE
                    "-REC"                     DELIMITED BY SIZE
                    WRK-FELD3                  DELIMITED BY SPACE
                    "."                        DELIMITED BY SIZE
                    INTO TXT-SRCLINE           POINTER WRKC
           END-STRING.
           PERFORM  CHANGE-LINE.

‚     * MOVE-STATEMENT FÜR JEDES SCHLÜSSELFELD ERSTELLEN
           OPEN     INPUT QAFDACCP-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISK-80
           END-IF.
           INITIALIZE WRKX.
           INITIALIZE WRKK.

           PERFORM  UNTIL FILE-STATUS NOT = ZEROES
                    READ QAFDACCP-DATP NEXT
                    IF   FILE-STATUS = ZEROES
     ‚*                  SUCHE FELD IN FLD-TABELLE
                         MOVE APKEYF OF QAFDACCP-RECP TO WRK-FELD10
                         PERFORM SUCHE-FLDDEF
     ‚*                  HINZUFÜGEN DER ZEILE IN USERSPACE
                         IF   FLDX > ZEROES
                              PERFORM MOVE-KEYFLD
                              PERFORM INSERT-LINE
                         END-IF
                    END-IF
           END-PERFORM.

‚     * LETZTE ZEILE MIT START DATEI FÜLLEN
           SET      CLEAR-COMMAND TO TRUE.
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           MOVE     "START"            TO NEU-SRCLINE(12:8).
           MOVE     21 TO WRKC.
           STRING   PHYFILE                    DELIMITED BY SPACE
                    "-DAT"                     DELIMITED BY SIZE
                    WRK-FELD3                  DELIMITED BY SPACE
                    " KEY NOT < EXTERNALLY-DESCRIBED-KEY."
                                               DELIMITED BY SIZE
                    INTO NEU-SRCLINE           POINTER WRKC
           END-STRING.
           PERFORM  INSERT-LINE.
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           MOVE     "IF"               TO NEU-SRCLINE(12:8).
           MOVE     "FILE-STATUS NOT = ZEROES"
                                       TO NEU-SRCLINE(21:30).
           PERFORM  INSERT-LINE.
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           MOVE     "GO TO XXX-YYY-90" TO NEU-SRCLINE(21:30).
           PERFORM  INSERT-LINE.
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           MOVE     "END-IF."          TO NEU-SRCLINE(12:8).
           PERFORM  INSERT-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0012" TO EXMSGID
           END-IF.

     ‚* FREIGABE DER OVERRIDES
       EXC-ISK-80.
           CLOSE    QAFDACCP-DATP.
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QAFDACCP"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.
           CLOSE    QAFDRFMT-DATP.
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QAFDRFMT"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.

       EXC-ISK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE-ZEILE FÜR MOVE-BEFEHL SCHLÜSSEL-FELD ERSTELLEN
     ‚*--------------------------------------------------------------
       MOVE-KEYFLD SECTION.
       MOV-KEY-00.

     ‚* AUSRICHTUNG NACH DEM LÄNGSTEN FELDNAMEN
           INITIALIZE WRKL.
           INSPECT  WHFLDI OF FLD-EINTRAG(FLDX)
                    TALLYING WRKL FOR CHARACTERS BEFORE INITIAL SPACE.
           IF       WRKL > WRKK
                    MOVE WRKL TO WRKK
           END-IF.

     ‚* ZEROES/SPACES ODER 999999/HI-VALUES
           INITIALIZE WRK-FELD50.
           MOVE     WHFLDD OF FLD-EINTRAG(FLDX) TO WRKL.
           MOVE     WHFLDP OF FLD-EINTRAG(FLDX) TO WRKX.
           EVALUATE WHFLDT OF FLD-EINTRAG(FLDX) ALSO
                    APKSEQ OF QAFDACCP-RECP     ALSO TRUE
                    WHEN "A" ALSO "A" ALSO WRKK > ZERO
                         MOVE "SPACES" TO WRK-FELD50
                    WHEN "S" ALSO "A" ALSO WRKK > ZERO
                    WHEN "P" ALSO "A" ALSO WRKK > ZERO
                    WHEN "F" ALSO "A" ALSO WRKK > ZERO
                    WHEN "B" ALSO "A" ALSO WRKK > ZERO
                         MOVE "ZEROES" TO WRK-FELD50
                    WHEN "A" ALSO "D" ALSO WRKK > ZERO
                         MOVE "HIGH-VALUES" TO WRK-FELD50
                    WHEN "S" ALSO "D" ALSO WRKX = ZERO
                    WHEN "P" ALSO "D" ALSO WRKX = ZERO
                    WHEN "F" ALSO "D" ALSO WRKX = ZERO
                    WHEN "B" ALSO "D" ALSO WRKX = ZERO
                         MOVE ALL9(1:WRKL) TO WRK-FELD50
                    WHEN "S" ALSO "D" ALSO WRKX > ZERO
                    WHEN "P" ALSO "D" ALSO WRKX > ZERO
                    WHEN "F" ALSO "D" ALSO WRKX > ZERO
                    WHEN "B" ALSO "D" ALSO WRKX > ZERO
                         STRING ALL9(1:WRKL - WRKX) ","
                                ALL9(1:WRKX)  DELIMITED BY SIZE
                                INTO WRK-FELD50
                    WHEN OTHER
                         STRING "<Typ:"
                                WHFLDT OF FLD-EINTRAG(FLDX)
                                ">"           DELIMITED BY SIZE
                                INTO WRK-FELD50
           END-EVALUATE.

     ‚* FÜLLE ZEILE MIT DEFINITIONEN
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           MOVE     "MOVE"             TO NEU-SRCLINE(12:4).
           MOVE     WRK-FELD50         TO NEU-SRCLINE(21:).

           INITIALIZE WRK-FELD50.
           STRING   "TO "                       DELIMITED BY SIZE
                    WHFLDI OF FLD-EINTRAG(FLDX) DELIMITED BY SPACE
                    INTO WRK-FELD50
           END-STRING.
           COMPUTE  WRKC = 3 + WRKK + 1.
           STRING   " OF "                     DELIMITED BY SIZE
                    PHYFILE                    DELIMITED BY SPACE
                    "-REC"                     DELIMITED BY SIZE
                    WRK-FELD3                  DELIMITED BY SPACE
                    "."                        DELIMITED BY SIZE
                    INTO WRK-FELD50            POINTER WRKC
           END-STRING.
           COMPUTE  WRKX = ENDCOL - (WRKC - 1) + 1.
           MOVE     WRK-FELD50(1:WRKC) TO NEU-SRCLINE(WRKX:).

       MOV-KEY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES ISC-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-ISC SECTION.
       EXC-ISC-00.

‚     * ERMITTLE TYP DES MEMBERS (NUR COBOL-PROGRAMME)
           IF       KZ  OF FT-FOUND = "1"
            AND     PGM OF FT-FOUND = "C"
                    CONTINUE
           ELSE
                    GO TO EXC-ISC-90
           END-IF.

     ‚* HINZUFÜGEN VON NEUEN ZEILEN
           INITIALIZE NEU-SRCLINE.
           MOVE     "HI" TO WRK-FELD3.
           PERFORM  GET-ATHEX-VALUE.
           INITIALIZE CHGID.
           PERFORM  GET-TAGVALUE.
           MOVE     COMMAND-CODE(4:) TO WRK-FELD3.

     ‚* ERMITTLE DATABASE-NAMEN (=1.WORD DER AKTUELLEN ZEILE)
           INITIALIZE PHYFILE.
           IF       BEGCOL > 1
            AND     TXT-SRCLINE(BEGCOL - 1:1) = SPACE
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE ZERO TO WRKC
           END-IF.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZEROES
            OR      WRKC > (ENDCOL - 2 + 1)
                    GO TO EXC-ISC-90
           END-IF.
           MOVE     WRKC TO WRKK.
           IF       WRKK > ZEROES
                    MOVE 1 TO WRKL
                    PERFORM SUCHE-LEERSTELLE
                    COMPUTE WRKL = WRKC - WRKK
                    IF  WRKL > ZEROES
                        MOVE TXT-SRCLINE(WRKK:WRKL) TO PHYFILE
                    END-IF
           END-IF.
           IF       PHYFILE = SPACES
                    GO TO EXC-ISC-90
           END-IF.

     ‚* ERMITTLE KEY-FELDER
           INITIALIZE EXC-PARAMS.
           MOVE     "DSPKEY"                    TO EXC-COMMAND.
           MOVE     PHYFILE                     TO EXC-FILE.
           MOVE     PHYFILE                     TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO EXC-ISC-80
           END-IF.

           OPEN     INPUT QAFDACCP-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISC-80
           END-IF.
           READ     QAFDACCP-DATP NEXT.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISC-80
           END-IF.

     ‚* ERMITTLE SATZ-BESCHREIBUNG
           INITIALIZE EXC-PARAMS.
           MOVE     "DSPRCD"                    TO EXC-COMMAND.
           MOVE     PHYFILE                     TO EXC-FILE.
           MOVE     PHYFILE                     TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO EXC-ISC-80
           END-IF.

           OPEN     INPUT QAFDRFMT-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISC-80
           END-IF.
           READ     QAFDRFMT-DATP NEXT.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISC-80
           END-IF.
           IF       APFATR  OF QAFDACCP-RECP(1:2) = "PF"
                    MOVE "P" TO WRK-FELD3
                    MOVE RFNAME OF QAFDRFMT-RECP TO RCDNAME
                    MOVE APFILE OF QAFDACCP-RECP TO PHYFILE
           ELSE
                    MOVE APBOLF OF QAFDACCP-RECP TO RCDNAME
                    MOVE APBOF  OF QAFDACCP-RECP TO PHYFILE
                    IF   WRK-FELD3 = SPACES
                         MOVE "L" TO WRK-FELD3
                    END-IF
           END-IF.
           MOVE     APNKYF OF QAFDACCP-RECP TO EXTNO.
           PERFORM  EXTEND-USERSPACE.
           CLOSE    QAFDACCP-DATP.

     ‚* SPEICHERE FELD-DEFINITIONEN
           INITIALIZE FLD-TABELLE.
           PERFORM  FLD-EINLESEN.

‚     * AKTUELLE ZEILE MIT KOMMENTAR FÜLLEN
           SET      CLEAR-COMMAND TO TRUE.
           INITIALIZE TXT-SRCLINE.
           MOVE     CHGMRK             TO TXT-SRCLINE(1:5).
           MOVE     AT-HEX OF AT-FOUND TO TXT-SRCLINE(6:1).
           MOVE     "*"                TO TXT-SRCLINE(BEGCOL:1).
           MOVE     BEGCOL TO WRKC.
           STRING   "* STIMMT DER SCHLÜSSEL ?" DELIMITED BY SIZE
                    INTO TXT-SRCLINE           POINTER WRKC
           END-STRING.
           PERFORM  CHANGE-LINE.

‚     * IF/AND-STATEMENT FÜR JEDES SCHLÜSSELFELD ERSTELLEN
           OPEN     INPUT QAFDACCP-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISC-80
           END-IF.
           INITIALIZE WRKX.
           INITIALIZE WRKK.

           PERFORM  UNTIL FILE-STATUS NOT = ZEROES
                    READ QAFDACCP-DATP NEXT
                    IF   FILE-STATUS = ZEROES
     ‚*                  SUCHE FELD IN FLD-TABELLE
                         MOVE APKEYF OF QAFDACCP-RECP TO WRK-FELD10
                         PERFORM SUCHE-FLDDEF
     ‚*                  HINZUFÜGEN DER ZEILE IN USERSPACE
                         IF   FLDX > ZEROES
                              PERFORM COMP-KEYFLD
                              PERFORM INSERT-LINE
                         END-IF
                    END-IF
           END-PERFORM.

‚     * LETZTE ZEILE MIT START DATEI FÜLLEN
           SET      CLEAR-COMMAND TO TRUE.
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           MOVE     "GO TO XXX-YYY-90" TO NEU-SRCLINE(21:).
           PERFORM  INSERT-LINE.
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           MOVE     "END-IF."          TO NEU-SRCLINE(12:).
           PERFORM  INSERT-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0012" TO EXMSGID
           END-IF.

     ‚* FREIGABE DER OVERRIDES
       EXC-ISC-80.
           CLOSE    QAFDACCP-DATP.
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QAFDACCP"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.
           CLOSE    QAFDRFMT-DATP.
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QAFDRFMT"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.

       EXC-ISC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE-ZEILE FÜR IF/OR-BEFEHL SCHLÜSSEL-FELD ERSTELLEN
     ‚*--------------------------------------------------------------
       COMP-KEYFLD SECTION.
       CMP-KEY-00.

     ‚* AUSRICHTUNG NACH DEM LÄNGSTEN FELDNAMEN
           INITIALIZE WRKL.
           INSPECT  APKEYF OF QAFDACCP-RECP
                    TALLYING WRKL FOR CHARACTERS BEFORE INITIAL SPACE.
           IF       WRKL > WRKK
                    MOVE WRKL TO WRKK
           END-IF.

     ‚* FÜLLE ZEILE MIT DEFINITIONEN
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           IF       APKEYN OF QAFDACCP-RECP = 1
                    MOVE "IF"          TO NEU-SRCLINE(12:3)
           ELSE
                    MOVE " OR"         TO NEU-SRCLINE(12:3)
           END-IF.
           MOVE     APKEYF OF QAFDACCP-RECP     TO NEU-SRCLINE(21:).

           COMPUTE  WRKC = 21 + WRKK + 1.
           STRING   "OF "                       DELIMITED BY SIZE
                    PHYFILE                     DELIMITED BY SPACE
                    "-REC"                      DELIMITED BY SIZE
                    WRK-FELD3                   DELIMITED BY SPACE
                    " NOT = X"                  DELIMITED BY SIZE
                    WHFLDT OF FLD-EINTRAG(FLDX) DELIMITED BY SIZE
                    INTO NEU-SRCLINE            POINTER WRKC
           END-STRING.

       CMP-KEY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES ISF-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-ISF SECTION.
       EXC-ISF-00.

‚     * ERMITTLE TYP DES MEMBERS (NUR COBOL-PROGRAMME)
           IF       KZ  OF FT-FOUND = "1"
            AND     PGM OF FT-FOUND = "C"
                    CONTINUE
           ELSE
                    GO TO EXC-ISF-90
           END-IF.

     ‚* HINZUFÜGEN VON NEUEN ZEILEN
           INITIALIZE NEU-SRCLINE.
           MOVE     "HI" TO WRK-FELD3.
           PERFORM  GET-ATHEX-VALUE.
           INITIALIZE CHGID.
           PERFORM  GET-TAGVALUE.
           MOVE     COMMAND-CODE(4:) TO WRK-FELD3.

     ‚* ERMITTLE DATABASE-NAMEN (=1.WORD DER AKTUELLEN ZEILE)
           INITIALIZE PHYFILE.
           IF       BEGCOL > 1
            AND     TXT-SRCLINE(BEGCOL - 1:1) = SPACE
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE ZERO TO WRKC
           END-IF.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZEROES
            OR      WRKC > (ENDCOL - 2 + 1)
                    GO TO EXC-ISF-90
           END-IF.
           MOVE     WRKC TO WRKK.
           IF       WRKK > ZEROES
                    MOVE 1 TO WRKL
                    PERFORM SUCHE-LEERSTELLE
                    COMPUTE WRKL = WRKC - WRKK
                    IF  WRKL > ZEROES
                        MOVE TXT-SRCLINE(WRKK:WRKL) TO PHYFILE
                    END-IF
           END-IF.
           IF       PHYFILE = SPACES
                    GO TO EXC-ISF-90
           END-IF.

     ‚* ERMITTLE SATZ-BESCHREIBUNG
           INITIALIZE EXC-PARAMS.
           MOVE     "DSPRCD"                    TO EXC-COMMAND.
           MOVE     PHYFILE                     TO EXC-FILE.
           MOVE     PHYFILE                     TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO EXC-ISF-80
           END-IF.

           OPEN     INPUT QAFDRFMT-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISF-80
           END-IF.
           READ     QAFDRFMT-DATP NEXT.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-ISF-80
           END-IF.
           IF       RFFTYP OF QAFDRFMT-RECP = "P"
                    MOVE "P" TO WRK-FELD3
                    MOVE RFNAME OF QAFDRFMT-RECP TO RCDNAME
           ELSE
                    MOVE RFNAME OF QAFDRFMT-RECP TO RCDNAME
                    IF   WRK-FELD3 = SPACES
                         MOVE RFFTYP OF QAFDRFMT-RECP TO WRK-FELD3
                    END-IF
           END-IF.
           CLOSE    QAFDACCP-DATP.

     ‚* SPEICHERE FELD-DEFINITIONEN
           INITIALIZE FLD-TABELLE.
           PERFORM  FLD-EINLESEN.
           MOVE     FLD-ANZAHL TO EXTNO.
           PERFORM  EXTEND-USERSPACE.

‚     * AKTUELLE ZEILE MIT INITIALIZE RECORD FÜLLEN
           SET      CLEAR-COMMAND TO TRUE.
           INITIALIZE TXT-SRCLINE.
           MOVE     CHGMRK             TO TXT-SRCLINE(1:5).
           MOVE     12 TO WRKC.
           STRING   "INITIALIZE "              DELIMITED BY SIZE
                    RCDNAME                    DELIMITED BY SPACE
                    " OF "                     DELIMITED BY SIZE
                    PHYFILE                    DELIMITED BY SPACE
                    "-REC"                     DELIMITED BY SIZE
                    WRK-FELD3                  DELIMITED BY SPACE
                    "."                        DELIMITED BY SIZE
                    INTO TXT-SRCLINE           POINTER WRKC
           END-STRING.
           PERFORM  CHANGE-LINE.

     ‚* MOVE-STATEMENT FÜR JEDES FELD ERSTELLEN
           MOVE     1 TO FLDX.
           INITIALIZE WRKK.

           PERFORM  UNTIL FLDX > FLD-ANZAHL
     ‚*             HINZUFÜGEN DER ZEILE IN USERSPACE
                    PERFORM MOVE-DATFLD
                    PERFORM INSERT-LINE
                    ADD 1 TO FLDX
           END-PERFORM.

           IF       EXMSGID = SPACES
                    MOVE "SEU0012" TO EXMSGID
           END-IF.

     ‚* FREIGABE DER OVERRIDES
       EXC-ISF-80.
           CLOSE    QAFDRFMT-DATP.
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QAFDRFMT"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.

       EXC-ISF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE-ZEILE FÜR MOVE-BEFEHL DATEN-FELD ERSTELLEN
     ‚*--------------------------------------------------------------
       MOVE-DATFLD SECTION.
       MOV-DAT-00.

     ‚* AUSRICHTUNG NACH DEM LÄNGSTEN FELDNAMEN
           INITIALIZE WRKL.
           INSPECT  WHFLDI OF FLD-EINTRAG(FLDX)
                    TALLYING WRKL FOR CHARACTERS BEFORE INITIAL SPACE.
           IF       WRKL > WRKK
                    MOVE WRKL TO WRKK
           END-IF.

     ‚* ZEROES/SPACES
           INITIALIZE WRK-FELD50.
           MOVE     WHFLDD OF FLD-EINTRAG(FLDX) TO WRKL.
           MOVE     WHFLDP OF FLD-EINTRAG(FLDX) TO WRKX.
           EVALUATE WHFLDT OF FLD-EINTRAG(FLDX) ALSO TRUE
                    WHEN "A"          ALSO WRKK > ZERO
                         MOVE "SPACES" TO WRK-FELD50
                    WHEN "S"          ALSO WRKK > ZERO
                    WHEN "P"          ALSO WRKK > ZERO
                    WHEN "F"          ALSO WRKK > ZERO
                    WHEN "B"          ALSO WRKK > ZERO
                         MOVE "ZEROES" TO WRK-FELD50
                    WHEN OTHER
                         STRING "<Typ:"
                                WHFLDT OF FLD-EINTRAG(FLDX)
                                ">"           DELIMITED BY SIZE
                                INTO WRK-FELD50
           END-EVALUATE.

     ‚* FÜLLE ZEILE MIT DEFINITIONEN
           INITIALIZE NEU-SRCLINE.
           MOVE     CHGMRK             TO NEU-SRCLINE(1:5).
           MOVE     "MOVE"             TO NEU-SRCLINE(12:4).
           MOVE     WRK-FELD50         TO NEU-SRCLINE(21:).

           INITIALIZE WRK-FELD50.
           STRING   "TO "                       DELIMITED BY SIZE
                    WHFLDI OF FLD-EINTRAG(FLDX) DELIMITED BY SPACE
                    INTO WRK-FELD50
           END-STRING.
           COMPUTE  WRKC = 3 + WRKK + 1.
           STRING   " OF "                     DELIMITED BY SIZE
                    PHYFILE                    DELIMITED BY SPACE
                    "-REC"                     DELIMITED BY SIZE
                    WRK-FELD3                  DELIMITED BY SPACE
                    "."                        DELIMITED BY SIZE
                    INTO WRK-FELD50            POINTER WRKC
           END-STRING.
           COMPUTE  WRKX = ENDCOL - (WRKC - 1) + 1.
           MOVE     WRK-FELD50(1:WRKC) TO NEU-SRCLINE(WRKX:).

       MOV-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES CKT-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-CKT SECTION.
       EXC-CKT-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-CKT-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* ERSTELLE ÄNDERUNGSMERKMAL
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:3) = "CKT"
            AND     COMMAND-SAVE     = SPACE
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE SPACES TO WRK-FELD2
                    MOVE COMMAND-CODE(4:) TO CHGID
                    PERFORM GET-TAGVALUE
                    INITIALIZE SWBLK
           END-IF.

           IF       SWBLK = "1"
                    GO TO EXC-CKT-80
           END-IF.

     ‚* ERWEITERE USER-SPACE.
           COMPUTE  EXTNO = (HDR-RCDSIN - 1).
           PERFORM  EXTEND-USERSPACE.

     ‚* KOPIERE ZEILEN.
           PERFORM  VARYING RECNO FROM 1 BY 1
                    UNTIL RECNO > EXTNO
                    PERFORM GET-NEXT-LINE
                    INITIALIZE NEU-SRCLINE
                    MOVE TXT-SRCLINE TO NEU-SRCLINE
                    IF   (    KZ  OF FT-FOUND  = "1"
                          OR  KZ  OF FT-FOUND  = "2" )
                         IF   CHGMRK NOT = SPACES
                              MOVE CHGMRK TO NEU-SRCLINE(1:5)
                         END-IF
                    END-IF
                    PERFORM INSERT-LINE
           END-PERFORM.
           MOVE     "1" TO SWBLK.

     ‚* LESE AKTUELLEN SATZ
           MOVE     IDX TO RECNO.
           PERFORM  GET-NEXT-LINE.

       EXC-CKT-80.
           IF       EXMSGID = SPACES
                    MOVE "SEU0018" TO EXMSGID
           END-IF.

     ‚* AUSKOMMENTIEREN, TAG UND ZURÜCKSCHREIBEN
           PERFORM  EXECUTE-KK.

       EXC-CKT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES KOMMENTAR-BEFEHLS (K/KK) UND KT
     ‚*--------------------------------------------------------------
       EXECUTE-KK SECTION.
       EXC-KOM-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-KOM-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* MERKE FÜLL-ZEICHEN
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "KK"
            AND     COMMAND-SAVE      = SPACE
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE(3:) TO WRK-FELD2
           END-IF.
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:1) = "K"
            AND     COMMAND-CODE(2:1) NOT = "K"
            AND     COMMAND-CODE(1:2) NOT = "KT"
            AND     COMMAND-SAVE      = SPACE
                    MOVE COMMAND-CODE(2:) TO WRK-FELD2
           END-IF.
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "KT"
            AND     COMMAND-SAVE      = SPACE
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE SPACES TO WRK-FELD2
                    MOVE COMMAND-CODE(3:) TO CHGID
           END-IF.

     ‚* DE-KOMMENTIEREN PROGRAMME UND DDS
           IF       (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
            AND     WRK-FELD2(1:1)       = "-"
            AND     TXT-SRCLINE(7:1)     = "*"
                    IF   COMMAND-SAVE(1:2) = "KT"
                         MOVE SPACES TO TXT-SRCLINE(1:5)
                    END-IF
                    PERFORM SET-ATTR-RMV
                    MOVE SPACES TO TXT-SRCLINE(7:1)
                    IF   KZ  OF FT-FOUND  = "2"
                     AND TXT-SRCLINE(6:1) = SPACE
                         MOVE "A"    TO TXT-SRCLINE(6:1)
                    END-IF
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    MOVE WRK-FELD2(2:1) TO WRK-FELD8
                    MOVE "*"            TO WRK-FELD8(2:1)
                    SUBTRACT 1 FROM WRKE
                    IF   WRKE > (BEGCOL + 2)
                     AND (   TXT-SRCLINE(WRKE:2) = "**"
                          OR TXT-SRCLINE(WRKE:2) = " *"
                          OR TXT-SRCLINE(WRKE:2) = WRK-FELD8(1:2) )
                         MOVE SPACES TO TXT-SRCLINE(WRKE:2)
                    END-IF
                    MOVE WRK-FELD2(2:1) TO WRK-FELD1
                    IF   WRK-FELD1 > SPACES
                         COMPUTE WRKL = ENDCOL - BEGCOL + 1
                         INSPECT TXT-SRCLINE(BEGCOL: WRKL )
                                 REPLACING ALL WRK-FELD1 BY SPACE
                    END-IF
                    GO TO EXC-KOM-80
           END-IF.

     ‚* DE-KOMMENTIEREN TEXT-DATEIEN
           IF       KZ  OF FT-FOUND = "0"
            AND     WRK-FELD2(1:1)  = "-"
                    MOVE BEGCOL TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC < BEGCOL
                     OR  WRKC > (ENDCOL - 2 + 1)
                         GO TO EXC-KOM-80
                    END-IF
                    PERFORM SET-ATTR-RMV
                    IF   TXT-SRCLINE(WRKC : 2) = "/*"
                         MOVE SPACES TO TXT-SRCLINE(WRKC:2)
                    END-IF
                    IF   TXT-SRCLINE(WRKC : 2) = "--"
                     AND SQL OF FT-FOUND    = "S"
                         MOVE SPACES TO TXT-SRCLINE(WRKC:2)
                    END-IF
                    IF   TXT-SRCLINE(WRKC : 1) = "*"
                         MOVE SPACES TO TXT-SRCLINE(WRKC:1)
                    END-IF
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > (BEGCOL + 2)
                     AND TXT-SRCLINE(WRKE: 1) = "*"
                         MOVE SPACES TO TXT-SRCLINE(WRKE:1)
                    END-IF
                    IF   WRKE > (BEGCOL + 2)
                     AND WRKE < (ENDCOL - 2 + 1)
                     AND TXT-SRCLINE(WRKE - 1: 2) = "*/"
                         SUBTRACT 1 FROM WRKE
                         MOVE SPACES TO TXT-SRCLINE(WRKE:2)
                    END-IF
                    MOVE WRK-FELD2(2:1) TO WRK-FELD1
                    IF   WRK-FELD1 > SPACES
                         COMPUTE WRKL = WRKE - WRKC + 1
                         INSPECT TXT-SRCLINE(WRKC: WRKL )
                                 REPLACING ALL WRK-FELD1 BY SPACE
                    END-IF
                    GO TO EXC-KOM-80
           END-IF.

     ‚* KENNZEICHNEN FÜR PROGRAMME UND DDS MIT KT ODER CKT
           IF       (    KZ  OF FT-FOUND  = "1"
                     OR  KZ  OF FT-FOUND  = "2" )
            AND     (    COMMAND-SAVE(1:2) = "KT"
                     OR  COMMAND-SAVE(1:3) = "CKT" )
                    PERFORM GET-TAGVALUE
                    IF   CHGMRK NOT = SPACES
                         MOVE CHGMRK TO TXT-SRCLINE(1:5)
                    END-IF
           END-IF.

     ‚* AUSKOMMENTIEREN PROGRAMME UND DDS
           IF       (    KZ  OF FT-FOUND  = "1"
                     OR  KZ  OF FT-FOUND  = "2" )
            AND     TXT-SRCLINE(BEGCOL:1) NOT = "*"
                    MOVE BEGCOL TO WRKC
                    MOVE 2 TO WRKL
                    PERFORM SUCHE-LEERSTELLE
                    MOVE WRKC TO WRKE
                    MOVE BEGCOL TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC < BEGCOL
                     OR  WRKC > ENDCOL
                         MOVE BEGCOL TO WRKC
                    END-IF
     ‚*             VERSCHIEBE, WENN KEIN PLATZ FÜR "*"
                    IF   WRKE > BEGCOL
                     AND WRKE < (ENDCOL - 2 + 1)
                         COMPUTE WRKL = WRKE - WRKC
                         MOVE TXT-SRCLINE TO NEU-SRCLINE
                         MOVE NEU-SRCLINE(WRKC    : WRKL) TO
                              TXT-SRCLINE(WRKC + 1: WRKL)
                    END-IF
     ‚*             "*" = KOMMENTAR
                    MOVE "*" TO TXT-SRCLINE(BEGCOL:1)
                    MOVE "HI" TO WRK-FELD3
                    PERFORM GET-ATHEX-VALUE
                    MOVE AT-HEX OF AT-FOUND TO TXT-SRCLINE(6:1)
                    MOVE "N"  TO WRK-FELD3
                    PERFORM GET-ATHEX-VALUE
                    MOVE AT-HEX OF AT-FOUND TO TXT-SRCLINE(8:1)
                    IF   KZ  OF FT-FOUND  = "2"
                         MOVE "A" TO TXT-SRCLINE(6:1)
                    END-IF
     ‚*             AUSFÜLLEN
                    IF   WRK-FELD2(1:1) = "+"
                     AND WRK-FELD2(2:1) > SPACE
                         MOVE WRK-FELD2(2:1) TO WRK-FELD1
                    ELSE
                         MOVE WRK-FELD2(1:1) TO WRK-FELD1
                    END-IF
                    IF   WRK-FELD1 > SPACES
                         COMPUTE WRKL = ENDCOL - BEGCOL + 1
                         INSPECT TXT-SRCLINE(BEGCOL: WRKL )
                                 REPLACING ALL SPACES BY WRK-FELD1
                    END-IF
                    GO TO EXC-KOM-80
           END-IF.
     ‚*    AUSFÜLLEN DER RECHTEN SEITE
           IF       (    KZ  OF FT-FOUND  = "1"
                     OR  KZ  OF FT-FOUND  = "2" )
            AND     TXT-SRCLINE(BEGCOL:1) = "*"
            AND     TXT-SRCLINE(ENDCOL:1) = SPACE
                    IF   WRK-FELD2(1:1) = "+"
                     AND WRK-FELD2(2:1) > SPACE
                         MOVE WRK-FELD2(2:1) TO WRK-FELD1
                    ELSE
                         MOVE WRK-FELD2(1:1) TO WRK-FELD1
                    END-IF
                    IF   WRK-FELD1 > SPACES
                         MOVE WRK-FELD1 TO
                              TXT-SRCLINE(ENDCOL: 1)
                    END-IF
                    GO TO EXC-KOM-80
           END-IF.

     ‚* AUSKOMMENTIEREN VON TEXT-QUELLEN
           IF       KZ  OF FT-FOUND  = "0"
                    MOVE BEGCOL TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC < BEGCOL
                     OR  WRKC > (ENDCOL - 2 + 1)
                         GO TO EXC-KOM-80
                    END-IF
                    IF   TXT-SRCLINE(WRKC : 2) = "/*"
                     AND SQL OF FT-FOUND    = " "
                         GO TO EXC-KOM-80
                    END-IF
                    IF   TXT-SRCLINE(WRKC : 2) = "--"
                     AND SQL OF FT-FOUND    = "S"
                         GO TO EXC-KOM-80
                    END-IF
                    MOVE 2 TO WRKL
                    MOVE BEGCOL TO WRKC
                    PERFORM SUCHE-LEERSTELLE
                    MOVE WRKC TO WRKE
                    MOVE BEGCOL TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC < BEGCOL
                     OR  WRKC > ENDCOL
                         MOVE BEGCOL TO WRKC
                    END-IF
     ‚*             VERSCHIEBE, WENN KEIN PLATZ FÜR /* /--
                    IF   WRKE > WRKC
                     AND WRKE < (ENDCOL - 3 + 1)
                         COMPUTE WRKL = WRKE - WRKC
                         MOVE TXT-SRCLINE TO NEU-SRCLINE
                         MOVE SPACES TO
                              TXT-SRCLINE(WRKC    : WRKL)
                         MOVE NEU-SRCLINE(WRKC    : WRKL) TO
                              TXT-SRCLINE(WRKC + 2: WRKL)
                    END-IF
     ‚*             /*    */ = KOMMENTAR
                    IF   SQL OF FT-FOUND    = " "
                     AND TXT-SRCLINE(BEGCOL:2) = SPACE
                         MOVE "/*" TO TXT-SRCLINE(BEGCOL: 2)
                         MOVE "*/" TO TXT-SRCLINE(ENDCOL - 1: 2)
                    END-IF
     ‚*             --       = KOMMENTAR
                    IF   SQL OF FT-FOUND    = "S"
                     AND TXT-SRCLINE(BEGCOL:2) = SPACE
                         MOVE "--" TO TXT-SRCLINE(BEGCOL: 2)
                    END-IF
                    GO TO EXC-KOM-80
           END-IF.

       EXC-KOM-80.
           PERFORM  CHANGE-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0016" TO EXMSGID
           END-IF.

       EXC-KOM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES APPEND-BEFEHLS (AP)
     ‚*--------------------------------------------------------------
       EXECUTE-AP SECTION.
       EXC-APP-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-APP-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* MERKE FÜLL-ZEICHEN
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "AP"
            AND     COMMAND-SAVE     = SPACE
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE(3:) TO WRK-FELD2
           END-IF.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:2) = "/*"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.
           IF       TXT-SRCLINE(BEGCOL:2) = "--"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.

     ‚* ZEICHEN ENTFERNEN
           IF       WRK-FELD2(1:1)       = "-"
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE < BEGCOL
                     OR  WRKE > ENDCOL
                         MOVE BEGCOL TO WRKE
                    END-IF
                    MOVE WRK-FELD2(2:1) TO WRK-FELD1
                    IF   WRK-FELD1 = SPACE
                         MOVE "." TO WRK-FELD1
                    END-IF
     ‚*             ENTFERNE ZEICHEN, WENN AN LETZTER POSITION
                    IF   TXT-SRCLINE(WRKE:1) = WRK-FELD1
                         MOVE SPACE TO TXT-SRCLINE(WRKE:1)
                    END-IF
                    GO TO EXC-APP-80
           END-IF.

     ‚* ERMITTLE FÜLL-ZEICHEN
           IF       WRK-FELD2(1:1) = SPACE
                    MOVE "." TO WRK-FELD1
           ELSE
                    MOVE WRK-FELD2(1:1) TO WRK-FELD1
           END-IF
           IF       WRK-FELD2(1:1) = "+"
            AND     WRK-FELD2(2:1) > SPACE
                    MOVE WRK-FELD2(2:1) TO WRK-FELD1
           END-IF

     ‚* ZEICHEN ANHÄNGEN
           IF       PGM OF FT-FOUND  = "R"
            AND     WRK-FELD1        = "."
                    CONTINUE
           ELSE
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE < BEGCOL
                         MOVE BEGCOL TO WRKE
                    END-IF
     ‚*             ZEICHEN AN LETZTE POSITION ANFÜGEN
                    IF   WRKE < ENDCOL
                         COMPUTE WRKX = WRKE + 1
                         MOVE WRK-FELD1 TO TXT-SRCLINE(WRKX:1)
                    ELSE
                         MOVE "SEU0011" TO EXMSGID
                    END-IF
                    GO TO EXC-APP-80
           END-IF.

       EXC-APP-80.
           PERFORM  CHANGE-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0021" TO EXMSGID
           END-IF.

       EXC-APP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES INSERT-CHAR-BEFEHLS (IC)
     ‚*--------------------------------------------------------------
       EXECUTE-IC SECTION.
       EXC-INS-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-INS-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* MERKE FÜLL-ZEICHEN
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "IC"
            AND     COMMAND-SAVE     = SPACE
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE(3:) TO WRK-FELD7
           END-IF.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:2) = "/*"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.
           IF       TXT-SRCLINE(BEGCOL:2) = "--"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           MOVE     WRKC TO BEGSAVE1.

     ‚* ERMITTLE LÄNGE DES EINZUFÜGENDEN TEXTES
           INITIALIZE WRKX.
           INSPECT  FUNCTION REVERSE(WRK-FELD7)
                    TALLYING WRKX FOR LEADING SPACES.
           COMPUTE  WRKL = LENGTH OF WRK-FELD7
                         - WRKX
           END-COMPUTE.
           IF       COMMAND-SAVE(1:3) = "IC-"
            OR      COMMAND-SAVE(1:3) = "IC+"
                    SUBTRACT 1 FROM WRKL
           END-IF.
           IF       WRKL < 1
                    MOVE 1 TO WRKL
           END-IF.
           MOVE     WRKL TO BEGSAVE2.

     ‚* SUCHE RAUM ZUM EINFÜGEN
           MOVE     ENDCOL TO WRKE.
           PERFORM  SUCHE-LETZTES-ZEICHEN.
           MOVE     BEGSAVE1 TO WRKC.
           COMPUTE  WRKL = BEGSAVE2 + 1.
           MOVE     SPACES TO WRK-FELD50.
           PERFORM  SUCHE-ZEICHEN.
           IF       WRKC > ENDCOL
            OR      WRKC = ZEROES
                    COMPUTE WRKC = ENDCOL + BEGSAVE2
           END-IF.
           MOVE     WRKC TO ENDSAVE1.

     ‚* ZEICHEN ENTFERNEN
           IF       WRK-FELD7(1:1)       = "-"
                    MOVE BEGSAVE1 TO WRKC
                    MOVE BEGSAVE2 TO WRKL
                    MOVE ENDSAVE1 TO WRKP
                    IF   WRKP > ENDCOL
                         COMPUTE WRKP = ENDCOL + 1
                    END-IF
                    COMPUTE WRKX = WRKP - (WRKC + WRKL)
     ‚*             ENTFERNE ZEICHEN BEI ÜBEREINSTIMMUNG
                    IF   TXT-SRCLINE(WRKC:WRKL) = WRK-FELD7(2:WRKL)
                         MOVE SPACE TO TXT-SRCLINE(WRKC:WRKL)
                         MOVE TXT-SRCLINE TO NEU-SRCLINE
                         IF  (WRKX > ZEROES)
                              MOVE SPACES TO
                                   TXT-SRCLINE(WRKC + WRKL:WRKX)
                              MOVE NEU-SRCLINE(WRKC + WRKL:WRKX) TO
                                   TXT-SRCLINE(WRKC       :WRKX)
                         END-IF
                    END-IF
                    GO TO EXC-INS-80
           END-IF.

     ‚* ERMITTLE FÜLL-ZEICHEN
           IF       WRK-FELD7(1:1) = "+"
                    MOVE 2 TO WRKP
           ELSE
                    MOVE 1 TO WRKP
           END-IF.
           MOVE     BEGSAVE2 TO WRKL.

     ‚* ZEICHEN EINFÜGEN
           MOVE     BEGSAVE1 TO WRKC.
           MOVE     ENDSAVE1 TO WRKX.
           IF       WRKX > ENDCOL
            AND     (WRKE + 1 + WRKL - 1) NOT > ENDCOL
                    COMPUTE WRKX = WRKE + 1
           END-IF.
           IF       (WRKX + WRKL - 1) > ENDCOL
                    MOVE "SEU0011" TO EXMSGID
                    GO TO EXC-INS-80
           END-IF.
           IF       (WRKX - WRKC) > ZEROES
                    MOVE TXT-SRCLINE TO NEU-SRCLINE
                    MOVE NEU-SRCLINE(WRKC       : WRKX - WRKC) TO
                         TXT-SRCLINE(WRKC + WRKL: WRKX - WRKC)
           END-IF.
           MOVE     WRK-FELD7(WRKP : WRKL) TO TXT-SRCLINE(WRKC: WRKL).

       EXC-INS-80.
           PERFORM  CHANGE-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0021" TO EXMSGID
           END-IF.

       EXC-INS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES CASE-BEFEHLS (LC/MC/UC)
     ‚*--------------------------------------------------------------
       EXECUTE-CASE SECTION.
       EXC-CAS-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-CAS-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* MERKE FÜLL-ZEICHEN
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-SAVE     = SPACE
            AND     (   COMMAND-CODE(1:3) = "LCC"
                     OR COMMAND-CODE(1:3) = "MCC"
                     OR COMMAND-CODE(1:3) = "UCC" )
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE(4:) TO WRK-FELD3
           END-IF.
           IF       COMMAND-SAVE = SPACE
            AND     (   COMMAND-CODE(1:2) = "LC"
                     OR COMMAND-CODE(1:2) = "MC"
                     OR COMMAND-CODE(1:2) = "UC" )
                    MOVE COMMAND-CODE(3:) TO WRK-FELD3
           END-IF.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:2) = "/*"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.
           IF       TXT-SRCLINE(BEGCOL:2) = "--"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           MOVE     WRKC TO WRKW.

     ‚* BIS ZEILEN-ENDE ODER NUR EIN WORT
           IF       WRK-FELD3(1:1) = "W"
                    MOVE WRKW TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC = ZERO
                         MOVE ENDCOL TO WRKC
                    END-IF
                    MOVE 1 TO WRKL
                    PERFORM SUCHE-WORTENDE
                    IF   WRKC = ZERO
                         COMPUTE WRKC = ENDCOL + 1
                    END-IF
                    COMPUTE WRKL = WRKC - WRKW
           ELSE
                    COMPUTE WRKC = ENDCOL + 1
                    COMPUTE WRKL = WRKC - WRKW
           END-IF.

     ‚* ZEICHEN UMSETZEN
           EVALUATE TRUE
                    WHEN COMMAND-CODE(1:2) = "LC"
                     OR  COMMAND-SAVE(1:3) = "LCC"
                         MOVE TXT-SRCLINE TO NEU-SRCLINE
                         INSPECT NEU-SRCLINE
                                 CONVERTING UPCHAR TO LOCHAR
                         MOVE NEU-SRCLINE(WRKW: WRKL) TO
                              TXT-SRCLINE(WRKW: WRKL)
                    WHEN COMMAND-CODE(1:2) = "MC"
                     OR  COMMAND-SAVE(1:3) = "MCC"
                         MOVE TXT-SRCLINE TO NEU-SRCLINE
                         INSPECT NEU-SRCLINE
                                 CONVERTING UPCHAR TO LOCHAR
                         MOVE NEU-SRCLINE(WRKW: WRKL) TO
                              TXT-SRCLINE(WRKW: WRKL)
                         PERFORM EXECUTE-MIXED
                    WHEN COMMAND-CODE(1:2) = "UC"
                     OR  COMMAND-SAVE(1:3) = "UCC"
                         MOVE TXT-SRCLINE TO NEU-SRCLINE
                         INSPECT NEU-SRCLINE
                                 CONVERTING LOCHAR TO UPCHAR
                         MOVE NEU-SRCLINE(WRKW: WRKL) TO
                              TXT-SRCLINE(WRKW: WRKL)
           END-EVALUATE.

           PERFORM  CHANGE-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0015" TO EXMSGID
           END-IF.

       EXC-CAS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MIXED CASE = 1.BUCHSTABE EINES WORTES IN GROSS-SCHRIFT
     ‚*--------------------------------------------------------------
       EXECUTE-MIXED SECTION.
       EXC-MIX-00.

           SET      UPPER-CASE TO TRUE.
           PERFORM  VARYING WRKX FROM WRKW BY 1
                    UNTIL WRKX > ( WRKW + WRKL)
                          MOVE TXT-SRCLINE(WRKX:1) TO WRK-FELD1
                          INITIALIZE WRKZ
                          INSPECT LOCHAR TALLYING WRKZ
                                  FOR ALL WRK-FELD1
                          IF   WRKZ = ZEROES
                               SET  UPPER-CASE TO TRUE
                          ELSE
                               IF   UPPER-CASE
                                    INSPECT WRK-FELD1
                                            CONVERTING LOCHAR TO UPCHAR
                                    MOVE WRK-FELD1
                                         TO TXT-SRCLINE(WRKX: 1)
                                    SET  LOWER-CASE TO TRUE
                               END-IF
                          END-IF
           END-PERFORM.

       EXC-MIX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES SHIFT-LEFT BEFEHLS (LS)
     ‚*--------------------------------------------------------------
       EXECUTE-LS SECTION.
       EXC-SLS-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-SLS-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* MERKE FÜLL-ZEICHEN
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-SAVE     = SPACE
            AND     COMMAND-CODE(1:2) = "LS"
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE(3:) TO WRK-FELD3
                    PERFORM  GET-NUMINP
           END-IF.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:2) = "/*"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.
           IF       TXT-SRCLINE(BEGCOL:2) = "--"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.

     ‚* FINDE BEGINN DER DATEN
           MOVE     BEGCOL TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
                    MOVE ENDCOL TO WRKC
           END-IF.
           MOVE     WRKC TO WRKW.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           IF       WRKC > WRKW
                    MOVE WRKC TO WRKW
           END-IF.

     ‚* ANZAHL AUS PREFIX-BEREICH
           IF       WRK-FELD5 IS NUMERIC
                    MOVE WRK-FELD5 TO WRKK
           ELSE
                    MOVE ZEROES TO WRKK
           END-IF.
           IF       WRKK = ZEROES
                    MOVE 1 TO WRKK
           END-IF.

     ‚* ERRECHNE VERSCHIEBUNG
           MOVE     ENDCOL TO WRKE.
           PERFORM  SUCHE-LETZTES-ZEICHEN.
           IF       WRKE < WRKW
                    MOVE WRKW TO WRKE
           END-IF.
           COMPUTE  WRKC = WRKW - WRKK.
           COMPUTE  WRKL = WRKE - WRKW + 1.
           IF       WRKC < BEGCOL
                    IF   EXMSGID = SPACES
                         MOVE "SEU0010" TO EXMSGID
                    END-IF
                    PERFORM CHANGE-LINE
                    GO TO EXC-SLS-90
           END-IF.

     ‚* VERSCHIEBE
           MOVE     TXT-SRCLINE TO NEU-SRCLINE.
           MOVE     SPACES      TO TXT-SRCLINE(WRKW: WRKL).
           MOVE     NEU-SRCLINE(WRKW: WRKL) TO
                    TXT-SRCLINE(WRKC: WRKL).

           PERFORM  CHANGE-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0019" TO EXMSGID
           END-IF.

       EXC-SLS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES SHIFT-RIGHT BEFEHLS (RS)
     ‚*--------------------------------------------------------------
       EXECUTE-RS SECTION.
       EXC-SLR-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-SLR-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* MERKE FÜLL-ZEICHEN
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-SAVE     = SPACE
            AND     COMMAND-CODE(1:2) = "RS"
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE(3:) TO WRK-FELD3
                    PERFORM  GET-NUMINP
           END-IF.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:2) = "/*"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.
           IF       TXT-SRCLINE(BEGCOL:2) = "--"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           MOVE     WRKC TO WRKW.

     ‚* FINDE BEGINN DER DATEN
           MOVE     WRKW TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
                    MOVE WRKW TO WRKC
           END-IF.
           IF       WRKC > WRKW
                    MOVE WRKC TO WRKW
           END-IF.

     ‚* ANZAHL AUS PREFIX-BEREICH
           IF       WRK-FELD5 IS NUMERIC
                    MOVE WRK-FELD5 TO WRKK
           ELSE
                    MOVE ZEROES TO WRKK
           END-IF.
           IF       WRKK = ZEROES
                    MOVE 1 TO WRKK
           END-IF.

     ‚* ERRECHNE VERSCHIEBUNG
           MOVE     ENDCOL TO WRKE.
           PERFORM  SUCHE-LETZTES-ZEICHEN.
           IF       WRKE < WRKW
                    MOVE WRKW TO WRKE
           END-IF.
           COMPUTE  WRKC = WRKW + WRKK.
           COMPUTE  WRKL = WRKE - WRKW + 1.
           IF       WRKC > ENDCOL
                    IF   EXMSGID = SPACES
                         MOVE "SEU0010" TO EXMSGID
                    END-IF
                    PERFORM CHANGE-LINE
                    GO TO EXC-SLR-90
           END-IF.

     ‚* VERSCHIEBE
           MOVE     TXT-SRCLINE TO NEU-SRCLINE.
           MOVE     SPACES      TO TXT-SRCLINE(WRKW: WRKL).
           MOVE     NEU-SRCLINE(WRKW: WRKL) TO
                    TXT-SRCLINE(WRKC: WRKL).

           PERFORM  CHANGE-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0019" TO EXMSGID
           END-IF.

       EXC-SLR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES BOX-BEFEHLS (BX)
     ‚*--------------------------------------------------------------
       EXECUTE-BX SECTION.
       EXC-BOX-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-BOX-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* MERKE PARAMETER 1 UND 2
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "BX"
            AND     COMMAND-SAVE      = SPACE
                    INITIALIZE SWBLK
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE TO BOXPAR1
                    COMPUTE  RECNO = (HDR-RCDSIN - 1)
                    PERFORM  GET-NEXT-LINE
                    MOVE TXT-LINECMD TO COMMAND-SAVE
                    MOVE TXT-LINECMD TO BOXPAR2
                    MOVE IDX TO RECNO
                    PERFORM  GET-NEXT-LINE
           END-IF.

     ‚* KOMMENTAR
           IF       TXT-SRCLINE(BEGCOL:ENDCOL - BEGCOL + 1) = SPACES
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    MOVE "HI" TO WRK-FELD3
                    PERFORM GET-ATHEX-VALUE
                    MOVE AT-HEX OF AT-FOUND TO TXT-SRCLINE(6:1)
                    MOVE "*" TO TXT-SRCLINE(BEGCOL:1)
           END-IF.
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           ELSE
                    IF   EXMSGID = SPACES
                         MOVE "SEU0007" TO EXMSGID
                    END-IF
                    PERFORM CHANGE-LINE
                    GO TO EXC-BOX-90
           END-IF.

     ‚* EINLESEN EINGEGEBENER ECKPUNK-POSITIONEN
           COMPUTE  WRKP = (HDR-RCDSIN - 1).
           MOVE     BOXPAR1(3:) TO COMMAND-SAVE.
           PERFORM  GET-NUMINP.
           MOVE     WRK-FELD5 TO BOXL.
           IF       WRKP > 1
                    MOVE     BOXPAR2(3:) TO COMMAND-SAVE
                    PERFORM  GET-NUMINP
                    MOVE     WRK-FELD5 TO BOXR
           ELSE
                    INITIALIZE BOXR
           END-IF.
           MOVE     BOXPAR1 TO COMMAND-SAVE.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE ZEROES TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
     ‚* SIND KEINE KOORDINATEN ANGEGEBEN, SETZE AUF CURSOR-POS
           IF       BOXL = ZEROES
                    MOVE WRKC TO BOXL
           END-IF.
           IF       BOXR = ZEROES
                    MOVE WRKC TO BOXR
           END-IF.
     ‚* SIND KEINE KOORDINATEN ANGEGEBEN, SETZE AUF MAXIMAL-WERTE
           IF       BOXL = ZEROES
            AND     BOXR = ZEROES
                    MOVE BEGCOL TO BOXL
                    MOVE ENDCOL TO BOXR
           END-IF.
     ‚* FEHLENDE KOORDINATE IST ZEILENANFANG
           IF       BOXL = ZEROES
                    MOVE BEGCOL TO BOXL
           END-IF.
           IF       BOXR = ZEROES
                    MOVE BEGCOL TO BOXR
           END-IF.
     ‚* WERTE AUF MAXIMAL-WERTE SETZEN
           IF       BOXL < BEGCOL
                    MOVE BEGCOL TO BOXL
           END-IF.
           IF       BOXR < BEGCOL
                    MOVE BEGCOL TO BOXR
           END-IF.
           IF       BOXL > ENDCOL
                    MOVE ENDCOL TO BOXL
           END-IF.
           IF       BOXR > ENDCOL
                    MOVE ENDCOL TO BOXR
           END-IF.
     ‚* LINKS MUSS KLEINER ALS RECHTS SEIN
           IF       BOXL > BOXR
                    MOVE BOXL TO WRKX
                    MOVE BOXR TO BOXL
                    MOVE WRKX TO BOXR
           END-IF.

     ‚* EINZEL-ZEILE
           COMPUTE  WRKP = (HDR-RCDSIN - 1).
           IF       WRKP = 1
                    INSPECT TXT-SRCLINE(BOXL:BOXR - BOXL + 1)
                            REPLACING ALL SPACES BY "-"
                    GO TO EXC-BOX-80
           END-IF.

     ‚* TOP, BODY, END
           EVALUATE TRUE
                    WHEN IDX = 1
                         MOVE "A" TO SWBLK
                    WHEN IDX = WRKP
                         MOVE "E" TO SWBLK
                    WHEN OTHER
                         MOVE "Z" TO SWBLK
           END-EVALUATE.

     ‚* BEGINN DER BOX (PROGRAMME, DDS)
           IF       (    KZ  OF FT-FOUND  = "1"
                     OR  KZ  OF FT-FOUND  = "2" )
            AND     TXT-SRCLINE(7:1) = "*"
            AND     (    SWBLK = "A"
                     OR  SWBLK = "E" )
                    COMPUTE WRKL = BOXR - BOXL + 1
                    INSPECT TXT-SRCLINE(BOXL: WRKL )
                            REPLACING ALL SPACES BY "-"
                    MOVE     "+" TO TXT-SRCLINE(BOXL:1)
                    MOVE     "+" TO TXT-SRCLINE(BOXR:1)
                    GO TO EXC-BOX-80
           END-IF.

     ‚* BEGINN DER BOX (TEXTE)
           IF       KZ  OF FT-FOUND  = "0"
            AND     (    SWBLK = "A"
                     OR  SWBLK = "E" )
                    COMPUTE WRKL = BOXR - BOXL + 1
                    INSPECT TXT-SRCLINE(BOXL: WRKL )
                            REPLACING ALL SPACES BY "-"
                    MOVE     "+" TO TXT-SRCLINE(BOXL:1)
                    MOVE     "+" TO TXT-SRCLINE(BOXR:1)
                    GO TO EXC-BOX-80
           END-IF.

     ‚* BEGINN DER BOX (TEXTE)
           IF       SWBLK = "Z"
                    MOVE "|" TO TXT-SRCLINE(BOXL:1)
                    MOVE "|" TO TXT-SRCLINE(BOXR:1)
           END-IF.

       EXC-BOX-80.
           PERFORM  CHANGE-LINE.

     ‚* BEFEHLS-ENDE
           IF       IDX = (HDR-RCDSIN - 1)
            AND     COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "BX"
            AND     COMMAND-SAVE NOT = SPACE
                    INITIALIZE COMMAND-SAVE
                    INITIALIZE BOXL
                    INITIALIZE BOXR
                    INITIALIZE SWBLK
           END-IF.

           IF       EXMSGID = SPACES
                    MOVE "SEU0009" TO EXMSGID
           END-IF.

       EXC-BOX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES VT-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-VT SECTION.
       EXC-VT-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-VT-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* BEFEHLS-START
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "VT"
            AND     COMMAND-SAVE     = SPACE
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    INITIALIZE SWBLK
           END-IF.

           IF       SWBLK = "1"
                    GO TO EXC-VT-80
           END-IF.

     ‚* KOPIERE ZEILEN.
           INITIALIZE SRC-TABELLE.
           COMPUTE  RECNO = (HDR-RCDSIN - 1).
           IF       RECNO > 99
                    GO TO EXC-VT-90
           END-IF.
           MOVE     RECNO TO SRC-ANZAHL.
           INITIALIZE SRCX.
           PERFORM  VARYING RECNO FROM 1 BY 1
                    UNTIL RECNO > SRC-ANZAHL
                    PERFORM GET-NEXT-LINE
                    INITIALIZE NEU-SRCLINE
     ‚*             ERMITTLE TEXT-BEREICH IM KOMMENTAR
                    PERFORM CHK-BOUNDARY
     ‚*             NORMALISIERE LEERSTELLEN
                    MOVE 1 TO WRKL
                    PERFORM MAKE-SPACE
     ‚*             HINZUFÜGEN DER ZEILE IN TABELLE
                    IF   NEU-SRCLINE > SPACES
                         ADD 1 TO SRCX
                         IF   SRC-ANZAHL OF SRC-TABELLE < SRCX
                              MOVE SRCX TO SRC-ANZAHL OF SRC-TABELLE
                         END-IF
                         INITIALIZE SRC-EINTRAG(SRCX)
                         MOVE NEU-SRCLINE TO SRC-EINTRAG(SRCX)
                    END-IF
           END-PERFORM.
           MOVE     "1" TO SWBLK.

     ‚* KOMPRIMIERT ZURÜCKSCHREIBEN
           MOVE     IDX TO RECNO.
           PERFORM  GET-NEXT-LINE.
           PERFORM  CHK-BOUNDARY.

     ‚* KOMPRIMIERT ZURÜCKSCHREIBEN
           INITIALIZE RECNO.
           INITIALIZE SRCX.
           INITIALIZE SRC-BUFFER.
           MOVE     1 TO WRKC.
           INITIALIZE WRKK.
           PERFORM  BUFFER-EINLESEN.
           MOVE     1 TO WRKP.
           PERFORM  UNTIL SRC-BUFFER = SPACES
                    IF   WRKK < ( ENDCOL - BEGCOL + 1)
                         PERFORM BUFFER-EINLESEN
                    END-IF
                    COMPUTE WRKL = ENDCOL - BEGCOL + 1
                    MOVE SPACES TO TXT-SRCLINE(BEGCOL:WRKL)
     ‚*             ERMITTLE ÜBERTRAGUNGSLÄNGE
                    COMPUTE WRKX = WRKC + WRKL - 1
                    PERFORM WORTE-AUS-BUFFER
     ‚*             HINZUFÜGEN DER ZEILE IN TABELLE
                    COMPUTE WRKX = BEGCOL
                    IF   WRKL > ZEROES
                         MOVE SRC-BUFFER(WRKC:WRKL) TO NEU-SRCLINE
                         MOVE NEU-SRCLINE TO TXT-SRCLINE(WRKX:WRKL)
                         MOVE SPACES TO SRC-BUFFER(WRKC:WRKL)
                         COMPUTE WRKC = WRKC + WRKL + 1
                         ADD 1 TO RECNO
                         COMPUTE  USRSPC-OFFSET
                                  = LENGTH OF HEADER-SPACE + 1
                                  + ( (RECNO - 1) * USRSPC-SIZE)
                         END-COMPUTE
                         PERFORM CHANGE-LINE
                    END-IF
           END-PERFORM.
           MOVE     RECNO TO HDR-RCDSOUT.

       EXC-VT-80.
     ‚* BEFEHLS-ENDE
           IF       IDX = (HDR-RCDSIN - 1)
            AND     COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "VT"
            AND     COMMAND-SAVE NOT = SPACE
                    INITIALIZE COMMAND-SAVE
                    INITIALIZE SWBLK
           END-IF.

           IF       EXMSGID = SPACES
                    MOVE "SEU0019" TO EXMSGID
           END-IF.

       EXC-VT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TRENNE WORTE MIT 1 LEERZEICHEN
     ‚*--------------------------------------------------------------
       MAKE-SPACE SECTION.
       MAK-SPC-00.

           MOVE     BEGCOL TO WRKP.
           MOVE     1 TO WRKK.
           PERFORM  UNTIL WRKP > ENDCOL
                    MOVE WRKP TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC > ZERO
                         MOVE WRKC TO WRKP
                         MOVE 1 TO WRKL
                         PERFORM SUCHE-LEERSTELLE
                         IF   WRKC = ZEROES
                              COMPUTE WRKC = ENDCOL + 1
                         END-IF
                         COMPUTE WRKL = WRKC - WRKP
                         MOVE TXT-SRCLINE(WRKP:WRKL) TO
                              NEU-SRCLINE(WRKK:WRKL)
                         COMPUTE WRKK = WRKK + WRKL + 1
                         COMPUTE WRKP = WRKP + WRKL + 1
                    ELSE
                         COMPUTE WRKP = ENDCOL + 1
                    END-IF
           END-PERFORM.

       MAK-SPC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÄNGE DER ÜBERTRAGUNG AUS BUFFER ERMITTELN
     ‚*--------------------------------------------------------------
       WORTE-AUS-BUFFER SECTION.
       WRD-BFR-00.

     ‚* BUFFER-BEGINN MINDESTENS STELLE 1
           IF       WRKC < 1
                    MOVE 1 TO WRKC
           END-IF.

     ‚* TEXTE-ENDE MAX. AUF BUFFER-ENDE
           IF       WRKX > (WRKC + WRKK - 1)
                    COMPUTE WRKX = WRKC + WRKK - 1
           END-IF.

     ‚* SUCHE LETZTE LEERSTELLE
           INITIALIZE WRKL.
           PERFORM  UNTIL WRKX < WRKC
                    OR    WRKL > ZEROES
                    IF   SRC-BUFFER(WRKX:1) = SPACES
                         COMPUTE WRKL = WRKX - WRKC
                    ELSE
                         SUBTRACT 1 FROM WRKX
                    END-IF
           END-PERFORM.

       WRD-BFR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GESPEICHERTEN TEXT AUS SRC-TABELLE IN BUFFER EINLESEN
     ‚*--------------------------------------------------------------
       BUFFER-EINLESEN SECTION.
       BFR-LES-00.

           IF       WRKC < 1
                    MOVE 1 TO WRKC
           END-IF.

     ‚* SCHIEBE RESTDATEN AN DEN ANFANG DES BUFFERS
           IF       WRKK > ZEROES
            AND     WRKK < ( ENDCOL - BEGCOL + 1)
                    INITIALIZE NEU-SRCLINE
                    MOVE SRC-BUFFER(WRKC:WRKK) TO NEU-SRCLINE
                    MOVE NEU-SRCLINE TO SRC-BUFFER
                    MOVE 1 TO WRKC
           END-IF.

           IF       SRCX > SRC-ANZAHL
                    GO TO BFR-LES-90
           END-IF.

     ‚* FÜLLE BUFFER MIT WEITEREN DATEN
           IF       SRCX = ZERO
                    MOVE 1 TO SRCX
           END-IF.
           PERFORM  UNTIL SRCX > SRC-ANZAHL
                     OR   WRKK > ( LENGTH OF SRC-BUFFER
                                 - HDR-RCDLEN )
                    INITIALIZE NEU-SRCLINE
                    MOVE SRC-EINTRAG(SRCX) TO NEU-SRCLINE
                    INITIALIZE WRKX
                    INSPECT FUNCTION REVERSE(NEU-SRCLINE)
                            TALLYING WRKX FOR LEADING SPACES
                    COMPUTE WRKL = LENGTH OF NEU-SRCLINE
                                 - WRKX + 1
                    END-COMPUTE
                    MOVE NEU-SRCLINE TO SRC-BUFFER(WRKC:WRKL)
                    ADD WRKL TO WRKK
                    ADD WRKL TO WRKC
                    ADD 1 TO SRCX
           END-PERFORM.
           MOVE     1 TO WRKC.

       BFR-LES-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES SORT-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-SORT SECTION.
       EXC-SRT-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-SRT-90
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* BEFEHLS-START
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:4) = "SORT"
            AND     COMMAND-SAVE     = SPACE
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE(5:) TO WRK-FELD3
                    INITIALIZE SWBLK
           END-IF.

           IF       SWBLK = "1"
                    GO TO EXC-SRT-80
           END-IF.

     ‚* IST EIN SORT NÖTIG/MÖGLICH ?
           COMPUTE  WRKP = (HDR-RCDSIN - 1).
           IF       WRKP < 2
                    MOVE "SEU0022" TO EXMSGID
                    GO TO EXC-SRT-90
           END-IF.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           MOVE     WRKC TO WRKK.
           COMPUTE  WRKL = ENDCOL - WRKK + 1.

     ‚* INITIALISIERUNG DER SORTROUTINE
           INITIALIZE SRT-KEYINF(1).
           MOVE     WRKK TO KEY-START OF KEYINF-ENTRY(1).
           MOVE     WRKL TO KEY-SIZE  OF KEYINF-ENTRY(1).
           MOVE     6    TO KEY-TYP   OF KEYINF-ENTRY(1).
           IF       WRK-FELD3(1:1) = "D"
                    MOVE 2 TO KEY-ORDER OF KEYINF-ENTRY(1)
           ELSE
                    MOVE 1 TO KEY-ORDER OF KEYINF-ENTRY(1)
           END-IF.
           MOVE     1                  TO SRT-NBROFKEYS.
           MOVE     LENGTH OF SORT-HDR TO SRT-BLOCKLEN.
           MOVE     USRSPC-SIZE        TO SRT-RECLEN.
           CALL      "QLGSORT"  USING SORT-HDR
                                      SRT-NOTUSED
                                      SRT-NOTUSED
                                      SRT-SIZELIST
                                      SRT-RETSIZE
                                      QUS-EC
           END-CALL.
           IF       EXCEPTION-ID > SPACES
                    MOVE EXCEPTION-ID TO MSG-ID
                    PERFORM SENDE-NACHRICHT
                    GO TO EXC-SRT-90
           END-IF.

     ‚* ÜBERGABE DER ZEILEN AN SORT-ROUTINE
           MOVE     USRSPC-SIZE TO IORECLEN OF IO-BLOCK.
           MOVE     1           TO IORECCNT OF IO-BLOCK.
           MOVE     1           TO IOTYPE   OF IO-BLOCK.
           INITIALIZE MSG-ID.
           PERFORM  VARYING RECNO FROM IDX BY 1
                    UNTIL RECNO > (IDX + WRKP - 1)
                     OR   MSG-ID > SPACES
                    PERFORM GET-NEXT-LINE
     ‚*             ÜBERGABE DER ZEILE AN SORT
                    CALL "QLGSRTIO" USING IO-BLOCK
                                          TXT-SRCLINE
                                          SRT-NOTUSED
                                          SRT-SIZELIST
                                          OUT-BLOCK
                                          QUS-EC
                    END-CALL
                    IF   EXCEPTION-ID > SPACES
                         MOVE EXCEPTION-ID TO MSG-ID
                         PERFORM SENDE-NACHRICHT
                    END-IF
           END-PERFORM.
           MOVE     "1" TO SWBLK.

     ‚* SORTIEREN
           MOVE     2   TO IOTYPE  OF IO-BLOCK.
           CALL     "QLGSRTIO" USING IO-BLOCK
                                     NEU-SRCLINE
                                     SRT-NOTUSED
                                     SRT-SIZELIST
                                     OUT-BLOCK
                                     QUS-EC
           END-CALL.
           IF       EXCEPTION-ID > SPACES
                    MOVE EXCEPTION-ID TO MSG-ID
                    PERFORM SENDE-NACHRICHT
                    GO TO EXC-SRT-90
           END-IF

     ‚* SORTIERT ZURÜCKSCHREIBEN
           MOVE     3   TO IOTYPE  OF IO-BLOCK.
           PERFORM  VARYING RECNO FROM IDX BY 1
                    UNTIL RECNO > (IDX + WRKP - 1)
                     OR   MSG-ID > SPACES
     ‚*             ÜBERGABE DER ZEILE VON SORT
                    CALL "QLGSRTIO" USING IO-BLOCK
                                          SRT-NOTUSED
                                          TXT-SRCLINE
                                          SRT-RECLEN
                                          OUT-BLOCK
                                          QUS-EC
                    END-CALL
                    IF   EXCEPTION-ID > SPACES
                         MOVE EXCEPTION-ID TO MSG-ID
                         PERFORM SENDE-NACHRICHT
                    ELSE
                         COMPUTE  USRSPC-OFFSET
                                  = LENGTH OF HEADER-SPACE + 1
                                  + ( (RECNO - 1) * USRSPC-SIZE)
                         END-COMPUTE
                         PERFORM CHANGE-LINE
                         IF  OUT-RECAV <= ZERO
                             COMPUTE RECNO = IDX + WRKP
                         END-IF
                    END-IF
           END-PERFORM.

       EXC-SRT-80.
     ‚* BEFEHLS-ENDE
           IF       IDX = (HDR-RCDSIN - 1)
            AND     COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:4) = "SORT"
            AND     COMMAND-SAVE NOT = SPACE
                    INITIALIZE COMMAND-SAVE
                    INITIALIZE SWBLK
           END-IF.

           IF       EXMSGID = SPACES
                    MOVE "SEU0022" TO EXMSGID
           END-IF.

       EXC-SRT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES DELETE-WORD-BEFEHLS (DW)
     ‚*--------------------------------------------------------------
       EXECUTE-DW SECTION.
       EXC-DEL-00.

‚     * ERMITTLE TYP DES MEMBERS
           IF       KZ  OF FT-FOUND = "0"
            OR      KZ  OF FT-FOUND = "1"
            OR      KZ  OF FT-FOUND = "2"
                    CONTINUE
           ELSE
                    GO TO EXC-DEL-90
           END-IF.

     ‚* MERKE BLOCK-BEFEHL
           IF       COMMAND-CODE NOT = SPACE
            AND     COMMAND-SAVE     = SPACE
            AND     COMMAND-CODE(1:2) = "DW"
                    MOVE COMMAND-CODE TO COMMAND-SAVE
                    MOVE COMMAND-CODE(3:) TO WRK-FELD3
                    PERFORM  GET-NUMINP
           END-IF.

     ‚* BEFEHLS-ENDE
           IF       SWBLK = "1"
            AND     COMMAND-CODE NOT = SPACE
            AND     COMMAND-CODE(1:2) = "DW"
            AND     COMMAND-SAVE NOT = SPACE
                    INITIALIZE COMMAND-SAVE
                    MOVE ZERO TO SWBLK
           END-IF.

           SET      CLEAR-COMMAND TO TRUE.

     ‚* ERMITTLE LINKS UND RECHTS
           PERFORM  CHK-BOUNDARY.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           MOVE     WRKC TO BEGSAVE1.

     ‚* ANZAHL AUS PREFIX-BEREICH
           IF       WRK-FELD5 IS NUMERIC
                    MOVE WRK-FELD5 TO WRKJ
           ELSE
                    MOVE ZEROES TO WRKJ
           END-IF.
           IF       WRKK = ZEROES
                    MOVE 1 TO WRKJ
           END-IF.

       EXC-DEL-10.
     ‚* FINDE BEGINN DER DATEN
           MOVE     BEGSAVE1 TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
            OR      WRKC > ENDCOL
                    GO TO EXC-DEL-90
           END-IF.
           MOVE     WRKC TO BEGSAVE1.

     ‚* SUCHE ATTRIBUT VOR CURSOR-POSITION
     ‚* UND DIE POSITION DAVON ODER EINER LEERSTELLE
           MOVE     ZEROES   TO WRKC.
           MOVE     BEGSAVE1 TO WRKX.
           PERFORM  UNTIL WRKX < BEGCOL
                     OR   WRKC > ZEROES
                    IF   TXT-SRCLINE(WRKX:1) = SPACE
                         MOVE WRKX TO WRKC
                    ELSE
                         IF   TXT-SRCLINE(WRKX:1) < X"20"
                          OR  TXT-SRCLINE(WRKX:1) > X"3F"
                              SUBTRACT 1 FROM WRKX
                         ELSE
                              MOVE WRKX TO WRKC
                              SUBTRACT 1 FROM WRKX
                         END-IF
                    END-IF
           END-PERFORM.
           IF       WRKC < BEGCOL
            OR      WRKC = ZEROES
                    MOVE BEGCOL TO WRKC
           ELSE
                    ADD 1 TO WRKC
           END-IF.
           MOVE     WRKC TO BEGSAVE1.

     ‚* SUCHE FOLGENDES ATTRIBUT NACH WORT-BEGINN
     ‚* UND DIE POSITION DAVON ODER EINER LEERSTELLE
           MOVE     BEGSAVE1 TO WRKX.
           MOVE     ZEROES TO WRKK.
           PERFORM  UNTIL WRKX > ENDCOL
                     OR   WRKK > ZEROES
                    IF   TXT-SRCLINE(WRKX:1) = SPACE
                         MOVE WRKX TO WRKK
                    ELSE
                         IF   TXT-SRCLINE(WRKX:1) < X"20"
                          OR  TXT-SRCLINE(WRKX:1) > X"3F"
                              ADD 1 TO WRKX
                         ELSE
                              MOVE WRKX TO WRKK
                         END-IF
                    END-IF
           END-PERFORM.
           IF       WRKK > ENDCOL
            OR      WRKK = ZEROES
                    MOVE ENDCOL TO WRKK
           END-IF.
           MOVE     WRKK TO ENDSAVE1.
           MOVE     ENDSAVE1 TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
            OR      WRKC > ENDCOL
                    MOVE ENDCOL TO WRKK
           ELSE
                    COMPUTE WRKK = WRKC - 1
           END-IF.
           MOVE     WRKK TO ENDSAVE1.

     ‚* ERMITTLE LÄNGE DES ZU LÖSCHENDEN TEXTES
           COMPUTE  WRKL = ENDSAVE1 - BEGSAVE1 + 1.
           IF       WRKL < 1
                    MOVE 1 TO WRKL
           END-IF.
           MOVE     WRKL TO BEGSAVE2.

     ‚* ZEICHEN ENTFERNEN
           MOVE     BEGSAVE1 TO WRKC.
           COMPUTE  WRKL = ENDSAVE1 - BEGSAVE1 + 1.
           IF       WRKL > ZEROES
                    MOVE TXT-SRCLINE TO NEU-SRCLINE
                    MOVE SPACES TO TXT-SRCLINE(WRKC:WRKL)
     ‚*             OPTION A=ALIGN
                    IF   WRK-FELD3(1:1) = "A"
                         COMPUTE WRKP = WRKC + WRKL
                         COMPUTE WRKK = ENDCOL - WRKP + 1
                         IF   WRKP NOT > ENDCOL
                          AND WRKK > ZERO
                              MOVE SPACES TO TXT-SRCLINE(WRKP:WRKK)
                              MOVE NEU-SRCLINE(WRKP:WRKK) TO
                                   TXT-SRCLINE(WRKC:WRKK)
                         END-IF
                    END-IF
           END-IF.

           IF       WRKJ > 1
                    SUBTRACT 1 FROM WRKJ
                    GO TO EXC-DEL-10
           END-IF.

           PERFORM  CHANGE-LINE.

           IF       COMMAND-SAVE NOT = SPACES
            AND     SWBLK            = ZERO
                    MOVE "1" TO SWBLK
           END-IF.

           IF       EXMSGID = SPACES
                    MOVE "SEU0024" TO EXMSGID
           END-IF.

       EXC-DEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES DFD-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-DFD SECTION.
       EXC-DFD-00.

‚     * ZEILE MIT BEFEHL BLEIBT UNVERÄNDERT
           SET      CLEAR-COMMAND TO TRUE.
           PERFORM  CHANGE-LINE.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:2) = "/*"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.
           IF       TXT-SRCLINE(BEGCOL:2) = "--"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           MOVE     WRKC TO WRKW.

     ‚* FINDE BEGINN DER DATEN
           MOVE     WRKW TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
                    GO TO EXC-DFD-90
           END-IF.
           IF       WRKC > WRKW
                    MOVE WRKC TO WRKW
           END-IF.

     ‚* ERMITTLE DATABASE-NAMEN
           INITIALIZE PHYFILE.
           INITIALIZE PHYFILE2.
           MOVE     WRKW TO WRKC.
           MOVE     "DATABASE-" TO WRK-FELD50.
           MOVE     9 TO WRKL.
           PERFORM  SUCHE-ZEICHEN.
           MOVE     WRKC TO WRKK.
           IF       WRKK > ZEROES
                    MOVE 1 TO WRKL
                    PERFORM SUCHE-LEERSTELLE
                    COMPUTE WRKL = WRKC - (WRKK + 9)
                    IF  WRKL > ZEROES
                        MOVE TXT-SRCLINE(WRKK + 9:WRKL) TO PHYFILE
                    END-IF
           END-IF.
           IF       PHYFILE = SPACES
                    MOVE WRKW TO WRKC
                    MOVE "-REC" TO WRK-FELD50
                    MOVE 4 TO WRKL
                    PERFORM SUCHE-ZEICHEN
                    MOVE WRKC TO WRKK
                    IF   WRKK = ZEROES
                         MOVE WRKW TO WRKC
                         MOVE "-DAT" TO WRK-FELD50
                         MOVE 4 TO WRKL
                         PERFORM SUCHE-ZEICHEN
                         MOVE WRKC TO WRKK
                    END-IF
                    IF   WRKK > ZEROES
                         MOVE 1 TO WRKL
                         INITIALIZE WRK-FELD50
                         PERFORM SUCHE-ZEICHEN-REVERSE
                         COMPUTE WRKL = WRKK - (WRKC + 1)
                         IF  WRKL > ZEROES
                             MOVE TXT-SRCLINE(WRKC + 1:WRKL)
                                  TO PHYFILE
                         END-IF
                    END-IF
           END-IF.
           IF       PHYFILE = SPACES
            AND     KZ  OF FT-FOUND = "2"
                    IF   WRKW < 45
                         MOVE 45 TO WRKC
                    ELSE
                         MOVE WRKW TO WRKC
                    END-IF
                    MOVE "REFFLD(" TO WRK-FELD50
                    MOVE 7 TO WRKL
                    PERFORM SUCHE-ZEICHEN
                    MOVE WRKC TO WRKK
                    IF   WRKK > ZEROES
                         MOVE ")" TO WRK-FELD50
                         MOVE 1 TO WRKL
                         PERFORM SUCHE-ZEICHEN
                         MOVE WRKC TO WRKK
                    END-IF
                    IF   WRKK > ZEROES
                         MOVE 1 TO WRKL
                         INITIALIZE WRK-FELD50
                         PERFORM SUCHE-ZEICHEN-REVERSE
                         COMPUTE WRKL = WRKK - (WRKC + 1)
                         IF  WRKL > ZEROES
                             MOVE TXT-SRCLINE(WRKC + 1:WRKL)
                                  TO PHYFILE
                         END-IF
                    END-IF
           END-IF.
           IF       PHYFILE = SPACES
                    MOVE "?" TO PHYFILE
           END-IF.

     ‚* ERMITTLE FELD-DEFINITIONEN
           INITIALIZE EXC-PARAMS.
           MOVE     "DSPFLD"                    TO EXC-COMMAND.
           MOVE     PHYFILE                     TO EXC-FILE.
           MOVE     "DSPFLDD"                   TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO EXC-DFD-80
           END-IF.
           OPEN     INPUT QADSPFFD-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-DFD-80
           END-IF.
           INITIALIZE MSG-TABELLE.
           MOVE     ZEROES TO MSGX.

           PERFORM  UNTIL FILE-STATUS NOT = ZEROES
                    READ QADSPFFD-DATP NEXT
                    IF   FILE-STATUS = ZEROES
     ‚*                  SETZE TABELLE AUF MAX. ZU ERWARTENDE SÄTZE
                         IF   MSG-ANZAHL = ZERO
                              MOVE WHFLDN OF QADSPFFD-RECP TO
                                   MSG-ANZAHL
                         END-IF
     ‚*                  ANZEIGE DER DATEIBESCHREIBUNG
                         IF   WHFILE OF QADSPFFD-RECP NOT = PHYFILE2
                              MOVE WHFILE OF QADSPFFD-RECP TO PHYFILE2
                              PERFORM AUSGABE-FLDHDR
                         END-IF
     ‚*                  FÜLLE ZEILE MIT DEFINITIONEN
                         PERFORM AUSGABE-FLDDEF
     ‚*                  HINZUFÜGEN DER ZEILE IN TABELLE
                         ADD 1 TO MSGX
                         IF   MSG-ANZAHL OF MSG-TABELLE < MSGX
                              MOVE MSGX TO MSG-ANZAHL OF MSG-TABELLE
                         END-IF
                         MOVE MSG-TXT TO
                              MSG-ZEILE OF MSG-EINTRAG(MSGX)
                    END-IF
           END-PERFORM.
           MOVE     "SEU0103" TO MSG-ID.
           PERFORM  ANZEIGE-MSGTAB.

       EXC-DFD-80.
     ‚* FREIGABE DER OVERRIDES
           CLOSE    QADSPFFD-DATP.
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QADSPFFD"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.

       EXC-DFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE-ZEILE FÜR FELD-DEFINITION AUFBEREITEN
     ‚*--------------------------------------------------------------
       AUSGABE-FLDDEF SECTION.
       AUS-FLD-00.

     ‚* FÜLLE ZEILE MIT DEFINITIONEN
           INITIALIZE MSG-TXT.
           MOVE     WHFLDI OF QADSPFFD-RECP TO MSG-TXT(01:10).
           MOVE     WHFLDT OF QADSPFFD-RECP TO MSG-TXT(12:01).
           EVALUATE WHFLDT OF QADSPFFD-RECP
                    WHEN "S"
                    WHEN "P"
                    WHEN "F"
                    WHEN "B"
                         MOVE WHFLDD OF QADSPFFD-RECP TO MSG-TXT(19:02)
                         INSPECT MSG-TXT(19:02)
                                 REPLACING LEADING ZEROES BY SPACES
                         MOVE WHFLDP OF QADSPFFD-RECP(1:2) TO
                              MSG-TXT(21:02)
                         IF   MSG-TXT(21:01) = ZERO
                              MOVE SPACE TO MSG-TXT(21:01)
                         END-IF
                    WHEN "L"
                    WHEN "T"
                    WHEN "Z"
                         CONTINUE
                    WHEN OTHER
                         MOVE WHFLDB OF QADSPFFD-RECP TO MSG-TXT(16:05)
                         INSPECT MSG-TXT(16:04)
                                 REPLACING LEADING ZEROES BY SPACES
           END-EVALUATE.
           MOVE     WHFTXT OF QADSPFFD-RECP TO MSG-TXT(24:50).

       AUS-FLD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHRUNG DES DK-BEFEHLS
     ‚*--------------------------------------------------------------
       EXECUTE-DK SECTION.
       EXC-DK-00.

           INITIALIZE MSG-TABELLE.

‚     * ZEILE MIT BEFEHL BLEIBT UNVERÄNDERT
           SET      CLEAR-COMMAND TO TRUE.
           PERFORM  CHANGE-LINE.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:2) = "/*"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.
           IF       TXT-SRCLINE(BEGCOL:2) = "--"
            AND     KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-IF.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           MOVE     WRKC TO WRKW.

     ‚* FINDE BEGINN DER DATEN
           MOVE     WRKW TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
                    GO TO EXC-DK-90
           END-IF.
           IF       WRKC > WRKW
                    MOVE WRKC TO WRKW
           END-IF.

     ‚* ERMITTLE DATABASE-NAMEN
           INITIALIZE PHYFILE.
           MOVE     WRKW TO WRKC.
           MOVE     "DATABASE-" TO WRK-FELD50.
           MOVE     9 TO WRKL.
           PERFORM  SUCHE-ZEICHEN.
           MOVE     WRKC TO WRKK.
           IF       WRKK > ZEROES
                    MOVE 1 TO WRKL
                    PERFORM SUCHE-LEERSTELLE
                    COMPUTE WRKL = WRKC - (WRKK + 9)
                    IF  WRKL > ZEROES
                        MOVE TXT-SRCLINE(WRKK + 9:WRKL) TO PHYFILE
                    END-IF
           END-IF.
           IF       PHYFILE = SPACES
                    MOVE "?" TO PHYFILE
           END-IF.

     ‚* ERMITTLE KEY-FELDER
           INITIALIZE EXC-PARAMS.
           MOVE     "DSPKEY"                    TO EXC-COMMAND.
           MOVE     PHYFILE                     TO EXC-FILE.
           MOVE     "DSPKEYS"                   TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO EXC-DK-80
           END-IF.
           OPEN     INPUT QAFDACCP-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO EXC-DK-80
           END-IF.
           INITIALIZE FLD-TABELLE.
           INITIALIZE PHYFILE.
           INITIALIZE PHYFILE2.
           MOVE     1 TO MSGX.

           PERFORM  UNTIL FILE-STATUS NOT = ZEROES
                    READ QAFDACCP-DATP NEXT
                    IF   FILE-STATUS = ZEROES
     ‚*                  SPEICHERE FELD-DEFINITIONEN
                         IF PHYFILE = SPACES
                            MOVE APFILE OF QAFDACCP-RECP TO
                                 PHYFILE
                            PERFORM FLD-EINLESEN
                         END-IF
     ‚*                  SETZE TABELLE AUF MAX. ZU ERWARTENDE SÄTZE
                         IF   MSG-ANZAHL = ZERO
                              MOVE APNKYF OF QAFDACCP-RECP TO
                                   MSG-ANZAHL
                              ADD  1 TO MSG-ANZAHL
                         END-IF
     ‚*                  ANZEIGE DER DATEIBESCHREIBUNG
                         IF   APFILE OF QAFDACCP-RECP NOT = PHYFILE2
                              PERFORM SEL-RECORDS
                              MOVE APFILE OF QAFDACCP-RECP TO PHYFILE2
                              PERFORM AUSGABE-KEYHDR
                         END-IF
     ‚*                  SUCHE FELD IN FLD-TABELLE
                         MOVE APKEYF OF QAFDACCP-RECP TO WRK-FELD10
                         PERFORM SUCHE-FLDDEF
     ‚*                  HINZUFÜGEN DER ZEILE IN TABELLE
                         IF   FLDX > ZEROES
                              PERFORM AUSGABE-KEYFLD
                              ADD 1 TO MSGX
                              IF   MSG-ANZAHL OF MSG-TABELLE < MSGX
                                   MOVE MSGX TO
                                        MSG-ANZAHL OF MSG-TABELLE
                              END-IF
                              MOVE MSG-TXT TO
                                   MSG-ZEILE OF MSG-EINTRAG(MSGX)
                         END-IF
                    END-IF
           END-PERFORM.
           MOVE     "SEU0102" TO MSG-ID.

     ‚* ERMITTLE SELEKTIONS-BEDINGUNGEN
           PERFORM  SEL-RECORDS.

       EXC-DK-80.
     ‚* ANZEIGE DER BILDSCHIRM-TABELLE
           PERFORM  ANZEIGE-MSGTAB.

     ‚* FREIGABE DER OVERRIDES
           CLOSE    QAFDACCP-DATP.
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QAFDACCP"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.

       EXC-DK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DEFINITIONEN DER FELDER IN FLD-TABELLE EINLESEN
     ‚*--------------------------------------------------------------
       FLD-EINLESEN SECTION.
       LES-FLD-00.

     ‚* ERMITTLE FELD-DEFINITIONEN
           INITIALIZE EXC-PARAMS.
           MOVE     "DSPFLD"                    TO EXC-COMMAND.
           MOVE     PHYFILE                     TO EXC-FILE.
           MOVE     PHYFILE                     TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO LES-FLD-90
           END-IF.
           OPEN     INPUT QADSPFFD-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-FLD-90
           END-IF.
           INITIALIZE FLD-TABELLE.
           MOVE     ZEROES TO FLDX.

           PERFORM  UNTIL FILE-STATUS NOT = ZEROES
                    READ QADSPFFD-DATP NEXT
                    IF   FILE-STATUS = ZEROES
     ‚*                  SETZE TABELLE AUF MAX. ZU ERWARTENDE SÄTZE
                         IF   FLD-ANZAHL = ZERO
                              MOVE WHFLDN OF QADSPFFD-RECP TO
                                   FLD-ANZAHL
                         END-IF
     ‚*                  HINZUFÜGEN DER ZEILE IN TABELLE
                         ADD 1 TO FLDX
                         IF    FLD-ANZAHL OF FLD-TABELLE < FLDX
                               MOVE FLDX TO FLD-ANZAHL OF FLD-TABELLE
                         END-IF
                         INITIALIZE FLD-EINTRAG(FLDX)
                         MOVE CORR QWHDRFFD OF QADSPFFD-RECP TO
                              FLD-EINTRAG(FLDX)
                    END-IF
           END-PERFORM.

     ‚* FREIGABE DES OVERRIDES
           CLOSE    QADSPFFD-DATP.
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QADSPFFD"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.

       LES-FLD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHE EIN FELD IN DER FELD-TABELLE
     ‚*--------------------------------------------------------------
       SUCHE-FLDDEF SECTION.
       SUC-FLD-00.

           INITIALIZE FLDX.
           IF       WRK-FELD10 = SPACES
            OR      FLD-ANZAHL OF FLD-TABELLE = ZEROES
                    GO TO SUC-FLD-90
           END-IF.

           MOVE     1 TO WRKC.
           PERFORM  UNTIL WRKC > FLD-ANZAHL OF FLD-TABELLE
                     OR   FLDX > ZEROES
                    IF   WHFLDI OF FLD-EINTRAG(WRKC) = WRK-FELD10
                     AND WHFLDI OF FLD-EINTRAG(WRKC) > SPACES
                         MOVE WRKC TO FLDX
                    ELSE
                         ADD  1 TO WRKC
                    END-IF
           END-PERFORM.

       SUC-FLD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE-ZEILE FÜR SCHLÜSSEL-DEFINITION AUFBEREITEN
     ‚*--------------------------------------------------------------
       AUSGABE-KEYFLD SECTION.
       AUS-KEY-00.

     ‚* FÜLLE ZEILE MIT DEFINITIONEN
           INITIALIZE MSG-TXT.
           MOVE     WHFLDI OF FLD-EINTRAG(FLDX) TO MSG-TXT(01:10).
           MOVE     WHFLDT OF FLD-EINTRAG(FLDX) TO MSG-TXT(12:01).
           MOVE     APKSEQ OF QAFDACCP-RECP     TO MSG-TXT(14:01).
           EVALUATE WHFLDT OF FLD-EINTRAG(FLDX)
                    WHEN "S"
                    WHEN "P"
                    WHEN "F"
                    WHEN "B"
                         MOVE WHFLDD OF FLD-EINTRAG(FLDX) TO
                              MSG-TXT(19:02)
                         INSPECT MSG-TXT(19:02)
                                 REPLACING LEADING ZEROES BY SPACES
                         MOVE WHFLDP OF FLD-EINTRAG(FLDX)(1:2) TO
                              MSG-TXT(21:02)
                         IF   MSG-TXT(21:01) = ZERO
                              MOVE SPACE TO MSG-TXT(21:01)
                         END-IF
                    WHEN "L"
                    WHEN "T"
                    WHEN "Z"
                         CONTINUE
                    WHEN OTHER
                         MOVE WHFLDB OF FLD-EINTRAG(FLDX) TO
                              MSG-TXT(16:05)
                         INSPECT MSG-TXT(16:04)
                                 REPLACING LEADING ZEROES BY SPACES
           END-EVALUATE.
           MOVE     WHFTXT OF FLD-EINTRAG(FLDX) TO MSG-TXT(24:50).

       AUS-KEY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTLE SELEKTIONS-KRITERIEN
     ‚*--------------------------------------------------------------
       SEL-RECORDS SECTION.
       SEL-REC-00.

           INITIALIZE EXC-PARAMS.
           MOVE     "DSPSEL"                    TO EXC-COMMAND.
           MOVE     PHYFILE2                    TO EXC-FILE.
           MOVE     PHYFILE2                    TO EXC-MBR.
           PERFORM  EXEC-SYSTEMBEFEHL.
           IF       EXC-RC NOT = SPACES
                    GO TO SEL-REC-80
           END-IF.
           OPEN     INPUT QAFDSELO-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO SEL-REC-80
           END-IF.

           PERFORM  UNTIL FILE-STATUS NOT = ZEROES
                    READ QAFDSELO-DATP NEXT
                    IF   FILE-STATUS = ZEROES
     ‚*                  SETZE TABELLE AUF MAX. ZU ERWARTENDE SÄTZE
                         ADD SONRUL OF QAFDSELO-RECP TO MSG-ANZAHL
     ‚*                  HINZUFÜGEN DER ZEILE IN TABELLE
                         IF   SONRUL OF QAFDSELO-RECP > ZERO
                          AND SOFLD  OF QAFDSELO-RECP NOT = SPACES
                          AND (   SORULE OF QAFDSELO-RECP = "O"
                               OR SORULE OF QAFDSELO-RECP = "S" )
                              PERFORM AUSGABE-SELFLD
                              ADD 1 TO MSGX
                              IF   MSG-ANZAHL OF MSG-TABELLE < MSGX
                                   MOVE MSGX TO
                                        MSG-ANZAHL OF MSG-TABELLE
                              END-IF
                              MOVE MSG-TXT TO
                                   MSG-ZEILE OF MSG-EINTRAG(MSGX)
                         END-IF
                    END-IF
           END-PERFORM.
           CLOSE    QAFDSELO-DATP.

       SEL-REC-80.
     ‚* FREIGABE DER OVERRIDES
           INITIALIZE EXC-PARAMS.
           MOVE     "DLTOVR"    TO EXC-COMMAND.
           MOVE     "QAFDSELO"  TO EXC-FILE.
           PERFORM  EXEC-SYSTEMBEFEHL.

       SEL-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE-ZEILE FÜR SELECT/OMIT AUFBEREITEN
     ‚*--------------------------------------------------------------
       AUSGABE-SELFLD SECTION.
       AUS-SEL-00.

     ‚* FÜLLE ZEILE MIT SELEKTIONSBEDINGUNG
           INITIALIZE MSG-TXT.
           INITIALIZE WRK-FELD50.
           MOVE     SORULE OF QAFDSELO-RECP TO MSG-TXT(01:01).
           MOVE     SOFLD  OF QAFDSELO-RECP TO MSG-TXT(04:10).
           MOVE     SOVALL OF QAFDSELO-RECP TO WRKL.
           IF       SOCOMP OF QAFDSELO-RECP = "VA"
                    STRING "COMP(" "EQ" " "    DELIMITED BY SIZE
                           SOFLD  OF QAFDSELO-RECP(1:WRKL)
                           ")"                 DELIMITED BY SIZE
                           INTO WRK-FELD50
                    END-STRING
           ELSE
                    STRING "COMP("             DELIMITED BY SIZE
                           SOCOMP OF QAFDSELO-RECP
                           " "                 DELIMITED BY SIZE
                           SOVALU OF QAFDSELO-RECP(1:WRKL)
                           ")"                 DELIMITED BY SIZE
                           INTO WRK-FELD50
                    END-STRING
           END-IF.
           MOVE     WRK-FELD50 TO MSG-TXT(20:50).

       AUS-SEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE EINER DATEI-BESCHREIBUNG BEI KEY-FELDER
     ‚*--------------------------------------------------------------
       AUSGABE-KEYHDR SECTION.
       AUS-HDR-00.

           INITIALIZE EXC-PARAMS.
           MOVE     "RTVOBJD"      TO EXC-COMMAND.
           MOVE     PHYFILE2       TO EXC-FILE.
           MOVE     "*LIBL"        TO EXC-LIB.
           MOVE     "*FILE"        TO EXC-TYPE.
           PERFORM  EXEC-SYSTEMBEFEHL.

           INITIALIZE MSG-TXT.
           STRING   "Datei :"  PHYFILE2 " "
                    EXC-TEXT
                    DELIMITED BY SIZE INTO MSG-TXT.
           ADD      1 TO MSGX.
           IF       MSG-ANZAHL OF MSG-TABELLE < MSGX
                    MOVE MSGX TO MSG-ANZAHL OF MSG-TABELLE
           END-IF.
           MOVE     MSG-TXT TO    MSG-ZEILE OF MSG-EINTRAG(MSGX).

           INITIALIZE MSG-TXT.
           STRING   "Format:"  WHNAME OF FLD-EINTRAG(1) " "
                    "Typ:"     APFILA OF QAFDACCP-RECP " "
                    "Zugriff:" APACCP OF QAFDACCP-RECP " "
                    "Unique:"  APUNIQ OF QAFDACCP-RECP
                    DELIMITED BY SIZE INTO MSG-TXT.
           ADD      1 TO MSGX.
           IF       MSG-ANZAHL OF MSG-TABELLE < MSGX
                    MOVE MSGX TO MSG-ANZAHL OF MSG-TABELLE
           END-IF.
           MOVE     MSG-TXT TO    MSG-ZEILE OF MSG-EINTRAG(MSGX).

       AUS-HDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE EINER DATEI-BESCHREIBUNG BEI FLD-BESCHREIBUNG
     ‚*--------------------------------------------------------------
       AUSGABE-FLDHDR SECTION.
       FLD-HDR-00.

           INITIALIZE EXC-PARAMS.
           MOVE     "RTVOBJD"               TO EXC-COMMAND.
           MOVE     WHFILE OF QADSPFFD-RECP TO EXC-FILE.
           MOVE     "*LIBL"                 TO EXC-LIB.
           MOVE     "*FILE"                 TO EXC-TYPE.
           PERFORM  EXEC-SYSTEMBEFEHL.

           INITIALIZE MSG-TXT.
           STRING   "Datei :"  WHFILE OF QADSPFFD-RECP " "
                    EXC-TEXT
                    DELIMITED BY SIZE INTO MSG-TXT.
           ADD      1 TO MSGX.
           IF       MSG-ANZAHL OF MSG-TABELLE < MSGX
                    MOVE MSGX TO MSG-ANZAHL OF MSG-TABELLE
           END-IF.
           MOVE     MSG-TXT TO    MSG-ZEILE OF MSG-EINTRAG(MSGX).

           INITIALIZE MSG-TXT.
           STRING   "Format:"    WHNAME OF QADSPFFD-RECP " "
                    "Felder:"    WHFLDN OF QADSPFFD-RECP " "
                    "Satzlänge:" WHRLEN OF QADSPFFD-RECP
                    DELIMITED BY SIZE INTO MSG-TXT.
           ADD      1 TO MSGX.
           IF       MSG-ANZAHL OF MSG-TABELLE < MSGX
                    MOVE MSGX TO MSG-ANZAHL OF MSG-TABELLE
           END-IF.
           MOVE     MSG-TXT TO    MSG-ZEILE OF MSG-EINTRAG(MSGX).

       FLD-HDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHE LEERSTELLE AB ANGEGEBENER POSITION
     ‚*--------------------------------------------------------------
       SUCHE-LEERSTELLE SECTION.
       SUC-LER-00.

           INITIALIZE WRKX.

     ‚* LÄNGE 1 BIS 12
           IF       WRKL < 1
            OR      WRKL > 12
                    MOVE 12 TO WRKL
           END-IF.

     ‚* POSITION NICHT KLEINER START-POSITION
           IF       WRKC < BEGCOL
                    MOVE BEGCOL TO WRKC
           END-IF.

     ‚* POSITION NICHT GRÖßER END-POSITION
           IF       (WRKC + WRKL - 1) > ENDCOL
                    COMPUTE WRKC = ENDCOL - WRKL + 1
           END-IF.

     ‚* SUCHE LEERSTELLE(N), WENN GEFUNDEN, DANN MERKE POSITION
           PERFORM  UNTIL (WRKC + WRKL - 1) > ENDCOL
                    OR    WRKX > ZEROES
                    IF   TXT-SRCLINE(WRKC:WRKL) = SPACE
                         MOVE WRKC TO WRKX
                    ELSE
                         ADD 1 TO WRKC
                    END-IF
           END-PERFORM.

     ‚* LEERSTELLE(N) GEFUNDEN AUF POSITION ODER NULL
           MOVE     WRKX TO WRKC.

       SUC-LER-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHE TEXT AB ANGEGEBENER POSITION
     ‚*--------------------------------------------------------------
       SUCHE-ZEICHEN SECTION.
       SUC-TXT-00.

           INITIALIZE WRKX.

     ‚* LÄNGE 1 BIS 50
           IF       WRKL < 1
                    MOVE 1 TO WRKL
           END-IF.
           IF       WRKL > LENGTH OF WRK-FELD50
                    MOVE LENGTH OF WRK-FELD50 TO WRKL
           END-IF.

     ‚* POSITION NICHT GRÖßER END-POSITION
           IF       (WRKC + WRKL - 1) > ENDCOL
                    COMPUTE WRKC = ENDCOL - WRKL + 1
           END-IF.

     ‚* POSITION NICHT KLEINER START-POSITION
           IF       WRKC < BEGCOL
                    MOVE BEGCOL TO WRKC
           END-IF.

     ‚* SUCHE TEXT, WENN GEFUNDEN, DANN MERKE POSITION
           PERFORM  UNTIL (WRKC + WRKL - 1) > ENDCOL
                    OR    WRKX > ZEROES
                    IF   TXT-SRCLINE(WRKC:WRKL) = WRK-FELD50(1:WRKL)
                         MOVE WRKC TO WRKX
                    ELSE
                         ADD 1 TO WRKC
                    END-IF
           END-PERFORM.

     ‚* ZEICHEN GEFUNDEN AUF POSITION ODER NULL
           MOVE     WRKX TO WRKC.

       SUC-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHE LEERSTELLE AB ANGEGEBENER POSITION RÜCKWÄRTS
     ‚*--------------------------------------------------------------
       SUCHE-ZEICHEN-REVERSE SECTION.
       SUC-ZRV-00.

           INITIALIZE WRKX.

     ‚* LÄNGE 1 BIS 50
           IF       WRKL < 1
                    MOVE 1 TO WRKL
           END-IF.
           IF       WRKL > LENGTH OF WRK-FELD50
                    MOVE LENGTH OF WRK-FELD50 TO WRKL
           END-IF.

     ‚* POSITION NICHT GRÖßER END-POSITION
           IF       (WRKC + WRKL - 1) > ENDCOL
                    COMPUTE WRKC = ENDCOL - WRKL + 1
           END-IF.

     ‚* SUCHE ZEICHEN, WENN GEFUNDEN, DANN MERKE POSITION
           PERFORM  UNTIL WRKC < BEGCOL
                    OR    WRKX > ZEROES
                    IF   TXT-SRCLINE(WRKC:WRKL) = WRK-FELD50(1:WRKL)
                         MOVE WRKC TO WRKX
                    ELSE
                         SUBTRACT 1 FROM WRKC
                    END-IF
           END-PERFORM.

     ‚* ZEICHEN GEFUNDEN AUF POSITION ODER NULL
           MOVE     WRKX TO WRKC.

       SUC-ZRV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHE NON-BLANK AB POSITION
     ‚*--------------------------------------------------------------
       SUCHE-ANYZEICHEN SECTION.
       SUC-ANY-00.

           INITIALIZE WRKX.

     ‚* POSITION NICHT KLEINER START-POSITION
           IF       WRKC > ZEROES AND WRKC < BEGCOL
                    MOVE BEGCOL TO WRKC
           END-IF.
           IF       WRKC = ZEROES
                    MOVE 1 TO WRKC
           END-IF.

     ‚* SUCHE ZEICHEN, WENN GEFUNDEN, DANN MERKE POSITION
           PERFORM  UNTIL WRKC > ENDCOL
                    OR    WRKX > ZEROES
                    IF   TXT-SRCLINE(WRKC:1) > SPACE
                         MOVE WRKC TO WRKX
                    ELSE
                         ADD 1 TO WRKC
                    END-IF
           END-PERFORM.

     ‚* ZEICHEN(N) GEFUNDEN AUF POSITION ODER NULL
           MOVE     WRKX TO WRKC.

       SUC-ANY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHE LETZTES ZEICHEN  AB ANGEGEBENER POSITION
     ‚*--------------------------------------------------------------
       SUCHE-LETZTES-ZEICHEN SECTION.
       SUC-LZT-00.

           INITIALIZE WRKX.

     ‚* POSITION AUF END-POSITION SETZEN
           IF       WRKE > ENDCOL
                    MOVE ENDCOL TO WRKE
           END-IF.

     ‚* SUCHE ZEICHEN, WENN GEFUNDEN, DANN MERKE POSITION
           PERFORM  UNTIL WRKE < BEGCOL
                    OR    WRKX > ZEROES
                    IF   TXT-SRCLINE(WRKE:1) > SPACE
                         MOVE WRKE TO WRKX
                    ELSE
                         SUBTRACT 1 FROM WRKE
                    END-IF
           END-PERFORM.

     ‚* ZEICHEN(N) GEFUNDEN AUF POSITION ODER NULL
           MOVE     WRKX TO WRKE.

       SUC-LZT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHE WORTENDE, D.H. BIS LEERZEICHEN ODER SONDERZEICHEN
     ‚*--------------------------------------------------------------
       SUCHE-WORTENDE SECTION.
       SUC-WRE-00.

           INITIALIZE WRKX.

     ‚* POSITION NICHT KLEINER START-POSITION
           IF       WRKC < BEGCOL
                    MOVE BEGCOL TO WRKC
           END-IF.

     ‚* POSITION NICHT GRÖßER END-POSITION
           IF       WRKC > ENDCOL
                    MOVE ENDCOL TO WRKC
           END-IF.

     ‚* SUCHE LEERSTELLE(N) ODER SONDERZEICHEN,
     ‚* WENN GEFUNDEN, DANN MERKE POSITION
           PERFORM  UNTIL WRKC > ENDCOL
                    OR    WRKX > ZEROES
                    IF   TXT-SRCLINE(WRKC:1) IS ALPHABETIC
                     AND TXT-SRCLINE(WRKC:1) NOT = SPACE
                     OR  TXT-SRCLINE(WRKC:1) IS NUMERIC
                     OR  TXT-SRCLINE(WRKC:1) IS UMLAUTE
                         ADD 1 TO WRKC
                    ELSE
                         MOVE WRKC TO WRKX
                    END-IF
           END-PERFORM.

     ‚* LEERSTELLE(N) GEFUNDEN AUF POSITION ODER NULL
           MOVE     WRKX TO WRKC.

       SUC-WRE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EXTRAHIERE DEN NUMERISCHEN INPUT AUS DEM PREFIX-BEREICH
     ‚*--------------------------------------------------------------
       GET-NUMINP SECTION.
       NUM-INP-00.

     ‚* POSITION AUF END-POSITION SETZEN
           MOVE     LENGTH OF COMMAND-SAVE TO WRKJ.
           MOVE     LENGTH OF WRK-FELD5    TO WRKX.
           MOVE     ZEROES TO WRK-FELD5.

     ‚* SUCHE ZEICHEN, WENN GEFUNDEN, DANN MERKE POSITION
           PERFORM  UNTIL WRKJ < 1
                    OR    WRKX < 1
                    IF   COMMAND-SAVE(WRKJ:1) < "0"
                     OR  COMMAND-SAVE(WRKJ:1) > "9"
                         SUBTRACT 1 FROM WRKJ
                    ELSE
                         MOVE COMMAND-SAVE(WRKJ:1) TO
                              WRK-FELD5(WRKX:1)
                         SUBTRACT 1 FROM WRKX
                         SUBTRACT 1 FROM WRKJ
                    END-IF
           END-PERFORM.

       NUM-INP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER HILFE
     ‚*--------------------------------------------------------------
       EXECUTE-HILFE SECTION.
       EXC-HLP-00.

           EVALUATE TRUE
                    WHEN COMMAND-CODE      = "?AP"
                         MOVE "SEU1001" TO MSG-ID
                    WHEN COMMAND-CODE      = "?AT"
                         MOVE "SEU1002" TO MSG-ID
                    WHEN COMMAND-CODE      = "?BX"
                         MOVE "SEU1003" TO MSG-ID
                    WHEN COMMAND-CODE      = "?CKT"
                         MOVE "SEU1004" TO MSG-ID
                    WHEN COMMAND-CODE      = "?DT"
                         MOVE "SEU1005" TO MSG-ID
                    WHEN COMMAND-CODE      = "?ISS"
                         MOVE "SEU1007" TO MSG-ID
                    WHEN COMMAND-CODE      = "?ISF"
                         MOVE "SEU1008" TO MSG-ID
                    WHEN COMMAND-CODE      = "?ISK"
                         MOVE "SEU1009" TO MSG-ID
                    WHEN COMMAND-CODE      = "?ISC"
                         MOVE "SEU1010" TO MSG-ID
                    WHEN COMMAND-CODE      = "?IC"
                         MOVE "SEU1011" TO MSG-ID
                    WHEN COMMAND-CODE      = "?LS"
                         MOVE "SEU1012" TO MSG-ID
                    WHEN COMMAND-CODE      = "?RS"
                         MOVE "SEU1013" TO MSG-ID
                    WHEN COMMAND-CODE      = "?K"
                         MOVE "SEU1014" TO MSG-ID
                    WHEN COMMAND-CODE      = "?KT"
                         MOVE "SEU1015" TO MSG-ID
                    WHEN COMMAND-CODE      = "?KZ"
                         MOVE "SEU1016" TO MSG-ID
                    WHEN COMMAND-CODE      = "?LC"
                     OR  COMMAND-CODE      = "?LCC"
                         MOVE "SEU1017" TO MSG-ID
                    WHEN COMMAND-CODE      = "?MC"
                     OR  COMMAND-CODE      = "?MCC"
                         MOVE "SEU1018" TO MSG-ID
                    WHEN COMMAND-CODE      = "?UC"
                     OR  COMMAND-CODE      = "?UCC"
                         MOVE "SEU1019" TO MSG-ID
                    WHEN COMMAND-CODE      = "?T"
                     OR  COMMAND-CODE      = "?TT"
                         MOVE "SEU1020" TO MSG-ID
                    WHEN COMMAND-CODE      = "?DK"
                         MOVE "SEU1022" TO MSG-ID
                    WHEN COMMAND-CODE      = "?DFD"
                         MOVE "SEU1023" TO MSG-ID
                    WHEN COMMAND-CODE      = "?SORT"
                         MOVE "SEU1024" TO MSG-ID
                    WHEN COMMAND-CODE      = "?VT"
                         MOVE "SEU1025" TO MSG-ID
                    WHEN COMMAND-CODE      = "?DW"
                         MOVE "SEU1026" TO MSG-ID
               WHEN OTHER
                         MOVE "SEU0000" TO MSG-ID
                         PERFORM GET-MESSAGE-FROM-MSGF
                         MOVE MSG-ID TO LT-MSGID
                         MOVE "ULCMSGF" TO LT-MSGFIL
                         MOVE PGM-LIB   TO LT-MSGFIL(11:)
                         COMPUTE LT-DATALEN = LENGTH OF HLPTXT
                         CALL "QUILNGTX"  USING HLPTXT
                                                LT-DATALEN
                                                LT-MSGID
                                                LT-MSGFIL
                                                QUS-EC
                         END-CALL
           END-EVALUATE.

           IF       MSG-ID NOT = "SEU0000"
                    PERFORM  SENDE-NACHRICHT
           END-IF.
           SET      CLEAR-COMMAND TO TRUE.
           SET      NO-UPDATE     TO TRUE.
           SET      DONT-PROCESS  TO TRUE.
           PERFORM  CHANGE-LINE.

       EXC-HLP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERARBEITE PFKEYS F7 (SPLIT/JOIN) ODER F8 (SHIFT)
     ‚*--------------------------------------------------------------
       VERARBEITE-PFKEYS SECTION.
       EXC-PFK-00.

     ‚* EDITOR IST NICHT IM UPDATE-MODUS
           IF       HDR-MODE NOT = "U"
            AND     (   HDR-PFKEY = "7"
                     OR HDR-PFKEY = "8" )
                    GO TO EXC-PFK-90
           END-IF.

     ‚* CURSOR BEFINDET SICH NICHT INNERHALB EINER ZEILE
           IF       (   CURCOL < 1
                     OR CURCOL > ENDCOL )
            AND     (   HDR-PFKEY = "7"
                     OR HDR-PFKEY = "8" )
                    MOVE "SEU0001" TO EXMSGID
                    GO TO EXC-PFK-90
           END-IF.

     ‚* AUSFÜHRUNG PFTASTEN-FUNKTION
           EVALUATE HDR-PFKEY
                    WHEN "7"
                         PERFORM EXECUTE-SPLITJOIN
                    WHEN "8"
                         PERFORM EXECUTE-SHIFT
           END-EVALUATE.

       EXC-PFK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FESTSTELLUNG, OB ZEILEN-SPLIT ODER -JOIN
     ‚*--------------------------------------------------------------
       EXECUTE-SPLITJOIN SECTION.
       EXC-SOJ-00.

           INITIALIZE CONT-CHR.
           INITIALIZE CONT-CHR2.
           MOVE     1 TO RECNO.
           PERFORM  GET-NEXT-LINE.

     ‚* ERMITTLE LINKS UND RECHTS
           PERFORM  CHK-BOUNDARY.

     ‚* FORTSETZUNGSZEICHEN BEI DDS
           IF       KZ  OF FT-FOUND = "2"
            AND     TXT-SRCLINE(7:1) NOT = "*"
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE:1) = "-"
                          OR  TXT-SRCLINE(WRKE:1) = "+"
                              COMPUTE ENDCOL = WRKE - 1
                              PERFORM SUCHE-LETZTES-ZEICHEN
                         END-IF
                    END-IF
           END-IF.

     ‚* UNTERSUCHE ZEILE BEI CL-SOURCEN
           IF       KZ  OF FT-FOUND      = "0"
            AND     PGM OF FT-FOUND      = "P"
     ‚*             KOMMENTAR /*  */
                    MOVE BEGCOL TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC = ZERO
                         MOVE BEGCOL TO WRKC
                    ELSE
                         MOVE WRKC TO BEGCOL
                    END-IF
                    IF   TXT-SRCLINE(WRKC:2) = "/*"
                     AND WRKC < (ENDCOL - 2)
                         COMPUTE BEGCOL = WRKC + 2
                    END-IF
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE - 1:2) = "*/"
                          AND WRKE > (BEGCOL + 2)
                              COMPUTE ENDCOL = WRKE - 2
                              PERFORM SUCHE-LETZTES-ZEICHEN
                              MOVE WRKE TO ENDCOL
                         END-IF
                    END-IF
     ‚*             FORTSETZUNGSZEICHEN BEI CL-SOURCEN
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE:1) = "-"
                          OR  TXT-SRCLINE(WRKE:1) = "+"
                              COMPUTE ENDCOL = WRKE - 1
                              PERFORM SUCHE-LETZTES-ZEICHEN
                         END-IF
                    END-IF
           END-IF.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    MOVE CURCOL TO WRKC
           END-IF.
           COMPUTE  WRKL = ENDCOL - WRKC + 1.

     ‚* IST DER REST DER ZEILE LEER, DANN VERBINDE MIT FOLGEZEILE
           IF       TXT-SRCLINE(WRKC:WRKL) = SPACES
            OR      CURCOL > ENDCOL
                    PERFORM EXECUTE-JOIN
           ELSE
                    PERFORM EXECUTE-SPLIT
           END-IF.

       EXC-SOJ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERBINDE MIT DER NÄCHSTEN ZEILE
     ‚*--------------------------------------------------------------
       EXECUTE-JOIN SECTION.
       EXC-JOI-00.

           MOVE     BEGCOL TO BEGSAVE1.
           MOVE     ENDCOL TO ENDSAVE1.
           INITIALIZE CONT-CHR.

     ‚* LESE FOLGEZEILE
           MOVE     2 TO RECNO.
           PERFORM  GET-NEXT-LINE.
           MOVE     TXT-SRCLINE TO NEU-SRCLINE.
           PERFORM  CHK-BOUNDARY.
           MOVE     BEGCOL TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
                    MOVE BEGCOL TO WRKC
           END-IF.
           MOVE     WRKC TO BEGSAVE2.

     ‚* FORTSETZUNGSZEICHEN BEI DDS
           IF       KZ  OF FT-FOUND = "2"
            AND     TXT-SRCLINE(7:1) NOT = "*"
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE:1) = "-"
                              MOVE TXT-SRCLINE(WRKE:1) TO CONT-CHR
                              COMPUTE ENDCOL = WRKE
                         END-IF
                         IF   TXT-SRCLINE(WRKE:1) = "+"
                              MOVE TXT-SRCLINE(WRKE:1) TO CONT-CHR
                              COMPUTE ENDCOL = WRKE - 1
                         END-IF
                    END-IF
           END-IF.

     ‚* UNTERSUCHE ZEILE BEI CL-SOURCEN
           IF       KZ  OF FT-FOUND      = "0"
            AND     PGM OF FT-FOUND      = "P"
     ‚*             KOMMENTAR /*  */
                    MOVE BEGCOL TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC = ZERO
                         MOVE BEGCOL TO WRKC
                    ELSE
                         MOVE WRKC TO BEGCOL
                    END-IF
                    IF   TXT-SRCLINE(WRKC:2) = "/*"
                     AND WRKC < (ENDCOL - 2)
                         COMPUTE BEGCOL = WRKC + 2
                    END-IF
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE - 1:2) = "*/"
                          AND WRKE > (BEGCOL + 2)
                              COMPUTE ENDCOL = WRKE - 2
                         END-IF
                    END-IF
     ‚*             FORTSETZUNGSZEICHEN BEI CL-SOURCEN
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE:1) = "-"
                          OR  TXT-SRCLINE(WRKE:1) = "+"
                              COMPUTE ENDCOL = WRKE - 1
                              MOVE TXT-SRCLINE(WRKE:1) TO CONT-CHR
                         END-IF
                    END-IF
           END-IF.

           MOVE     ENDCOL TO WRKE.
           PERFORM  SUCHE-LETZTES-ZEICHEN.
           IF       WRKE = ZERO
                    MOVE BEGCOL TO WRKE
           END-IF.
           MOVE     WRKE TO ENDSAVE2.

     ‚* LESE BASISZEILE
           MOVE     1 TO RECNO.
           PERFORM  GET-NEXT-LINE.
           PERFORM  CHK-BOUNDARY.
           MOVE     ENDCOL TO WRKP.
           MOVE     ENDSAVE1 TO WRKE.
           PERFORM  SUCHE-LETZTES-ZEICHEN.
           IF       WRKE = ZERO
                    MOVE BEGCOL TO WRKE
           END-IF.
           COMPUTE  WRKL = ENDSAVE2 - BEGSAVE2 + 1.
           IF       CONT-CHR = "-"
                    SUBTRACT 1 FROM WRKL
           END-IF.
           COMPUTE  WRKX = CURCOL + WRKL - 1.
           IF       WRKX > ENDCOL
                    MOVE "SEU0011" TO EXMSGID
                    SET  NO-UPDATE    TO TRUE
                    SET  DONT-PROCESS TO TRUE
                    PERFORM CHANGE-LINE
                    GO TO EXC-JOI-90
           END-IF.

     ‚* FORTSETZUNGSZEICHEN BEI DDS ENTFERNEN
           IF       KZ  OF FT-FOUND = "2"
            AND     TXT-SRCLINE(7:1) NOT = "*"
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE:1) = "-"
                          OR  TXT-SRCLINE(WRKE:1) = "+"
                              MOVE SPACE TO TXT-SRCLINE(WRKE:1)
                         END-IF
                    END-IF
           END-IF.

     ‚* FORTSETZUNGSZEICHEN BEI CL-SOURCEN ENTFERNEN
           IF       KZ  OF FT-FOUND      = "0"
            AND     PGM OF FT-FOUND      = "P"
     ‚*             KOMMENTAR /*  */
                    MOVE BEGCOL TO WRKC
                    PERFORM SUCHE-ANYZEICHEN
                    IF   WRKC = ZERO
                         MOVE BEGCOL TO WRKC
                    ELSE
                         MOVE WRKC TO BEGCOL
                    END-IF
                    IF   TXT-SRCLINE(WRKC:2) = "/*"
                     AND WRKC < (ENDCOL - 2)
                         COMPUTE BEGCOL = WRKC + 2
                    END-IF
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE - 1:2) = "*/"
                          AND WRKE > (BEGCOL + 2)
                              COMPUTE ENDCOL = WRKE - 2
                         END-IF
                    END-IF
     ‚*             FORTSETZUNGSZEICHEN
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE:1) = "-"
                          OR  TXT-SRCLINE(WRKE:1) = "+"
                              MOVE SPACE TO TXT-SRCLINE(WRKE:1)
                         END-IF
                    END-IF
           END-IF.

     ‚* ÜBERTRAGE DATEN IN NEUE ZEILE
           MOVE     NEU-SRCLINE(BEGSAVE2:WRKL) TO
                    TXT-SRCLINE(CURCOL  :WRKL).
           MOVE     SPACES TO NEU-SRCLINE(BEGSAVE2:WRKL).
           IF       CONT-CHR = "+"
                    COMPUTE  WRKE = CURCOL + WRKL
                    PERFORM  SUCHE-LETZTES-ZEICHEN
                    MOVE CONT-CHR TO TXT-SRCLINE(WRKE + 2:1)
           END-IF.
           IF       CONT-CHR = "-"
                    COMPUTE  WRKX = CURCOL + WRKL
                    MOVE CONT-CHR TO TXT-SRCLINE(WRKX:1)
           END-IF.

     ‚* 1 ODER 2 ZEILEN ZURÜCKSCHREIBEN
           IF       NEU-SRCLINE(BEGSAVE2:WRKL) = SPACES
                    PERFORM CHANGE-LINE
                    MOVE "A000000" TO HDR-SEQINS
                    MOVE 1 TO HDR-RCDSOUT
           ELSE
                    MOVE 1 TO HDR-RCDSOUT
                    PERFORM CHANGE-LINE
                    PERFORM INSERT-LINE
           END-IF.

           IF       EXMSGID = SPACES
                    MOVE "SEU0003" TO EXMSGID
           END-IF.

       EXC-JOI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEILE ZEILE IN ZWEI ZEILEN
     ‚*--------------------------------------------------------------
       EXECUTE-SPLIT SECTION.
       EXC-SPL-00.

           INITIALIZE NEU-SRCLINE.
           INITIALIZE CONT-CHR2.

     ‚* ERMITTLE LINKS UND RECHTS
           PERFORM  CHK-BOUNDARY.

     ‚* FORTSETZUNGSZEICHEN BEI DDS ENTFERNEN
           IF       KZ  OF FT-FOUND = "2"
            AND     TXT-SRCLINE(7:1) NOT = "*"
                    MOVE "-" TO CONT-CHR
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE:1) = "+"
                              MOVE TXT-SRCLINE(WRKE:1) TO CONT-CHR2
                              MOVE SPACE TO TXT-SRCLINE(WRKE:1)
                         END-IF
                    END-IF
           END-IF.

     ‚* FORTSETZUNGSZEICHEN BEI CL-SOURCEN ENTFERNEN
           IF       KZ  OF FT-FOUND      = "0"
            AND     PGM OF FT-FOUND      = "P"
                    MOVE "+" TO CONT-CHR
     ‚*             KOMMENTAR /*  */
                    MOVE     BEGCOL TO WRKC
                    PERFORM  SUCHE-ANYZEICHEN
                    IF       WRKC = ZERO
                             MOVE BEGCOL TO WRKC
                    END-IF
                    IF   TXT-SRCLINE(WRKC:2) = "/*"
                     AND WRKC < (ENDCOL - 2)
                         COMPUTE BEGCOL = WRKC + 2
                         INITIALIZE CONT-CHR
                    END-IF
                    MOVE     ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE - 1:2) = "*/"
                          AND WRKE > (BEGCOL + 2)
                              COMPUTE ENDCOL = WRKE - 2
                              INITIALIZE CONT-CHR
                         END-IF
                    END-IF
     ‚*             FORTSETZUNGSZEICHEN BEI CL-SOURCEN ENTFERNEN
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         IF   TXT-SRCLINE(WRKE:1) = "-"
                          OR  TXT-SRCLINE(WRKE:1) = "+"
                              MOVE TXT-SRCLINE(WRKE:1) TO CONT-CHR2
                              MOVE SPACE TO TXT-SRCLINE(WRKE:1)
                         END-IF
                    END-IF
           END-IF.

     ‚* ÜBERNEHME RECHTE UND LINKE SEITE IN NEUE ZEILE
           IF       BEGCOL > 1
                    COMPUTE WRKL = BEGCOL - 1
                    MOVE TXT-SRCLINE(1:WRKL) TO
                         NEU-SRCLINE(1:WRKL)
           END-IF.
           IF       ENDCOL < HDR-RCDLEN
                    COMPUTE WRKL = HDR-RCDLEN - ENDCOL
                    MOVE TXT-SRCLINE(ENDCOL + 1:WRKL) TO
                         NEU-SRCLINE(ENDCOL + 1:WRKL)
           END-IF.
           IF       CONT-CHR2 = "+"
            AND     ENDCOL > BEGCOL
                    MOVE ENDCOL TO WRKE
                    PERFORM  SUCHE-LETZTES-ZEICHEN
                    IF   WRKE > BEGCOL
                         MOVE CONT-CHR2 TO TXT-SRCLINE(WRKE + 2:1)
                    END-IF
           END-IF.
           IF       CONT-CHR2 = "-"
            AND     ENDCOL > BEGCOL
                    MOVE ENDCOL TO WRKX
                    IF   WRKX > BEGCOL
                         MOVE CONT-CHR2 TO TXT-SRCLINE(WRKX:1)
                    END-IF
           END-IF.

     ‚* ERMITTLE ZIEL-POSITION
           INITIALIZE WRK-FELD12.
           MOVE     BEGCOL TO WRKC.
           EVALUATE TRUE
             WHEN   PGM OF FT-FOUND = "C"
              AND   TXT-SRCLINE(7:1) NOT = "*"
                    PERFORM COBOL-AUSRICHTEN
             WHEN   PGM OF FT-FOUND = "R"
              AND   TXT-SRCLINE(7:1) NOT = "*"
                    MOVE 36 TO WRKC
             WHEN   KZ  OF FT-FOUND = "2"
              AND   TXT-SRCLINE(7:1) NOT = "*"
                    MOVE 45 TO WRKC
             WHEN   OTHER
                    PERFORM SUCHE-ANYZEICHEN
           END-EVALUATE.
           IF       WRKC = ZEROES
                    MOVE BEGCOL TO WRKC
           END-IF.
           MOVE     ENDCOL TO WRKE.
           PERFORM  SUCHE-LETZTES-ZEICHEN.
           IF       WRKE < CURCOL
                    MOVE CURCOL TO WRKE
           END-IF.
           COMPUTE  WRKL = WRKE - CURCOL + 1.
           IF       (WRKC + WRKL - 1) > ENDCOL
                    MOVE "SEU0011" TO EXMSGID
                    SET  NO-UPDATE    TO TRUE
                    SET  DONT-PROCESS TO TRUE
                    PERFORM CHANGE-LINE
                    GO TO EXC-SPL-90
           END-IF.

     ‚* ÜBERTRAGE ABGESPLITTETEN TEXT IN NEUE ZEILE
           IF       PGM OF FT-FOUND = "C"
              AND   TXT-SRCLINE(7:1) NOT = "*"
                    IF   WRK-FELD12 = "IF"
                     OR  WRK-FELD12 = "OR"
                     OR  WRK-FELD12 = "AND"
                     OR  WRK-FELD12 = "ELSE"
                         MOVE WRKC   TO WRKP
                         MOVE CURCOL TO WRKC
                         PERFORM SUCHE-ANYZEICHEN
                         IF   WRKC = ZEROES
                              MOVE CURCOL TO WRKC
                         END-IF
                         MOVE WRKC TO CURCOL
                         MOVE 1 TO WRKL
                         PERFORM SUCHE-LEERSTELLE
                         IF   WRKC = ZEROES
                              COMPUTE WRKC = CURCOL
                              INSPECT WRK-FELD12 TALLYING WRKC
                                      FOR CHARACTERS BEFORE SPACE
                         END-IF
                         PERFORM SUCHE-ANYZEICHEN
                         IF   WRKC = ZEROES
                              COMPUTE WRKC = CURCOL
                              INSPECT WRK-FELD12 TALLYING WRKC
                                      FOR CHARACTERS BEFORE SPACE
                         END-IF
                         INITIALIZE WRKL
                         INSPECT WRK-FELD12 TALLYING WRKL
                                 FOR CHARACTERS BEFORE SPACE
                         MOVE WRK-FELD12 TO NEU-SRCLINE(WRKP:WRKL)
                         MOVE SPACES TO TXT-SRCLINE(CURCOL:WRKL)
                         MOVE WRKC TO CURCOL
                         COMPUTE WRKC = WRKP + 5
                         COMPUTE WRKL = ENDCOL - CURCOL + 1
                    END-IF
           END-IF.
           MOVE     TXT-SRCLINE(CURCOL:WRKL) TO
                    NEU-SRCLINE(WRKC  :WRKL).
           MOVE     SPACES TO TXT-SRCLINE(CURCOL:WRKL).

     ‚* FORTSETZUNGSZEICHEN ANHÄNGEN
           IF       CONT-CHR = "+"
                    PERFORM  CHK-BOUNDARY
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE = ZERO
                         MOVE BEGCOL TO WRKE
                    END-IF
                    IF   TXT-SRCLINE(WRKE:1) = "+"
                     OR  TXT-SRCLINE(WRKE:1) = "-"
                         MOVE SPACES TO TXT-SRCLINE(WRKE:1)
                    END-IF
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE = ZERO
                         MOVE BEGCOL TO WRKE
                    END-IF
                    MOVE CONT-CHR TO TXT-SRCLINE(WRKE + 2:1)
           END-IF.
           IF       CONT-CHR = "-"
                    MOVE CURCOL TO WRKX
                    MOVE CONT-CHR TO TXT-SRCLINE(WRKX:1)
           END-IF.

     ‚* ÄNDEREN DER GESPLITTETEN ZEILE
           PERFORM  CHANGE-LINE.
           MOVE     1 TO HDR-RCDSOUT.

     ‚* HINZUFÜGEN DER NEUEN ZEILE
           PERFORM  INSERT-LINE.

           IF       EXMSGID = SPACES
                    MOVE "SEU0002" TO EXMSGID
           END-IF.

       EXC-SPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSRICHTEN DER ABGESPLITTETEN ZEILE FÜR COBOL-STATEMENTS
     ‚*--------------------------------------------------------------
       COBOL-AUSRICHTEN SECTION.
       COB-AUS-00.

     ‚* ERMITTLE BEGINN DER DATEN IN DER ZEILE
           MOVE     BEGCOL TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC < BEGCOL
                    MOVE 21 TO BEGCOL
                    GO TO COB-AUS-90
           END-IF.
           MOVE     WRKC TO BEGSAVE1.

     ‚* PRÜFE, OB 1. WORT DER ZEILE EIN COBOL-BEFEHL IST
           MOVE     1 TO WRKL.
           PERFORM  SUCHE-LEERSTELLE.
           IF       WRKC = ZEROES
                    COMPUTE WRKC = ENDCOL + 1
           END-IF.
           COMPUTE  WRKL = WRKC - BEGSAVE1.
           MOVE     TXT-SRCLINE(BEGSAVE1:WRKL) TO WRK-FELD12.
           MOVE     WRK-FELD12 TO WRK-FELD10.
           PERFORM  CHK-COBOLCMD.
           IF       WRKL = ZEROES
            OR      WRK-FELD12(1:4) = "END-"
                    MOVE BEGSAVE1 TO WRKC
           ELSE
                    COMPUTE WRKC = BEGSAVE1 + WRKL + 1
           END-IF.
           MOVE     WRKC TO ENDSAVE1.

     ‚* PRÜFE, OB 1. WORT NACH CURSOR EIN COBOL-BEFEHL IST
           MOVE     CURCOL TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC < CURCOL
                    MOVE ENDSAVE1 TO WRKC
                    GO TO COB-AUS-90
           END-IF.
           MOVE     WRKC TO BEGSAVE2.
           MOVE     1 TO WRKL.
           PERFORM  SUCHE-LEERSTELLE.
           IF       WRKC = ZEROES
                    COMPUTE WRKC = ENDCOL + 1
           END-IF.
           COMPUTE  WRKL = WRKC - BEGSAVE2.
           MOVE     TXT-SRCLINE(BEGSAVE2:WRKL) TO WRK-FELD12.
           IF       WRK-FELD12(1:4) = "WHEN"
                    INITIALIZE WRK-FELD12
           END-IF
           IF       WRK-FELD12(1:4) = "END-"
            OR      WRK-FELD12(1:4) = "ELSE"
                    IF   WRK-FELD10 = "IF"
                     OR  WRK-FELD10 = "ELSE"
                         COMPUTE WRKC = BEGSAVE1
                    ELSE
                         COMPUTE WRKC = BEGSAVE1 - 5
                         IF   WRKC < 21
                              MOVE 12 TO WRKC
                         END-IF
                    END-IF
           ELSE
                    PERFORM CHK-COBOLCMD
                    IF   WRKL = ZEROES
                         MOVE ENDSAVE1 TO WRKC
                         INITIALIZE WRK-FELD12
                    END-IF
                    IF   WRKL > ZEROES
                     AND WRK-FELD10 = "WHEN"
                         MOVE ENDSAVE1 TO WRKC
                         INITIALIZE WRK-FELD12
                         INITIALIZE WRKL
                    END-IF
                    IF   WRKL > ZEROES
                     AND ENDSAVE1 > BEGSAVE1
                         COMPUTE WRKC = BEGSAVE1
                    END-IF
                    IF   WRKL > ZEROES
                     AND ENDSAVE1 = BEGSAVE1
                         COMPUTE WRKC = BEGSAVE1 - WRKL - 1
                    END-IF
                    IF   WRK-FELD10 = "IF"
                     OR  WRK-FELD10 = "OR"
                     OR  WRK-FELD10 = "AND"
                     OR  WRK-FELD10 = "ELSE"
                         COMPUTE WRKC = BEGSAVE1 + 5
                    END-IF
           END-IF.

     ‚* B-BEREICH AUF SPALTE 12, PARAMETER AUF SPALTE 21
           IF       WRKC < 12
                    MOVE 12 TO WRKC
           END-IF.
           IF       WRKC > 12
            AND     WRKC < 21
                    MOVE 21 TO WRKC
           END-IF.
           IF       WRKC > 21
            AND     WRKC < 26
            AND     WRK-FELD10 NOT = SPACES
                    MOVE 26 TO WRKC
           END-IF.

       COB-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSCHIEBE ANHAND DER CURSOR-POSITION
     ‚*--------------------------------------------------------------
       EXECUTE-SHIFT SECTION.
       EXC-SFT-00.

           MOVE     1 TO RECNO.
           PERFORM  GET-NEXT-LINE.

     ‚* ERMITTLE LINKS UND RECHTS
           PERFORM  CHK-BOUNDARY.

     ‚* BERÜCKSICHTIGE CURSOR-POSITION
           IF       CURCOL < BEGCOL
            OR      CURCOL > ENDCOL
                    GO TO EXC-SFT-90
           END-IF.

     ‚* 1. PRÜFUNG: CURSOR RECHTS NEBEN DEM LETZTEN ZEICHEN
           MOVE     CURCOL TO WRKC.
           PERFORM  SUCHE-ANYZEICHEN.
           MOVE     WRKC TO BEGSAVE1.
           IF       WRKC = ZEROES
     ‚*             SUCHE LETZTES ZEICHEN
                    MOVE ENDCOL TO WRKE
                    PERFORM SUCHE-LETZTES-ZEICHEN
                    IF   WRKE < BEGCOL
                         GO TO EXC-SFT-90
                    END-IF
                    MOVE WRKE TO ENDSAVE1
     ‚*             SUCHE BEGINN (TRENNUNG 1 SPACE)
                    MOVE WRKE TO WRKC
                    MOVE SPACES TO WRK-FELD50
                    MOVE 1 TO WRKL
                    PERFORM SUCHE-ZEICHEN-REVERSE
                    IF   WRKC < BEGCOL
                         MOVE BEGCOL TO WRKC
                    ELSE
                         ADD 1 TO WRKC
                    END-IF
                    COMPUTE WRKL = WRKE - WRKC + 1
                    COMPUTE WRKP = CURCOL - WRKC
                    IF   WRKP > 1
     ‚*                  SUCHE BEGINN (TRENNUNG 2 SPACES)
                         MOVE WRKE TO WRKC
                         MOVE SPACES TO WRK-FELD50
                         MOVE 2 TO WRKL
                         PERFORM SUCHE-ZEICHEN-REVERSE
                         IF   WRKC < BEGCOL
                              MOVE BEGCOL TO WRKC
                         ELSE
                              ADD 2 TO WRKC
                         END-IF
                         COMPUTE WRKL = WRKE - WRKC + 1
                    END-IF
                    COMPUTE WRKP = CURCOL - WRKL + 1
                    MOVE TXT-SRCLINE TO NEU-SRCLINE
                    MOVE SPACES TO TXT-SRCLINE(WRKC:WRKL)
                    MOVE NEU-SRCLINE(WRKC:WRKL) TO
                         TXT-SRCLINE(WRKP:WRKL)
                    GO TO EXC-SFT-80
           END-IF.

     ‚* 2. PRÜFUNG: CURSOR LINKS NEBEN DEM ERSTEN ZEICHEN
     ‚*    SUCHE BEGINN (TRENNUNG 1 SPACE)
           IF       CURCOL > BEGCOL
            AND     TXT-SRCLINE(CURCOL - 1:1) NOT = SPACE
                    COMPUTE WRKC = CURCOL - 1
                    PERFORM SUCHE-ANYZEICHEN
                    MOVE WRKC TO BEGSAVE1
           END-IF.
           MOVE     BEGSAVE1 TO WRKC.
           MOVE     SPACES TO WRK-FELD50.
           MOVE     1 TO WRKL.
           PERFORM  SUCHE-ZEICHEN-REVERSE.
           IF       WRKC < BEGCOL
                    MOVE BEGCOL TO WRKC
           ELSE
                    ADD 1 TO WRKC
           END-IF.
           MOVE     WRKC TO BEGSAVE2.
           COMPUTE  WRKP = CURCOL - WRKC.
           IF       WRKP = ZEROES
                    MOVE BEGSAVE1 TO WRKC
                    MOVE SPACES TO WRK-FELD50
                    MOVE 2 TO WRKL
                    PERFORM SUCHE-ZEICHEN-REVERSE
                    IF   WRKC < BEGCOL
                         MOVE BEGCOL TO WRKC
                    ELSE
                         ADD 1 TO WRKC
                    END-IF
                    MOVE WRKC TO BEGSAVE2
                    COMPUTE WRKP = CURCOL - WRKC
           END-IF.
           IF       WRKP > +1
            OR      WRKP < -1
     ‚*             SUCHE BEGINN (TRENNUNG X+1 SPACES)
                    MOVE BEGSAVE2 TO WRKC
                    MOVE SPACES TO WRK-FELD50
                    IF   WRKP < ZEROES
                         COMPUTE WRKL = WRKP * -1 + 1
                    ELSE
                         COMPUTE WRKL = 2
                    END-IF
                    PERFORM SUCHE-ZEICHEN-REVERSE
                    IF   WRKC = ZEROES
                     AND WRKL > 1
     ‚*                  SHIFT ZUM ZEILENBEGINN?
                         MOVE BEGCOL TO WRKC
                         SUBTRACT 1 FROM WRKL
                         IF   TXT-SRCLINE(WRKC:WRKL) NOT =
                              WRK-FELD50(1:WRKL)
                              INITIALIZE WRKC
                         END-IF
                    END-IF
                    IF   WRKC < BEGCOL
                         MOVE BEGCOL TO WRKC
                    ELSE
                         ADD WRKL TO WRKC
                    END-IF
                    MOVE WRKC TO BEGSAVE2
     ‚*             SUCHE ENDE (TRENNUNG X+1 SPACES)
                    MOVE BEGSAVE1 TO WRKC
                    MOVE SPACES TO WRK-FELD50
                    IF   WRKP > ZEROES
                         COMPUTE WRKL = WRKP + 1
                    ELSE
                         COMPUTE WRKL = 2
                    END-IF
                    PERFORM SUCHE-ZEICHEN
                    IF   WRKC = ZEROES
                     AND WRKL > 1
     ‚*                  SHIFT ANS ZEILENENDE?
                         SUBTRACT 1 FROM WRKL
                         COMPUTE WRKC = ENDCOL - WRKL + 1
                         IF   TXT-SRCLINE(WRKC:WRKL) NOT =
                              WRK-FELD50(1:WRKL)
                              INITIALIZE WRKC
                         END-IF
                    END-IF
                    IF   WRKC > ENDCOL
                     OR  WRKC = ZEROES
                         MOVE ENDCOL TO WRKC
                    ELSE
                         SUBTRACT 1 FROM WRKC
                    END-IF
                    MOVE WRKC TO ENDSAVE2
           ELSE
     ‚*             SUCHE BEGINN (TRENNUNG 1+1 SPACE)
                    MOVE BEGSAVE1 TO WRKC
                    MOVE SPACES TO WRK-FELD50
                    IF   WRKP < ZEROES
                         COMPUTE WRKL = 2
                    ELSE
                         COMPUTE WRKL = 1
                    END-IF
                    PERFORM SUCHE-ZEICHEN-REVERSE
                    IF   WRKC = ZEROES
                     AND WRKL > 1
     ‚*                  SHIFT ZUM ZEILENBEGINN?
                         MOVE BEGCOL TO WRKC
                         SUBTRACT 1 FROM WRKL
                         IF   TXT-SRCLINE(WRKC:WRKL) NOT =
                              WRK-FELD50(1:WRKL)
                              INITIALIZE WRKC
                         END-IF
                    END-IF
                    IF   WRKC < BEGCOL
                         MOVE BEGCOL TO WRKC
                    ELSE
                         ADD WRKL TO WRKC
                    END-IF
                    MOVE WRKC TO BEGSAVE2
     ‚*             SUCHE ENDE (TRENNUNG 1+1 SPACES)
                    COMPUTE WRKP = CURCOL - WRKC
                    MOVE BEGSAVE1 TO WRKC
                    MOVE SPACES TO WRK-FELD50
                    IF   WRKP > ZEROES
                         COMPUTE WRKL = WRKP + 1
                    ELSE
                         COMPUTE WRKL = 1
                    END-IF
                    PERFORM SUCHE-ZEICHEN
                    IF   WRKC = ZEROES
                     AND WRKL > 1
     ‚*                  SHIFT ANS ZEILENENDE?
                         MOVE BEGSAVE1 TO WRKC
                         SUBTRACT 1 FROM WRKL
                         COMPUTE WRKC = ENDCOL - WRKL + 1
                         IF   TXT-SRCLINE(WRKC:WRKL) NOT =
                              WRK-FELD50(1:WRKL)
                              INITIALIZE WRKC
                         END-IF
                    END-IF
                    IF   WRKC > ENDCOL
                     OR  WRKC = ZEROES
                         MOVE ENDCOL TO WRKC
                    ELSE
                         SUBTRACT 1 FROM WRKC
                    END-IF
                    MOVE WRKC TO ENDSAVE2
           END-IF.
           COMPUTE  WRKL = ENDSAVE2 - BEGSAVE2 + 1.
           MOVE     CURCOL   TO WRKP.
           MOVE     BEGSAVE2 TO WRKC.
           IF       (WRKP + WRKL - 1) > ENDCOL
                    MOVE "SEU0011" TO EXMSGID
                    GO TO  EXC-SFT-90
           END-IF.
           MOVE     TXT-SRCLINE TO NEU-SRCLINE.
           MOVE     SPACES TO TXT-SRCLINE(WRKC:WRKL).
           MOVE     NEU-SRCLINE(WRKC:WRKL) TO TXT-SRCLINE(WRKP:WRKL).

       EXC-SFT-80.
           PERFORM  CHANGE-LINE.
           MOVE     1 TO HDR-RCDSOUT.

           IF       EXMSGID = SPACES
                    MOVE "SEU0008" TO EXMSGID
           END-IF.

       EXC-SFT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTLE TEXT-BEREICH BEI KOMMENTAREN
     ‚*--------------------------------------------------------------
       CHK-BOUNDARY SECTION.
       CHK-BND-00.

     ‚* ANFANG- UND ENDE-POSITION
           MOVE     1                TO BEGCOL.
           MOVE     HDR-RCDLEN       TO ENDCOL.

     ‚* PROGRAMME UND DEFINITIONEN BEGINNEN AB 7
           IF       KZ OF FT-FOUND = "1"
            OR      KZ OF FT-FOUND = "2"
                    MOVE 7           TO BEGCOL
           END-IF.

     ‚* ENDE-POSITION JE NACH PROGRAMM-SPRACHE
           EVALUATE PGM OF FT-FOUND
                    WHEN "C"
                         SUBTRACT 6  FROM ENDCOL
                    WHEN "R"
                         SUBTRACT 8  FROM ENDCOL
                    WHEN OTHER
                         MOVE HDR-RCDLEN  TO ENDCOL
           END-EVALUATE.

     ‚* ENDE-POSITION AN BILDSCHIRM-GRÖßE ANPASSEN
           IF       KZ OF FT-FOUND = "1"
            AND     ENDCOL < 80
            AND     ENDCOL > 70
                    MOVE 71          TO ENDCOL
           END-IF.

     ‚* SCHÜTZE "*" BEI KOMMENTAREN
           IF       TXT-SRCLINE(BEGCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    ADD 1 TO BEGCOL
           END-IF.

     ‚* SCHÜTZE "/*" BEI KOMMENTAREN
           EVALUATE TRUE
             WHEN   TXT-SRCLINE(BEGCOL:2) = "/*"
              AND   KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
             WHEN   TXT-SRCLINE(BEGCOL:2) = "--"
              AND   KZ  OF FT-FOUND = "0"
                    ADD 2 TO BEGCOL
           END-EVALUATE.

     ‚* ERMITTLE GRENZEN BEI KOMMENTAREN
           IF       TXT-SRCLINE(7:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    CONTINUE
           ELSE
                    GO TO CHK-BND-90
           END-IF.

     ‚* RECHTER STERN
           IF       TXT-SRCLINE(ENDCOL:1) = "*"
            AND     (    KZ  OF FT-FOUND = "1"
                     OR  KZ  OF FT-FOUND = "2" )
                    SUBTRACT 1 FROM ENDCOL
           END-IF.

     ‚* FINDE BEGINN DER DATEN
           MOVE     BEGCOL TO WRKC.
     ‚*    EINE LEERSTELLE IST MÖGLICH
           PERFORM  SUCHE-ANYZEICHEN.
           IF       WRKC = ZERO
            OR      WRKC > ENDCOL
                    GO TO CHK-BND-90
           END-IF.
           COMPUTE  WRKL = WRKC - BEGCOL.
           IF       WRKL > 1
                    COMPUTE WRKC = BEGCOL + WRKL
           END-IF.
           MOVE     WRKC TO WRKK.
     ‚*    PRÜFE BIS ZUR 1. LEERSTELLE AUF SONDERZ.
           MOVE     1 TO WRKL.
           PERFORM  SUCHE-LEERSTELLE.
           IF       WRKC = ZERO
            OR      WRKC > ENDCOL
                    GO TO CHK-BND-90
           END-IF.
           COMPUTE  WRKL = WRKC - WRKK.
           IF       WRKL > 20
                    GO TO CHK-BND-90
           END-IF.
     ‚*    BEGINN IST 1. NICHT-SONDERZEICHEN
           MOVE     WRKK TO WRKX.
           INITIALIZE WRKP.
           PERFORM  UNTIL WRKX > (WRKK + WRKL)
             OR     WRKP > ZEROES
                    IF   TXT-SRCLINE(WRKX:1) IS ALPHABETIC
                     AND TXT-SRCLINE(WRKX:1) NOT = SPACE
                     OR  TXT-SRCLINE(WRKX:1) IS NUMERIC
                     OR  TXT-SRCLINE(WRKX:1) IS UMLAUTE
                         MOVE WRKX TO WRKP
                    ELSE
                         ADD 1 TO WRKX
                    END-IF
           END-PERFORM.
           IF       WRKP = WRKK
                    GO TO CHK-BND-90
           END-IF.
           IF       WRKP = ZEROES
                    COMPUTE BEGCOL = WRKK + WRKL
           ELSE
                    MOVE WRKP TO BEGCOL
           END-IF.

     ‚* FINDE ENDE DER DATEN
           MOVE     ENDCOL TO WRKE.
           PERFORM  SUCHE-LETZTES-ZEICHEN.
     ‚*    PRÜFE BIS ZUR 1. LEERSTELLE AUF SONDERZ.
           IF       WRKE = ZERO
            OR      WRKE < BEGCOL
                    GO TO CHK-BND-90
           END-IF.
           COMPUTE  WRKL = ENDCOL - WRKE.
           IF       WRKL > 20
                    GO TO CHK-BND-90
           END-IF.
           MOVE     WRKE TO WRKC.
           MOVE     1 TO WRKL.
           INITIALIZE WRK-FELD50.
           PERFORM  SUCHE-ZEICHEN-REVERSE.
           COMPUTE  WRKL = WRKE - WRKC.
           IF       WRKL > 20
                    GO TO CHK-BND-90
           END-IF.
     ‚*    ENDE IST 1. NICHT-SONDERZEICHEN
           MOVE     WRKE TO WRKX.
           INITIALIZE WRKP.
           PERFORM  UNTIL WRKX < (WRKE - WRKL)
             OR     WRKP > ZEROES
                    IF   TXT-SRCLINE(WRKX:1) IS ALPHABETIC
                     AND TXT-SRCLINE(WRKX:1) NOT = SPACE
                     OR  TXT-SRCLINE(WRKX:1) IS NUMERIC
                     OR  TXT-SRCLINE(WRKX:1) = SPACE
                     OR  TXT-SRCLINE(WRKX:1) IS UMLAUTE
                         MOVE WRKX TO WRKP
                    ELSE
                         SUBTRACT 1 FROM WRKX
                    END-IF
           END-PERFORM.
           IF       WRKP = ZEROES
                    COMPUTE ENDCOL = WRKE - WRKL + 1
           ELSE
                    MOVE WRKP TO ENDCOL
           END-IF.

       CHK-BND-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFE, OB COBOL-BEFEHL
     ‚*--------------------------------------------------------------
       CHK-COBOLCMD SECTION.
       CHK-COB-00.

           INITIALIZE WRKX.

     ‚* BEFEHL ZU PRÜFEN ?
           IF       WRK-FELD12 = SPACES
                    INITIALIZE WRKL
                    GO TO CHK-COB-90
           END-IF.

     ‚* SUCHE COBOL-BEFEHL
           INITIALIZE WRKX.
           PERFORM  VARYING CMDX FROM 1 BY 1
                    UNTIL CMDX > CMD-ANZAHL
                     OR   WRKX > ZEROES
                    IF   CMD-NAME OF CMD-EINTRAG(CMDX) = WRK-FELD12
                         MOVE CMDX TO WRKX
                    END-IF
           END-PERFORM.
           IF       WRKX = ZEROES
                    INITIALIZE WRKL
           END-IF.

       CHK-COB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDE NACHRICHT AN USER
     ‚*--------------------------------------------------------------
       SENDE-NACHRICHT SECTION.
       SND-MSG-00.

           IF       MSG-ID NOT = SPACES
            AND     1 = 2
                    MOVE     MSG-ID         TO PM-MSGID
                    CALL     "QMHSNDPM"  USING PM-MSGID
                                               PM-MSGFIL
                                               PM-MSGDATA
                                               PM-MSGDATALEN
                                               PM-MSGTYP
                                               PM-MSGSTACKENT
                                               PM-MSGSTACKCNT
                                               PM-MSGKEY
                                               QUS-EC
                    END-CALL
                    INITIALIZE MSG-ID
           END-IF.

       SND-MSG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SYSTEM-BEFEHL AUSFÜHREN (ÜBER CL-PROGRAMM)
     ‚*--------------------------------------------------------------
       EXEC-SYSTEMBEFEHL SECTION.
       EXC-CMD-00.

           PERFORM  AUFRUF-CLPGM.
           GO TO    EXC-CMD-90.

     ‚* WELCHER COMMAND SOLL AUSGEFÜHRT WERDEN ?
           EVALUATE EXC-COMMAND
                    WHEN "RTVMBRD"
     ‚*                  TEIDATEIBESCHREIBUNG AUFFINDEN
                         CALL "CLP0002"   IN LIBRARY PGM-LIB
                                          USING RTVMBRD-REC
                                                RET-CODE
                         END-CALL
     ‚*                  DATENBEREICH ERSTELLEN
                    WHEN "CRTDTAARA"
                         CALL "CLP0067"   IN LIBRARY PGM-LIB
                                          USING CRTDTAARA-REC
                                                RET-CODE
                         END-CALL
     ‚*                  ERMITTELN PROGRAMMTEXT
                    WHEN "LGPGM008"
      *TODO
     ‚*                  OBJEKTBESCHREIBUNG AUFFINDEN
                    WHEN "RTVOBJD"
                         CALL "CLP0006" IN LIBRARY PGM-LIB
                                          USING RTVOBJD-REC
                                                RET-CODE
                         END-CALL
     ‚*                  DATENBEREICH AUFFINDEN
                    WHEN "RTVDTAARA"
                         CALL "CLP0068"   IN LIBRARY PGM-LIB
                                          USING RTVDTAARA-REC
                                                RET-CODE
                         END-CALL
     ‚*                  DATENBEREICH ÄNDERN
                    WHEN "CHGDTAARA"
                         CALL "CLP0069"   IN LIBRARY PGM-LIB
                                          USING RTVDTAARA-REC
                                                RET-CODE
                         END-CALL
           END-EVALUATE.

     ‚* SONST FEHLER
           IF       EXC-RC = "X"
                    MOVE "SEU0099"      TO EXMSGID
                    SET  NO-UPDATE      TO TRUE
           END-IF.

       EXC-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFRUF HILFSPROGRAMME
     ‚*--------------------------------------------------------------
       AUFRUF-CLPGM SECTION.
       AUF-CLP-00.

           INITIALIZE EXC-RC.
           CALL     "CPPGM70CL" IN LIBRARY PGM-LIB
                                USING   EXC-COMMAND
                                        EXC-FILE
                                        EXC-LIB
                                        EXC-MBR
                                        EXC-TYPE
                                        EXC-TEXT
                                        EXC-TXT3000
                                        EXC-LNG
                                        EXC-RC
                    ON EXCEPTION
                       MOVE "X"         TO EXC-RC
           END-CALL.

       AUF-CLP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERWEITERE EDITOR-USERSPACE
     ‚*--------------------------------------------------------------
       EXTEND-USERSPACE SECTION.
       EXT-USP-00.

           IF       EXTNO NOT > ZEROES
                    GO TO EXT-USP-90
           END-IF.

           COMPUTE  USRSPC-SIZE = HDR-RCDLEN
                                + LINEPRFX-LEN.

     ‚* ERMITTLE GRÖßE DES USER-DATASPACES
           MOVE     OC1-STRUCT            TO US-DATA OF QUSRUSAT-PARAM.
           MOVE     LENGTH OF OC1-STRUCT  TO US-DATALEN
                                                     OF QUSRUSAT-PARAM.

           CALL     "QUSRUSAT"  USING US-DATA      OF QUSRUSAT-PARAM
                                      US-DATALEN   OF QUSRUSAT-PARAM
                                      US-FORMAT    OF QUSRUSAT-PARAM
                                      USRSPC-NAME
                                      QUS-EC
           END-CALL.
           MOVE     US-DATA          OF QUSRUSAT-PARAM TO
                    SPCA0100-STRUCT  OF QUSRUSAT-PARAM.

     ‚* ERWEITERE UM ANGEGEBENE SATZ-ZAHL
           IF       SPC-AVAIL < (EXTNO * USRSPC-SIZE)
                    COMPUTE OC1-SIZE1 = SPC-SIZE
                                      + (EXTNO * USRSPC-SIZE)
                    END-COMPUTE
                    MOVE OC1-STRUCT            TO US-DATA
                                                     OF QUSRUSAT-PARAM
                    MOVE LENGTH OF OC1-STRUCT  TO US-DATALEN
                                                     OF QUSRUSAT-PARAM

                    CALL "QUSCUSAT"   USING
                                      US-LIB       OF QUSRUSAT-PARAM
                                      USRSPC-NAME
                                      US-DATA      OF QUSRUSAT-PARAM
                                      QUS-EC
                    END-CALL
           END-IF.

       EXT-USP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGE MSG-TABELLE
     ‚*--------------------------------------------------------------
       ANZEIGE-MSGTAB SECTION.
       ANZ-MSG-00.

           IF       MSG-ANZAHL NOT > ZEROES
                    GO TO ANZ-MSG-90
           END-IF.

     ‚* ERMITTLE NAMEN DER MESSAGE-FILE
           MOVE     MSG-ID         TO LT-MSGID.
           MOVE     "LFSMSGF"      TO LT-MSGFIL.
           MOVE     "*LIBL"        TO LT-MSGFIL(11:)
           IF       MSG-ID(1:3)    =  "CPF"
                    MOVE "QCPFMSG" TO LT-MSGFIL
                    MOVE "*LIBL"   TO LT-MSGFIL(11:)
           END-IF.
           IF       MSG-ID(1:3)    =  "SEU"
                    MOVE "ULCMSGF" TO LT-MSGFIL
                    MOVE PGM-LIB   TO LT-MSGFIL(11:)
           END-IF.

     ‚* ERMITTLE MAX.LÄNGE DER NACHRICHT
           COMPUTE  LT-DATALEN = LENGTH OF MSG-EINTRAG * MSG-ANZAHL.

     ‚* ANZEIGE DER DATEN
           CALL     "QUILNGTX"  USING MSG-EINTRAEGE
                                      LT-DATALEN
                                      LT-MSGID
                                      LT-MSGFIL
                                      QUS-EC
           END-CALL.

       ANZ-MSG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE VARIABLEN BEI PROGRAMMSTART INITIALISIEREN
     ‚*--------------------------------------------------------------
       INIT-VARS SECTION.
       INI-VAR-00.

           INITIALIZE MSG-ID.
           INITIALIZE MSG-TXT.
           INITIALIZE HLPTXT.
           INITIALIZE EXMSGID.
           INITIALIZE LCLDATA.
           INITIALIZE AT-TABELLE.
           INITIALIZE FT-TABELLE.
           INITIALIZE CMD-TABELLE.
           INITIALIZE ISS-TABELLE.
           INITIALIZE MSG-TABELLE.
           INITIALIZE SRC-TABELLE.
           MOVE     AT-CODES  TO AT-EINTRAEGE.
           COMPUTE  AT-ANZAHL = LENGTH OF AT-CODES
                              / LENGTH OF AT-EINTRAG
           END-COMPUTE.
           MOVE     FT-CODES  TO FT-EINTRAEGE.
           COMPUTE  FT-ANZAHL = LENGTH OF FT-CODES
                              / LENGTH OF FT-EINTRAG
           END-COMPUTE.
           MOVE     CMD-CODES  TO CMD-EINTRAEGE.
           COMPUTE  CMD-ANZAHL = LENGTH OF CMD-CODES
                               / LENGTH OF CMD-EINTRAG
           END-COMPUTE.
           MOVE     ISS-TEXT   TO ISS-EINTRAEGE.
           COMPUTE  ISS-ANZAHL = LENGTH OF ISS-TEXT
                               / LENGTH OF ISS-EINTRAG
           END-COMPUTE.
           MOVE     ALL "9" TO ALL9.

     ‚* DEM BIBLIOTHEK ERMITTELN
           MOVE     "APG"   TO PGM-LIB.

‚     * LESEN EINES GGF. GESPEICHERTEN TAG AUS DER DTAARA
           INITIALIZE RTVDTAARA-REC.
           MOVE     "ULCDTAARA"  TO DTAARA  OF RTVDTAARA-REC.
           MOVE     "QTEMP"      TO DTALIB  OF RTVDTAARA-REC.
           MOVE     "*ALL"       TO DTASPOS OF RTVDTAARA-REC.
           CALL     "CLP0068" IN LIBRARY PGM-LIB
                              USING RTVDTAARA-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE RTNVAR OF RTVDTAARA-REC TO LCLDATA
           END-IF.

       INI-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGE PROGRAMMTEXT
     ‚*--------------------------------------------------------------
       SHOW-PGMTEXT SECTION.
       SHW-TXT-00.

     ‚* NUR AUFRUFEN WENN BEIDES VORHANDEN IST
           IF       MSG-ID  = SPACES
            OR      MSG-TXT = SPACES
                    GO TO SHW-TXT-90
           END-IF.

     ‚* ERMITTLE NAMEN DER MESSAGE-FILE
           EVALUATE MSG-ID(1:3)
               WHEN "CPF" MOVE "QCPFMSG" TO LT-MSGFIL
                          MOVE "*LIBL"   TO LT-MSGFIL(11:)
               WHEN "SEU" MOVE "ULCMSGF" TO LT-MSGFIL
                          MOVE "*LIBL"   TO LT-MSGFIL(11:)
               WHEN OTHER MOVE MSG-FILE  TO LT-MSGFIL
                          MOVE "*LIBL"   TO LT-MSGFIL(11:)
           END-EVALUATE.

     ‚* ERMITTLE MAX.LÄNGE DER NACHRICHT
           MOVE     MSG-TXT        TO LT-DATA.
           MOVE     MSG-ID         TO LT-MSGID.
           COMPUTE  LT-DATALEN = FUNCTION LENGTH(
                                 FUNCTION TRIMR(LT-DATA)).

     ‚* ANZEIGE DER DATEN
           CALL     "QUILNGTX"  USING LT-DATA
                                      LT-DATALEN
                                      LT-MSGID
                                      LT-MSGFIL
                                      QUS-EC
           END-CALL.

       SHW-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* UPDATE DES USERSPACES
     ‚*--------------------------------------------------------------
       UPDATE-ULC-DTAARA SECTION.
       UPD-ULD-00.

     ‚* DIE ##-MARKIERUNG IN DER DTAARA SPEICHERN, DAMIT DIESE WIEDER
     ‚* VERWENDET WERDEN KANN, WENN MAN DIESE NICHT ANGIBT
           INITIALIZE CHGDTAARA-REC.
           MOVE     "ULCDTAARA" TO DTAARA OF CHGDTAARA-REC.
           MOVE     "QTEMP"     TO DTALIB OF CHGDTAARA-REC.
           MOVE     "*ALL"      TO STRPOS OF CHGDTAARA-REC.
           MOVE     LCLDATA     TO DTAVAL OF CHGDTAARA-REC.
           CALL     "CLP0069" IN LIBRARY PGM-LIB
                              USING CHGDTAARA-REC
                                    RET-CODE
           END-CALL.

       UPD-ULD-90.
           EXIT.
      /
     ‚*----------------------------------------------------------------
     ‚* ERSTELLEN DER DTAARA FÜR DEN ULC
     ‚*----------------------------------------------------------------
       CREATE-ULC-DTAARA SECTION.
       CRT-ULD-00.

           INITIALIZE CRTDTAARA-REC.
           MOVE     "ULCDTAARA"         TO DTAARA  OF CRTDTAARA-REC.
           MOVE     "QTEMP"             TO DTALIB  OF CRTDTAARA-REC.
           MOVE     "0080"              TO DTALEN  OF CRTDTAARA-REC.
           MOVE     "Werte aus ULCSEU"  TO DTATEXT OF CRTDTAARA-REC.
           CALL     "CLP0067" IN LIBRARY PGM-LIB
                              USING CRTDTAARA-REC
                                    RET-CODE
           END-CALL.

       CRT-ULD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBATTRIBUT AUS DEM AT-BEFEHL ENTFERNEN
     ‚*--------------------------------------------------------------
       SET-ATTR-RMV SECTION.
       SET-ARM-00.

     ‚* IN EINER SCHLEIFE ALLE HEX-WERTE ENTFERNEN
           PERFORM  VARYING POS FROM 1 BY 1 UNTIL POS > ENDCOL
                    IF   TXT-SRCLINE(POS:1) NOT < X"20"
                     AND TXT-SRCLINE(POS:1) NOT > X"3F"
                         MOVE SPACE TO TXT-SRCLINE(POS:1)
                    END-IF
           END-PERFORM.

       SET-ARM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBATTRIBUT AUS DEM AT-BEFEHL SETZEN
     ‚*--------------------------------------------------------------
       SET-ATTR-ADD SECTION.
       SET-ATA-00.

     ‚* VALIDIERE CURSOR-POSITION
           EVALUATE TRUE
                    WHEN KZ OF FT-FOUND = "0"
                     AND CURCOL < BEGCOL
                         MOVE 1 TO BEGCOL
                         MOVE 1 TO WRKL
                         PERFORM SUCHE-LEERSTELLE
                         MOVE WRKC TO CURCOL
                    WHEN TXT-SRCLINE(BEGCOL:1) = "*"
                     AND CURCOL < BEGCOL
                         MOVE 6 TO CURCOL
                    WHEN CURCOL > ENDCOL
                         MOVE ENDCOL TO CURCOL
           END-EVALUATE.

     ‚* SETZE HEX-WERT
           IF       CURCOL > ZEROES
                    IF   TXT-SRCLINE(CURCOL:1) = SPACE
                     OR  (    TXT-SRCLINE(CURCOL:1) NOT < X"20"
                          AND TXT-SRCLINE(CURCOL:1) NOT > X"3F" )
                         MOVE AT-HEX OF AT-FOUND TO
                              TXT-SRCLINE(CURCOL:1)
                    ELSE
                         PERFORM SET-ATTR-ADD-WORD
                    END-IF
           ELSE
                    SET DONT-PROCESS TO TRUE
           END-IF.

       SET-ATA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MARKIERE EINZELNES WORT FARBIG
     ‚*--------------------------------------------------------------
       SET-ATTR-ADD-WORD SECTION.
       SET-AAW-00.

           INITIALIZE ALTES-ATTRIBUT.
           INITIALIZE WORT-ANFANG-POS, WORT-ENDE-POS.
     ‚* FARB-ATTRIBUT VON POS. 6, WENN VORHANDEN
           IF       KZ OF FT-FOUND = "1"
            OR      KZ OF FT-FOUND = "2"
                    IF   TXT-SRCLINE(6:1) < X"20"
                     OR  TXT-SRCLINE(6:1) > X"3F"
                         MOVE X"20"            TO ALTES-ATTRIBUT
                    ELSE
                         MOVE TXT-SRCLINE(6:1) TO ALTES-ATTRIBUT
                    END-IF
           ELSE
                    MOVE X"20"                 TO ALTES-ATTRIBUT
           END-IF.

     ‚* SUCHE LETZTES ATRIBUT VOR CURSOR-POSITION
     ‚* UND DIE POSITION DAVON ODER EINER LEERSTELLE
           MOVE     BEGCOL TO POS.
           PERFORM  UNTIL POS >= CURCOL
     ‚*             POSITION DER LEERSTELLE ...
                    IF   TXT-SRCLINE(POS:1) = SPACE
                         MOVE POS TO WORT-ANFANG-POS
                    END-IF
     ‚*             ... WENN DIE ZEILE KEIN HEX-ZEICHEN IST, DANN
     ‚*             WEITERSUCHEN. ANSONSTEN WERT MERKEN
                    IF   TXT-SRCLINE(POS:1) < X"20"
                     OR  TXT-SRCLINE(POS:1) > X"3F"
                         ADD 1 TO POS
                    ELSE
                         MOVE TXT-SRCLINE(POS:1) TO ALTES-ATTRIBUT
                         MOVE POS                TO WORT-ANFANG-POS
                         ADD 1 TO POS
                    END-IF
           END-PERFORM.

     ‚* SUCHE NACHFOLGENDES ATTRIBUT NACH DER CURSOR-POSITION UND DIE
     ‚* POSITION DAVON ODER EINER LEERSTELLE
           MOVE     CURCOL TO POS.
           PERFORM  UNTIL POS           > ENDCOL
                     OR   WORT-ENDE-POS > ZEROES
     ‚*             POSITION DER LEERSTELLE ...
                    IF   TXT-SRCLINE(POS:1) = SPACES
                         MOVE POS TO WORT-ENDE-POS
                    END-IF
     ‚*             ... WENN DIE ZEILE KEIN HEX-ZEICHEN IST, DANN
     ‚*             WEITERSUCHEN. ANSONSTEN WERT MERKEN
                    IF   TXT-SRCLINE(POS:1) < X"20"
                     OR  TXT-SRCLINE(POS:1) > X"3F"
                         ADD 1 TO POS
                    ELSE
                         MOVE TXT-SRCLINE(POS:1) TO ALTES-ATTRIBUT
                         MOVE POS                TO WORT-ENDE-POS
                    END-IF
           END-PERFORM.

     ‚* SETZE NEUES ATTRIBUT VOR DEN WORTANFANG
           IF       WORT-ANFANG-POS < BEGCOL
            AND     (    KZ OF FT-FOUND = "1"
                     OR  KZ OF FT-FOUND = "2" )
                    MOVE AT-HEX OF AT-FOUND TO TXT-SRCLINE(6:1)
           END-IF.
           IF       WORT-ANFANG-POS NOT < BEGCOL
            AND     WORT-ANFANG-POS NOT > ENDCOL
                    MOVE AT-HEX OF AT-FOUND TO
                         TXT-SRCLINE(WORT-ANFANG-POS:1)
           END-IF.

     ‚* SETZE ALTES ATTRIBUT HINTER WORTENDE
           IF       WORT-ENDE-POS NOT > ENDCOL
            AND     WORT-ENDE-POS NOT < BEGCOL
                    MOVE ALTES-ATTRIBUT TO TXT-SRCLINE(WORT-ENDE-POS:1)
           END-IF.

       SET-AAW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DEN HEADER AUS DEM USER-SPACE AUSLESEN
     ‚*--------------------------------------------------------------
       GET-USRSPC-HEADER SECTION.
       GET-USH-00.

           MOVE     +1                     TO USRSPC-OFFSET.
           MOVE     LENGTH OF HEADER-SPACE TO USRSPC-SIZE.
           CALL     "QUSRTVUS" USING USRSPC-NAME
                                     USRSPC-OFFSET
                                     USRSPC-SIZE
                                     HEADER-SPACE
                                     QUS-EC
           END-CALL.

       GET-USH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES MEMBER-TYP MITTELS RTVMBRD
     ‚*--------------------------------------------------------------
       GET-MBR-TYPE SECTION.
       GET-MBT-00.

           INITIALIZE RTVMBRD-REC.
           MOVE     HDR-LIBNAME TO OBJLIB   OF RTVMBRD-REC.
           MOVE     HDR-SRCFILE TO FILENAME OF RTVMBRD-REC.
           MOVE     HDR-MBRNAME TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" IN LIBRARY PGM-LIB
                              USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

       GET-MBT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NÄCHSTE ZEILE AUS DEM USERSPACE AUSLESEN
     ‚*--------------------------------------------------------------
       GET-NEXT-LINE SECTION.
       GET-NXL-00.

           IF       ACT-LINE NOT > ZEROES
                    GO TO GET-NXL-90
           END-IF.

     ‚* GRÖßE DES USERSPACE UND OFFSET DER AKTUELLEN ZEILE ERRECHNEN
           COMPUTE  USRSPC-SIZE = HDR-RCDLEN + LINEPRFX-LEN.
           COMPUTE  USRSPC-OFFSET =   LENGTH OF HEADER-SPACE
                                    + 1 + (USRSPC-SIZE
                                    * (ACT-LINE - 1))
           END-COMPUTE.

     ‚* API AUFRUFEN
           CALL     "QUSRTVUS" USING USRSPC-NAME
                                     USRSPC-OFFSET
                                     USRSPC-SIZE
                                     TEXT-SPACE
                                     QUS-EC
           END-CALL.

       GET-NXL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NUN DEN BEFEHL AUSWERTEN UND ENTSPRECHEND AUSFÜHREN
     ‚*--------------------------------------------------------------
       GET-COMMAND SECTION.
       GET-CMD-00.

     ‚* ALLE HEXZEICHEN DURCH LEERZEICHEN ERSETZEN
           INSPECT  TXT-LINECMD REPLACING ALL LOW-VALUES BY SPACES.

           INITIALIZE COMMAND-CODE.
           MOVE     TXT-LINECMD TO COMMAND-CODE.

     ‚* AUSFÜHREN DER VERSCHIEDENEN ZEILEN-BEFEHLE
           EVALUATE TRUE
     ‚*        HILFE
               WHEN COMMAND-CODE(1:1) = "?"
               WHEN COMMAND-CODE(1:3) = "ULC"
                    PERFORM EXECUTE-HILFE
     ‚*        AT-HILFE
               WHEN COMMAND-CODE(1:3) = "AT?"
                    MOVE "SEU0014" TO MSG-ID
                    PERFORM GET-MESSAGE-FROM-MSGF
                    MOVE MSG-ID TO LT-MSGID
                    MOVE "ULCMSGF" TO LT-MSGFIL
                    MOVE PGM-LIB   TO LT-MSGFIL(11:)
                    COMPUTE LT-DATALEN = LENGTH OF HLPTXT
                    CALL "QUILNGTX"  USING HLPTXT
                                           LT-DATALEN
                                           LT-MSGID
                                           LT-MSGFIL
                                           QUS-EC
                    END-CALL
                    SET CLEAR-COMMAND TO TRUE
                    SET NO-UPDATE     TO TRUE
                    PERFORM CHANGE-LINE
     ‚*        DT : DISPLAY TEXT
               WHEN COMMAND-CODE(1:2) = "DT"
                    PERFORM EXECUTE-DT
                    SET NO-UPDATE     TO TRUE
     ‚*        DFD : DISPLAY FELD-DEFINITION
               WHEN COMMAND-CODE(1:3) = "DFD"
                    PERFORM EXECUTE-DFD
                    SET NO-UPDATE     TO TRUE
     ‚*        DK : DISPLAY KEY
               WHEN COMMAND-CODE(1:2) = "DK"
                    PERFORM EXECUTE-DK
                    SET NO-UPDATE     TO TRUE
     ˜*        IST UPDATE ERLAUBT ?
               WHEN HDR-MODE NOT = "U"
                    MOVE "SEU0004" TO MSG-ID
                    PERFORM SENDE-NACHRICHT
                    SET NO-UPDATE     TO TRUE
     ‚*        AT : ATRIBUT
               WHEN COMMAND-CODE(1:2) = "AT"
               WHEN COMMAND-SAVE(1:2) = "AT"
                    PERFORM EXECUTE-AT
     ‚*        KZ : KOMMENTAR ZUGANG
               WHEN COMMAND-CODE(1:2) = "KZ"
                    PERFORM EXECUTE-KZ
     ‚*        T  : TAG
               WHEN COMMAND-CODE(1:1) = "T"
               WHEN COMMAND-CODE(1:2) = "TT"
               WHEN COMMAND-SAVE(1:2) = "TT"
                    PERFORM EXECUTE-TAG
     ‚*        ISS : INSERT SECTION
               WHEN COMMAND-CODE(1:3) = "ISS"
                    PERFORM EXECUTE-ISS
     ‚*        ISK : INSERT KEY-FELDER
               WHEN COMMAND-CODE(1:3) = "ISK"
                    PERFORM EXECUTE-ISK
     ‚*        ISC : INSERT KEY-CONDITIONS
               WHEN COMMAND-CODE(1:3) = "ISC"
                    PERFORM EXECUTE-ISC
     ‚*        ISF : INSERT FELDER
               WHEN COMMAND-CODE(1:3) = "ISF"
                    PERFORM EXECUTE-ISF
     ‚*        K  : KOMMENTAR BILDEN
               WHEN COMMAND-CODE(1:1) = "K"
               WHEN COMMAND-CODE(1:2) = "KK"
               WHEN COMMAND-CODE(1:2) = "KT"
               WHEN COMMAND-SAVE(1:2) = "KK"
               WHEN COMMAND-SAVE(1:2) = "KT"
                    PERFORM EXECUTE-KK
     ‚*        CKT : COPY, KOMMENTIERE, TAG
               WHEN COMMAND-CODE(1:3) = "CKT"
               WHEN COMMAND-SAVE(1:3) = "CKT"
                    PERFORM EXECUTE-CKT
     ‚*        AP : APPEND
               WHEN COMMAND-CODE(1:2) = "AP"
               WHEN COMMAND-SAVE(1:2) = "AP"
                    PERFORM EXECUTE-AP
     ‚*        IC : INSERT CHARACTER
               WHEN COMMAND-CODE(1:2) = "IC"
               WHEN COMMAND-SAVE(1:2) = "IC"
                    PERFORM EXECUTE-IC
     ‚*        LC/LCC/MC/MCC/UC/UCC: LOWER/MIXED/UPPER-CASE
               WHEN COMMAND-CODE(1:2) = "LC"
               WHEN COMMAND-CODE(1:2) = "MC"
               WHEN COMMAND-CODE(1:2) = "UC"
               WHEN COMMAND-CODE(1:3) = "LCC"
               WHEN COMMAND-CODE(1:3) = "MCC"
               WHEN COMMAND-CODE(1:3) = "UCC"
               WHEN COMMAND-SAVE(1:3) = "LCC"
               WHEN COMMAND-SAVE(1:3) = "MCC"
               WHEN COMMAND-SAVE(1:3) = "UCC"
                    PERFORM EXECUTE-CASE
     ‚*        LS : LEFT SHIFT
               WHEN COMMAND-CODE(1:2) = "LS"
               WHEN COMMAND-SAVE(1:2) = "LS"
                    PERFORM EXECUTE-LS
     ‚*        RS : RIGHT SHIFT
               WHEN COMMAND-CODE(1:2) = "RS"
               WHEN COMMAND-SAVE(1:2) = "RS"
                    PERFORM EXECUTE-RS
     ‚*        BX : BOX
               WHEN COMMAND-CODE(1:2) = "BX"
               WHEN COMMAND-SAVE(1:2) = "BX"
                    PERFORM EXECUTE-BX
     ‚*        VT : VERDICHTE TEXT
               WHEN COMMAND-CODE(1:2) = "VT"
               WHEN COMMAND-SAVE(1:2) = "VT"
                    PERFORM EXECUTE-VT
     ‚*        SORT : SORTIERE TEXT
               WHEN COMMAND-CODE(1:4) = "SORT"
               WHEN COMMAND-SAVE(1:4) = "SORT"
                    PERFORM EXECUTE-SORT
     ‚*        DW : DELETE WORD
               WHEN COMMAND-CODE(1:2) = "DW"
               WHEN COMMAND-SAVE(1:2) = "DW"
                    PERFORM EXECUTE-DW
     ‚*        ANSONSTEN UNBEKANNTER BEFEHL... FEHLERMELDUNG!
               WHEN OTHER
                    SET LEAVE-COMMAND TO TRUE
                    SET DONT-PROCESS  TO TRUE
           END-EVALUATE.

       GET-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERUNGSMERKMAL BILDEN (TAG)
     ‚* (NORMAL ##XXX, BIS 5 STELLEN MÖGLICH)
     ‚*--------------------------------------------------------------
       GET-TAGVALUE SECTION.
       GET-TAG-00.

‚     * LESEN EINES GGF. GESPEICHERTEN TAG AUS DER DTAARA
           INITIALIZE RTVDTAARA-REC.
           MOVE     "ULCDTAARA"  TO DTAARA  OF RTVDTAARA-REC.
           MOVE     "QTEMP"      TO DTALIB  OF RTVDTAARA-REC.
           MOVE     "*ALL"       TO DTASPOS OF RTVDTAARA-REC.
           CALL     "CLP0068" IN LIBRARY PGM-LIB
                              USING RTVDTAARA-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE RTNVAR OF RTVDTAARA-REC TO LCLDATA
           END-IF.

     ‚* SOLANGE WIR IN DER GLEICHEN BIBLIOTHEK SIND, KÖNNEN WIR DAS
     ‚* GESPEICHERTE TAG VERWENDEN. SOBALD DIE LIB WECHSELT, MÜSSEN
     ‚* WIR UNS DIE NEUEN WERTE ANHAND DER BIBLIOTHEK ERMITTELN
           IF       TAG-VALUE        NOT = SPACES
            AND     HDR-LIBNAME(1:3)     = LCLLIB OF LCLDATA(1:3)
                    MOVE LCLMRK OF LCLDATA TO TAG-VALUE
           END-IF.

     ‚* WENN WIR KEINEN TAG-WERT HABEN, DANN NUN DIE ERSTEN DREI
     ‚* STELLEN AUS DER BIBLIOTHEK NEHMEN (KUNDENKÜRZEL) SOFERN WIR
     ‚* KEIN KUNDENPROGRAMM SIND
           IF       TAG-VALUE = SPACES
                    IF   FORCE-TAG = ZEROES
                     AND (    HDR-LIBNAME(1:3) = "LFS"
                          OR  HDR-MBRNAME(1:2) = "KD"
                          OR  HDR-MBRNAME(1:2) = "FK"
                          OR  (    HDR-MBRNAME(1:2)  = "DU"
                               AND HDR-MBRNAME(6:3) >= "700"))
                         CONTINUE
                    ELSE
                         MOVE HDR-LIBNAME(1:3) TO TAG-VALUE
                    END-IF
           END-IF.

       GET-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTLE ATTRIBUT-WERT
     ‚*--------------------------------------------------------------
       GET-ATHEX-VALUE SECTION.
       GET-VAL-00.

     ‚* SETZE AUF "UNGÜLTIG"
           INITIALIZE AT-FOUND.
           MOVE     "#" TO AT-HEX OF AT-FOUND.

     ‚* DEFAULT = "H"
           IF       WRK-FELD3 = SPACES
                    MOVE "H" TO WRK-FELD3
           END-IF.

     ‚* SUCHE AT-SCHLÜSSELWORT
           PERFORM  VARYING ATX FROM 1 BY 1
                    UNTIL ATX > AT-ANZAHL
                     OR   AT-CMD OF AT-FOUND NOT = SPACES
                    IF   WRK-FELD3 = AT-CMD OF AT-EINTRAG(ATX)
                         MOVE CORR AT-EINTRAG(ATX) TO AT-FOUND
                    END-IF
           END-PERFORM.

       GET-VAL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* RETRIEVE MESSAGE
     ‚*--------------------------------------------------------------
       GET-MESSAGE-FROM-MSGF SECTION.
       GET-MFM-00.

           IF       MSG-ID NOT = SPACES
     ‚*             VARIABLEN FÜR DIE API FÜLLEN
                    MOVE     "RTVM0100" TO RM-MSGFMT
                    MOVE     MSG-FILE   TO RM-MSGFIL(1:10)
                    MOVE     "*LIBL"    TO RM-MSGFIL(11:10)
                    MOVE     MSG-ID     TO RM-MSGID
                    MOVE     3100       TO RM-MSGLEN
                    MOVE     "*NO"      TO RM-MSGTYP
                    MOVE     "*NO"      TO RM-MSGSTACKENT
     ‚*             NACHRICHTENTEXT PER API AUFRUF ERMITTELN
                    CALL     "QMHRTVM" USING RM-MSGDTA
                                             RM-MSGLEN
                                             RM-MSGFMT
                                             RM-MSGID
                                             RM-MSGFIL
                                             RM-MSGRDT
                                             RM-MSGDATALEN
                                             RM-MSGTYP
                                             RM-MSGSTACKENT
                                             RM-MSGKEY
                    END-CALL
                    MOVE RM-MSGDTA(25:) TO RM-MSGDTA
           END-IF.

       GET-MFM-90.
           EXIT.
      /
     ‚*----------------------------------------------------------------
     ‚* PRÜFE WELCHER BEFEHL AUSGEFÜHRT WERDEN SOLL
     ‚*----------------------------------------------------------------
       CHECK-USR-COMMAND SECTION.
       CHK-USC-00.

           INITIALIZE RET-CODE.
     ‚* ZÄHLE ALLE PREFIX-BEFEHLE

     ‚* ANZAHL DER BEFEHLE/BLOCKBEFEHLE
           INITIALIZE ANZ-COMMAND.
           INITIALIZE ANZ-BLK-CMD.
           INITIALIZE COMMAND-SAVE.

     ‚* ALLE ZEILEN NUN DURCHLESEN UND START UND END-BEFEHL MERKEN
           PERFORM  VARYING ACT-LINE FROM 1 BY 1
                    UNTIL ACT-LINE     > END-LINE
                     OR   RET-CODE NOT = SPACE
     ‚*             NÄCHSTE ZEILE LESEN
                    PERFORM GET-NEXT-LINE
     ‚*             BEFEHL AUSWERTEN
                    INITIALIZE COMMAND-CODE
                    MOVE TXT-LINECMD TO COMMAND-CODE
                    EVALUATE TRUE
                        WHEN TXT-LINECMD = SPACES
                             CONTINUE
                        WHEN TXT-LINECMD(1:2) = "AT"
                        WHEN TXT-LINECMD(1:2) = "TT"
                        WHEN TXT-LINECMD(1:2) = "KK"
                        WHEN TXT-LINECMD(1:2) = "KT"
                        WHEN TXT-LINECMD(1:2) = "AP"
                        WHEN TXT-LINECMD(1:2) = "LS"
                        WHEN TXT-LINECMD(1:2) = "RS"
                        WHEN TXT-LINECMD(1:2) = "VT"
                        WHEN TXT-LINECMD(1:2) = "BX"
                        WHEN TXT-LINECMD(1:2) = "IC"
                        WHEN TXT-LINECMD(1:2) = "DW"
                             MOVE TXT-LINECMD(1:2) TO COMMAND-CODE
                             ADD 1                 TO ANZ-COMMAND
                        WHEN TXT-LINECMD(1:3) = "CKT"
                        WHEN TXT-LINECMD(1:3) = "LCC"
                        WHEN TXT-LINECMD(1:3) = "MCC"
                        WHEN TXT-LINECMD(1:3) = "UCC"
                             MOVE TXT-LINECMD(1:3) TO COMMAND-CODE
                             ADD 1                 TO ANZ-COMMAND
                        WHEN TXT-LINECMD(1:4) = "SORT"
                             MOVE TXT-LINECMD(1:4) TO COMMAND-CODE
                             ADD 1                 TO ANZ-COMMAND
                    END-EVALUATE
     ‚*             ERSTEN BEFEHL ZWISCHENSPEICHERN, DAMIT GGF. DER
     ‚*             BLOCKBEFEHL ABGESCHLOSSEN WERDEN KANN
                    IF   COMMAND-SAVE = SPACES
                         MOVE COMMAND-CODE TO COMMAND-SAVE
                    END-IF
     ‚*             BEFEHLSWECHSEL UND BLOCKBEFEHL OFFEN
                    IF   COMMAND-CODE NOT = SPACE
                     AND COMMAND-CODE NOT = COMMAND-SAVE
                     AND ANZ-COMMAND      > ZEROES
                         MOVE "1" TO RET-CODE
                    END-IF
           END-PERFORM.

     ‚* KEIN BEFEHL VORHANDEN, KEIN FEHLER DURCH WECHSEL
           IF       RET-CODE     NOT = SPACE
            OR      COMMAND-SAVE     = SPACES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-USC-90
           END-IF.

     ‚* EINIGE BEFEHLE KÖNNEN EINZELND ABER AUCH ALS BLOCKBEFEHL
     ‚* GENUTZT WERDEN. HIER PRÜFEN OB DER BEFEHL EINMALIG VORKOMMT
     ‚* ODER MEHRMALS AUFTAUCHT.
           IF       ANZ-COMMAND = 1
            AND     (   COMMAND-SAVE = "AT"
                     OR COMMAND-SAVE = "KT"
                     OR COMMAND-SAVE = "AP"
                     OR COMMAND-SAVE = "RS"
                     OR COMMAND-SAVE = "LS"
                     OR COMMAND-SAVE = "VT"
                     OR COMMAND-SAVE = "BX"
                     OR COMMAND-SAVE = "IC"
                     OR COMMAND-SAVE = "DW"
                     OR COMMAND-SAVE = "CKT" )
                    CONTINUE
           ELSE
     ‚*             BLOCKBEFEHLE MÜSSEN AUCH DOPPELT VORKOMMEN
                    DIVIDE   2 INTO ANZ-COMMAND
                             GIVING ANZ-BLK-CMD REMAINDER REST
                    END-DIVIDE
                    IF   REST > ZEROES
                         MOVE "1" TO RET-CODE
                    END-IF
           END-IF.

       CHK-USC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN WELCHEN MBR-TYPE WIR HABEN UND EINSTELLUNGEN
     ‚* ENTSPRECHEND SETZEN
     ‚*--------------------------------------------------------------
       CHECK-MBR-TYPE SECTION.
       CHK-MBT-00.

           INITIALIZE FT-FOUND.
     ‚* SUCHE MEMBER-TYP IM ARRAY
           PERFORM  VARYING FTX FROM 1 BY 1
                    UNTIL FTX > FT-ANZAHL
                     OR   TYP OF FT-FOUND NOT = SPACES
                    IF   HDR-MBRTYPE = TYP OF FT-EINTRAG(FTX)
                         MOVE CORR FT-EINTRAG(FTX) TO FT-FOUND
                    END-IF
           END-PERFORM.

     ‚* ANFANG- UND ENDE-POSITION
           MOVE     1                TO BEGCOL.
           MOVE     HDR-RCDLEN       TO ENDCOL.

     ‚* PROGRAMME UND DEFINITIONEN BEGINNEN AB 7
           IF       KZ OF FT-FOUND = "1"
            OR      KZ OF FT-FOUND = "2"
                    MOVE 7           TO BEGCOL
           END-IF.

     ‚* ENDE-POSITION JE NACH PROGRAMM-SPRACHE
           EVALUATE PGM OF FT-FOUND
               WHEN "C"   SUBTRACT 6  FROM ENDCOL
               WHEN "R"   SUBTRACT 8  FROM ENDCOL
               WHEN OTHER MOVE HDR-RCDLEN  TO ENDCOL
           END-EVALUATE.

     ‚* ENDE-POSITION AN BILDSCHIRM-GRÖßE ANPASSEN
           IF       KZ OF FT-FOUND = "1"
            AND     ENDCOL < 80
            AND     ENDCOL > 70
                    MOVE 71          TO ENDCOL
           END-IF.

       CHK-MBT-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
