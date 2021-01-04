       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP0001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* COMPILER-PROGRAMM                                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  14.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSFÜHREN DER COMPILIERUNG                   *
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
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI COMPILER-HISTORIE
     ‚*-------------------------------------------------------------
           SELECT PCMPHIS-DP
                  ASSIGN       TO  DATABASE-PCMPHIS
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- COMPILER-HISTORIE
        FD PCMPHIS-DP
           LABEL RECORDS ARE STANDARD.
        01 PCMPHIS-P.
           COPY DDS-ALL-FORMATS OF PCMPHIS.
      /
     ‚*--- PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
        FD PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
        01 PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP0001".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  RTVOBJD-REC                   TYPE RTVOBJD-TYPE.
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.
       01  GRTOBJAUT-REC                 TYPE GRTOBJAUT-TYPE.
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  SNDPGMMSG-REC                 TYPE SNDPGMMSG-TYPE.
       01  CHGOBJOWN-REC                 TYPE CHGOBJOWN-TYPE.
       01  CRTCBLPGM-REC                 TYPE CRTCBLPGM-TYPE.
       01  CRTBNDCBL-REC                 TYPE CRTBNDCBL-TYPE.
       01  CRTCBLMOD-REC                 TYPE CRTCBLMOD-TYPE.
       01  CRTSQLCBL-REC                 TYPE CRTSQLCBL-TYPE.
       01  CRTBNDCL-REC                  TYPE CRTBNDCL-TYPE.
       01  CRTCMOD-REC                   TYPE CRTCMOD-TYPE.
       01  CRTCMD-REC                    TYPE CRTCMD-TYPE.
       01  CRTDSPF-REC                   TYPE CRTDSPF-TYPE.
       01  CRTPF-REC                     TYPE CRTPF-TYPE.
       01  CRTLF-REC                     TYPE CRTLF-TYPE.
       01  CRTTBL-REC                    TYPE CRTTBL-TYPE.
       01  CRTPNLGRP-REC                 TYPE CRTPNLGRP-TYPE.

       01  CMFCDE-WRK                    LIKE CMFCDE OF PCMPHIS-P.
       01  OBJTYPE-WRK                   LIKE OBJTYPE OF RTVOBJD-REC.
       01  USRAUT-WRK                    LIKE AUT     OF GRTOBJAUT-REC.

     ‚* FÜR LIKE-DEKLARATION
        01 PCMPSCM-WRK.
           COPY DDS-ALL-FORMATS OF PCMPSCM.

     ‚* PARAMETER FÜR DAS HLP0008
       01  CALL-PGM                      PIC X(10).
       01  CALL-PARM                     PIC X(1027).
       01  BATCH-KZ                      PIC X(1) VALUE "1".
       01  F3                            PIC X(1).
       01  F12                           PIC X(1).

     ‚* PARAMETER FÜR DAS CMP0007
       01  SCMKEY-WRK                    LIKE SMKEY  OF PCMPSCM-WRK.
       01  SCM-PTR                       USAGE POINTER.
       01  SCM-INP-PTR                   USAGE POINTER.
       01  AKTION-WRK                    PIC X(20).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CMLFDN                      LIKE CMLFDN OF PCMPHIS-P.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CMLFDN
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CMP0001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCMPHIS-DP
                          PCONFIG-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE CMFCDE-WRK.
     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-CMLFDN     = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* EINTRAG IN DER PCMPHIS LESEN UM OPTIONEN ZU ERMITTELN
           INITIALIZE PCMPHISF OF PCMPHIS-P.
           MOVE     X-CMLFDN       TO CMLFDN OF PCMPHIS-P.
           READ     PCMPHIS-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             START USER/DATUM/ZEIT FÜLLEN
                    PERFORM  COPY-GET-TIME
                    MOVE     T-USER   TO CMCUSR OF PCMPHIS-P
                    MOVE     T-TERM   TO CMCBS  OF PCMPHIS-P
                    MOVE     PGM-WRK  TO CMCPGM OF PCMPHIS-P
                    MOVE     DATE-8   TO CMCDTA OF PCMPHIS-P
                    MOVE     TIME-6   TO CMCTIA OF PCMPHIS-P
     ‚*             LETZTE MODIFIKATION IMMER FÜLLEN
                    MOVE     T-USER   TO CMMSER OF PCMPHIS-P
                    MOVE     T-TERM   TO CMMBS  OF PCMPHIS-P
                    MOVE     PGM-WRK  TO CMMPGM OF PCMPHIS-P
                    MOVE     DATE-8   TO CMMDTA OF PCMPHIS-P
                    MOVE     TIME-6   TO CMMTIA OF PCMPHIS-P
                    REWRITE PCMPHIS-P
           ELSE
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB ES DIE TEILDATEI GIBT
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    MOVE "CXE0001" TO CMFCDE-WRK
                    GO TO ENDE
           END-IF.

     ‚* WELCHE TEILDATEI-ART HABEN WIR?
           PERFORM  GET-MEMBER-DATEN.
           IF       RET-CODE NOT = SPACES
                    MOVE "CXE0002" TO CMFCDE-WRK
                    GO TO ENDE
           END-IF.

     ‚* MUSS NOCH EIN PROGRAMMAUFRUF VOR DEM WANDELN AUSGEFÜHRT WERDEN?
           IF       CMVCPG OF PCMPHIS-P NOT = SPACES
                    PERFORM  CALL-VOR-COMPILE
           END-IF.

     ‚* SOLL DIE SOURCE VOR DEM COMPILIEREN UNKENNTLICH GEMACHT WERDEN?
           IF       CMSCRM OF PCMPHIS-P NOT = ZEROES
                    PERFORM  SCRAMBLE-SOURCE
           END-IF.

           EVALUATE SRCTYPE OF RTVMBRD-REC
     ‚*             COBOL-PROGRAMME
                    WHEN "CBL"
                         PERFORM CREATE-COBOL
                    WHEN "CBLLE"
                         PERFORM CREATE-ILE-COBOL
                    WHEN "SQLCBLLE"
                         PERFORM CREATE-SQL-COBOL
     ‚*             CMD-PROGRAMME
                    WHEN "CMD"
                         PERFORM CREATE-CMD
     ‚*             CL-PROGRAMME
                    WHEN "CLP"
                         GO TO ENDE
                    WHEN "CLLE"
                         PERFORM CREATE-ILE-CL
     ‚*             DISPLAYS
                    WHEN "DSPF"
                         PERFORM CREATE-DSPF
     ‚*             PRINTERFILES
                    WHEN "PRTF"
                         GO TO ENDE
     ‚*             PHYSISCHE DATEI
                    WHEN "PF"
                         PERFORM CREATE-PF
     ‚*             LOGISCHE DATEI
                    WHEN "LF"
                         PERFORM CREATE-LF
     ‚*             ANZEIGEGRUPPIERUNG
                    WHEN "PNLGRP"
                         PERFORM CREATE-PNLGRP
     ‚*             C-MODULE
                    WHEN "C"
                         PERFORM CREATE-C-MODULE
     ‚*             TABELLE/SRTSEQ ETC
                    WHEN "TBL"
                         PERFORM CREATE-TBL
                    WHEN OTHER
                         MOVE "CXE0004" TO CMFCDE-WRK
                         GO TO ENDE
           END-EVALUATE.
           IF       RET-CODE NOT = SPACES
                    MOVE "CXE0003" TO CMFCDE-WRK
                    GO TO ENDE
           END-IF.

     ‚* MUSS NOCH EIN PROGRAMMAUFRUF NACH DEM WANDELN AUSGEFÜHRT
     ‚* WERDEN?
           IF       CMNCPG OF PCMPHIS-P NOT = SPACES
                    PERFORM  CALL-NACH-COMPILE
           END-IF.

     ‚* ERMITTELN DES OBJEKTTYPEN
           PERFORM  GET-OBJTYPE.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* SOLLEN DER EIGNER GEÄNDERT WERDEN?
           IF       CMNOWN OF PCMPHIS-P NOT = ZEROES
                    PERFORM  CHANGE-OWNER
           END-IF.

     ‚* SOLLEN JETZT DIE BERECHTIGUNGEN GEÄNDERT WERDEN?
           IF       CMRECT OF PCMPHIS-P NOT = ZEROES
                    PERFORM  CHANGE-BERECHTIGUNG
           END-IF.

       ENDE.
     ‚* JETZT DEN STATUS DER DATEI AKTUALISIEREN
           PERFORM  UPDATE-PCMPHIS.
           IF       CMFCDE-WRK = SPACES
                    PERFORM UPDATE-VERSION
           END-IF.

           CLOSE    PCMPHIS-DP
                    PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

     ‚* ERST HIER ABBRECHEN, DAMIT WIR EINEN JOBLOG KRIEGEN
           IF       CMFCDE-WRK NOT = SPACES
                    PERFORM SEND-BREAKMESSAGE
           END-IF.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* COBOL-ERSTELLEN (KEIN ILE)                                CBL
     ‚*--------------------------------------------------------------
       CREATE-COBOL SECTION.
       CRT-CBL-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND PROGRAMM ERSTELLEN
           INITIALIZE CRTCBLPGM-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTCBLPGM-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTCBLPGM-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTCBLPGM-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTCBLPGM-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTCBLPGM-REC.
           MOVE     CMTRLS  OF PCMPHIS-P  TO TGTRLS   OF CRTCBLPGM-REC.
           MOVE     CMUSPR  OF PCMPHIS-P  TO USRPRF   OF CRTCBLPGM-REC.
           MOVE     CMDBG   OF PCMPHIS-P  TO OPTIONS  OF CRTCBLPGM-REC.
     ‚* MAPPEN DER OPTIMIERUNG VON ILE-COBOL NACH COBOL
           IF       CMOPTI OF PCMPHIS-P = "*NONE"
                    MOVE "*NOOPTIMIZE"    TO GENOPT   OF CRTCBLPGM-REC
           ELSE
                    MOVE "*OPTIMIZE"      TO GENOPT   OF CRTCBLPGM-REC
           END-IF.
           CALL     "CLP0020" USING CRTCBLPGM-REC
                                    RET-CODE
           END-CALL.

       CRT-CBL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ILE-COBOL ERSTELLEN                                     CBLLE
     ‚*--------------------------------------------------------------
       CREATE-ILE-COBOL SECTION.
       CRT-ICB-00.

     ‚* MODUL ERSTELLEN, ODER BEIDES?
           EVALUATE CMCRTO OF PCMPHIS-P
     ‚*             NUR MODUL ERSTELLEN
                    WHEN 1
                         PERFORM CREATE-ILE-COBOL-MODULE
                         GO TO CRT-ICB-90
     ‚*             MODUL UND DANACH PGM ERSTELLEN
                    WHEN 2
                         PERFORM CREATE-ILE-COBOL-MODULE
           END-EVALUATE.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND PROGRAMM ERSTELLEN
           INITIALIZE CRTBNDCBL-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTBNDCBL-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTBNDCBL-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTBNDCBL-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTBNDCBL-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTBNDCBL-REC.
           MOVE     CMACTG  OF PCMPHIS-P  TO ACTGRP   OF CRTBNDCBL-REC.
           MOVE     CMTRLS  OF PCMPHIS-P  TO TGTRLS   OF CRTBNDCBL-REC.
           MOVE     CMUSPR  OF PCMPHIS-P  TO USRPRF   OF CRTBNDCBL-REC.
           MOVE     CMOPTI  OF PCMPHIS-P  TO OPTIMIZE OF CRTBNDCBL-REC.
           MOVE     CMDBG   OF PCMPHIS-P  TO DEBUG    OF CRTBNDCBL-REC.
           MOVE     CMBNDD  OF PCMPHIS-P  TO BNDDIR   OF CRTBNDCBL-REC.
           CALL     "CLP0021" USING CRTBNDCBL-REC
                                    RET-CODE
           END-CALL.

       CRT-ICB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ILE-COBOL MODUL ERSTELLEN                               CBLLE
     ‚*--------------------------------------------------------------
       CREATE-ILE-COBOL-MODULE SECTION.
       CRT-ICM-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND PROGRAMM ERSTELLEN
           INITIALIZE CRTBNDCBL-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTCBLMOD-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTCBLMOD-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTCBLMOD-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTCBLMOD-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTCBLMOD-REC.
           MOVE     CMTRLS  OF PCMPHIS-P  TO TGTRLS   OF CRTCBLMOD-REC.
           MOVE     CMOPTI  OF PCMPHIS-P  TO OPTIMIZE OF CRTCBLMOD-REC.
           MOVE     CMDBG   OF PCMPHIS-P  TO DEBUG    OF CRTCBLMOD-REC.
           CALL     "CLP0034" USING CRTCBLMOD-REC
                                    RET-CODE
           END-CALL.

       CRT-ICM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SQL-COBOL ERSTELLEN                                  SQLCBLLE
     ‚*--------------------------------------------------------------
       CREATE-SQL-COBOL SECTION.
       CRT-SCB-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND PROGRAMM ERSTELLEN
           INITIALIZE CRTSQLCBL-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTSQLCBL-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTSQLCBL-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTSQLCBL-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTSQLCBL-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTSQLCBL-REC.
           MOVE     CMACTG  OF PCMPHIS-P  TO ACTGRP   OF CRTSQLCBL-REC.
           MOVE     CMTRLS  OF PCMPHIS-P  TO TGTRLS   OF CRTSQLCBL-REC.
           MOVE     CMDBG   OF PCMPHIS-P  TO DEBUG    OF CRTSQLCBL-REC.
           MOVE     CMBNDD  OF PCMPHIS-P  TO BNDDIR   OF CRTSQLCBL-REC.
           CALL     "CLP0027" USING CRTSQLCBL-REC
                                    RET-CODE
           END-CALL.

       CRT-SCB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ILE-CL ERSTELLEN                                         CLLE
     ‚*--------------------------------------------------------------
       CREATE-ILE-CL SECTION.
       CRT-ICL-00.

     ‚* MODUL ERSTELLEN, ODER BEIDES?
           EVALUATE CMCRTO OF PCMPHIS-P
     ‚*             NUR MODUL ERSTELLEN
                    WHEN 1
                         PERFORM CREATE-ILE-CL-MODULE
                         GO TO CRT-ICL-90
     ‚*             MODUL UND DANACH PGM ERSTELLEN
                    WHEN 2
                         PERFORM CREATE-ILE-CL-MODULE
           END-EVALUATE.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND PROGRAMM ERSTELLEN
           INITIALIZE CRTBNDCL-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTBNDCL-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTBNDCL-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTBNDCL-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTBNDCL-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTBNDCL-REC.
           MOVE     CMACTG  OF PCMPHIS-P  TO ACTGRP   OF CRTBNDCL-REC.
           MOVE     CMTRLS  OF PCMPHIS-P  TO TGTRLS   OF CRTBNDCL-REC.
           MOVE     CMUSPR  OF PCMPHIS-P  TO USRPRF   OF CRTBNDCL-REC.
           MOVE     CMOPTI  OF PCMPHIS-P  TO OPTIMIZE OF CRTBNDCL-REC.
           MOVE     CMDBG   OF PCMPHIS-P  TO DEBUG    OF CRTBNDCL-REC.
           MOVE     CMBNDD  OF PCMPHIS-P  TO BNDDIR   OF CRTBNDCL-REC.
           CALL     "CLP0023" USING CRTBNDCL-REC
                                    RET-CODE
           END-CALL.

       CRT-ICL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN EINES CL-MODULS                                CLLE
     ‚*--------------------------------------------------------------
       CREATE-ILE-CL-MODULE SECTION.
       CRT-ILM-00.

           INITIALIZE RET-CODE.

       CER-ILM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN EINES COMMANDS                                  CMD
     ‚*--------------------------------------------------------------
       CREATE-CMD SECTION.
       CRT-CMD-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND PROGRAMM ERSTELLEN
           INITIALIZE CRTCMD-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTCMD-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTCMD-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTCMD-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTCMD-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTCMD-REC.
           MOVE     CMEXCP  OF PCMPHIS-P  TO EXCPGM   OF CRTCMD-REC.
           MOVE     CMEXCL  OF PCMPHIS-P  TO EXCLIB   OF CRTCMD-REC.
           MOVE     CMOVRP  OF PCMPHIS-P  TO OVRPGM   OF CRTCMD-REC.
           MOVE     CMOVRL  OF PCMPHIS-P  TO OVRLIB   OF CRTCMD-REC.
           MOVE     CMPRDL  OF PCMPHIS-P  TO PRDLIB   OF CRTCMD-REC.
           CALL     "CLP0024" USING CRTCMD-REC
                                    RET-CODE
           END-CALL.

       CRT-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN EINES DISPLAY-FILES                            DSPF
     ‚*--------------------------------------------------------------
       CREATE-DSPF SECTION.
       CRT-DSP-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND DISPLAY ERSTELLEN
           INITIALIZE CRTDSPF-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTDSPF-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTDSPF-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTDSPF-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTDSPF-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTDSPF-REC.
           MOVE     CMRSTD  OF PCMPHIS-P  TO RSTDSP   OF CRTDSPF-REC.
           MOVE     CMDFRW  OF PCMPHIS-P  TO DFRWRT   OF CRTDSPF-REC.
           CALL     "CLP0030" USING CRTDSPF-REC
                                    RET-CODE
           END-CALL.

     ‚* FÜLLEN DER PCFGFFD
           IF       RET-CODE = SPACES
                    PERFORM  FILL-XMLFFD
           END-IF.

       CRT-DSP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN EINER PHYSISCHEN DATEI                           PF
     ‚*--------------------------------------------------------------
       CREATE-PF SECTION.
       CRT-PFI-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND DISPLAY ERSTELLEN
           INITIALIZE CRTPF-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTPF-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTPF-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTPF-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTPF-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTPF-REC.
           MOVE     CMPFSZ  OF PCMPHIS-P  TO DLSIZE   OF CRTPF-REC.
           MOVE     CMPFEX  OF PCMPHIS-P  TO DLMAXE   OF CRTPF-REC.
           MOVE     CMPFME  OF PCMPHIS-P  TO DLEXT    OF CRTPF-REC.
           CALL     "CLP0031" USING CRTPF-REC
                                    RET-CODE
           END-CALL.

     ‚* FÜLLEN DER PCFGFFD
           IF       RET-CODE = SPACES
                    PERFORM  FILL-XMLFFD
           END-IF.

       CRT-PFI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN EINER LOGISCHEN DATEI                            LF
     ‚*--------------------------------------------------------------
       CREATE-LF SECTION.
       CRT-LGF-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND DISPLAY ERSTELLEN
           INITIALIZE CRTLF-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTLF-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTLF-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTLF-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTLF-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTLF-REC.
           CALL     "CLP0032" USING CRTLF-REC
                                    RET-CODE
           END-CALL.

     ‚* FÜLLEN DER PCFGFFD
           IF       RET-CODE = SPACES
                    PERFORM  FILL-XMLFFD
           END-IF.

       CRT-LGF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN EINER TABELLE/SRTSEQ                            TBL
     ‚*--------------------------------------------------------------
       CREATE-TBL SECTION.
       CRT-TBL-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND DISPLAY ERSTELLEN
           INITIALIZE CRTTBL-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTTBL-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTTBL-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTTBL-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTTBL-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTTBL-REC.
           MOVE     CMTTYP  OF PCMPHIS-P  TO TBLTYPE  OF CRTTBL-REC.
           CALL     "CLP0035" USING CRTTBL-REC
                                    RET-CODE
           END-CALL.

       CRT-TBL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN EINES PNLGRP                                 PNLGRP
     ‚*--------------------------------------------------------------
       CREATE-PNLGRP SECTION.
       CRT-PNL-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND DISPLAY ERSTELLEN
           INITIALIZE CRTPNLGRP-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTPNLGRP-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTPNLGRP-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTPNLGRP-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTPNLGRP-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTPNLGRP-REC.
           CALL     "CLP0028" USING CRTPNLGRP-REC
                                    RET-CODE
           END-CALL.

       CRT-PNL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* C-MODUL ERSTELLEN                                           C
     ‚*--------------------------------------------------------------
       CREATE-C-MODULE SECTION.
       CRT-CMO-00.

           INITIALIZE RET-CODE.
     ‚* STRUKTUR FÜLLEN UND PROGRAMM ERSTELLEN
           INITIALIZE CRTCMOD-REC.
           MOVE     CMOLIB  OF PCMPHIS-P  TO TOLIB    OF CRTCMOD-REC.
           MOVE     CMOPGM  OF PCMPHIS-P  TO TOPGM    OF CRTCMOD-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO FROMLIB  OF CRTCMOD-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FROMFILE OF CRTCMOD-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO FROMMBR  OF CRTCMOD-REC.
           MOVE     CMTRLS  OF PCMPHIS-P  TO TGTRLS   OF CRTCMOD-REC.
           MOVE     CMDBG   OF PCMPHIS-P  TO DEBUG    OF CRTCMOD-REC.
           MOVE     CMCDEF  OF PCMPHIS-P  TO DEFINE   OF CRTCMOD-REC.
           CALL     "CLP0025" USING CRTCMOD-REC
                                    RET-CODE
           END-CALL.

       CRT-CMO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PROGRAMM VOR DEM CP AUFRUFEN
     ‚*--------------------------------------------------------------
       CALL-VOR-COMPILE SECTION.
       CAL-VCP-00.

           INITIALIZE F3, F12.
     ‚* PROGRAMM AUFRUFEN
           MOVE     CMVCPG OF PCMPHIS-P TO CALL-PGM.
           MOVE     CMVCPA OF PCMPHIS-P TO CALL-PARM.
           CALL     "HLP0008" USING CALL-PGM
                                    CALL-PARM
                                    BATCH-KZ
                                    F3
                                    F12
           END-CALL.

       CAL-VCP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PROGRAMM NACH DEM CP AUFRUFEN
     ‚*--------------------------------------------------------------
       CALL-NACH-COMPILE SECTION.
       CAL-NCP-00.

           INITIALIZE F3, F12.
     ‚* PROGRAMM AUFRUFEN
           MOVE     CMNCPG OF PCMPHIS-P TO CALL-PGM.
           MOVE     CMNCPA OF PCMPHIS-P TO CALL-PARM.
           CALL     "HLP0008" USING CALL-PGM
                                    CALL-PARM
                                    BATCH-KZ
                                    F3
                                    F12
           END-CALL.

       CAL-NCP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER BERECHTIGUNG DES ERSTELLTEN OBJEKTS
     ‚*--------------------------------------------------------------
       CHANGE-BERECHTIGUNG SECTION.
       CHG-RCT-00.

           INITIALIZE RET-CODE.
           INITIALIZE USRAUT-WRK.
     ‚* WELCHE BERECHTIGUNG SOLL GESETZT WERDEN?
           EVALUATE CMRECT OF PCMPHIS-P
                    WHEN 1
                         MOVE "*USE"     TO USRAUT-WRK
                    WHEN 2
                         MOVE "*EXCLUDE" TO USRAUT-WRK
                    WHEN OTHER
                         GO TO CHG-RCT-90
           END-EVALUATE.

     ‚* APG IMMER AUF *ALL SETZEN
           INITIALIZE GRTOBJAUT-REC.
           MOVE     CMOLIB OF PCMPHIS-P TO LIBNAME  OF GRTOBJAUT-REC.
           MOVE     CMOPGM OF PCMPHIS-P TO FILENAME OF GRTOBJAUT-REC.
           MOVE     OBJTYPE-WRK         TO TYPNAME  OF GRTOBJAUT-REC.
           MOVE     "APG"               TO TUSER    OF GRTOBJAUT-REC.
           MOVE     "*ALL"              TO AUT      OF GRTOBJAUT-REC.
           CALL     "CLP0045" USING GRTOBJAUT-REC
                                    RET-CODE
           END-CALL.

     ‚* BERECHTIGUNG ÄNDERN -> QPGMR
           INITIALIZE GRTOBJAUT-REC.
           MOVE     CMOLIB OF PCMPHIS-P TO LIBNAME  OF GRTOBJAUT-REC.
           MOVE     CMOPGM OF PCMPHIS-P TO FILENAME OF GRTOBJAUT-REC.
           MOVE     OBJTYPE-WRK         TO TYPNAME  OF GRTOBJAUT-REC.
           MOVE     "QPGMR"             TO TUSER    OF GRTOBJAUT-REC.
           MOVE     USRAUT-WRK          TO AUT      OF GRTOBJAUT-REC.
           CALL     "CLP0045" USING GRTOBJAUT-REC
                                    RET-CODE
           END-CALL.

     ‚* BERECHTIGUNG ÄNDERN -> *PUBLIC
           INITIALIZE GRTOBJAUT-REC.
           MOVE     CMOLIB OF PCMPHIS-P TO LIBNAME  OF GRTOBJAUT-REC.
           MOVE     CMOPGM OF PCMPHIS-P TO FILENAME OF GRTOBJAUT-REC.
           MOVE     OBJTYPE-WRK         TO TYPNAME  OF GRTOBJAUT-REC.
           MOVE     "*PUBLIC"           TO TUSER    OF GRTOBJAUT-REC.
           MOVE     USRAUT-WRK          TO AUT      OF GRTOBJAUT-REC.
           CALL     "CLP0045" USING GRTOBJAUT-REC
                                    RET-CODE
           END-CALL.

       CHG-RCT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DES EIGNERS
     ‚*--------------------------------------------------------------
       CHANGE-OWNER SECTION.
       CHG-OWN-00.

     ‚* EIGNER JETZT ÄNDERN
           INITIALIZE CHGOBJOWN-REC.
           MOVE     CMOLIB OF PCMPHIS-P TO LIBNAME  OF CHGOBJOWN-REC.
           MOVE     CMOPGM OF PCMPHIS-P TO FILENAME OF CHGOBJOWN-REC.
           MOVE     OBJTYPE-WRK         TO TYPNAME  OF CHGOBJOWN-REC.
           MOVE     CMNOWN OF PCMPHIS-P TO NEWOWN   OF CHGOBJOWN-REC.
           CALL     "CLP0054" USING CHGOBJOWN-REC
                                    RET-CODE
           END-CALL.

       CHG-OWN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER PCFGFFD
     ‚*--------------------------------------------------------------
       FILL-XMLFFD SECTION.
       FIL-FFD-00.

           CALL     "CMP0005" USING CMOLIB OF PCMPHIS-P
                                    CMOPGM OF PCMPHIS-P
           END-CALL.

       FIL-FFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE SCRAMBLEN
     ‚*--------------------------------------------------------------
       SCRAMBLE-SOURCE SECTION.
       SCR-SRC-00.

     ‚* DAS SCRAMBLEN KOPIERT DIE SOURCE IN DIE QTEMP, DIESE WIRD
     ‚* DANN AUCH GEWANDELT UND HINTERHER AUCH IN DER DB ANGEZEIGT
           EVALUATE SRCTYPE OF RTVMBRD-REC
                    WHEN "CBLLE"
                    WHEN "CBL"
                         MOVE "SCRAMBLE_MAIN" TO SCMKEY-WRK
                         MOVE "SCRAMBLE"      TO AKTION-WRK
                         SET  SCM-PTR         TO NULL
                         SET  SCM-INP-PTR     TO NULL
                         CALL "CMP0007" USING CMSLIB  OF PCMPHIS-P
                                              CMSFIL  OF PCMPHIS-P
                                              CMSMBR  OF PCMPHIS-P
                                              AKTION-WRK
                                              SCMKEY-WRK
                                              SCM-INP-PTR
                                              SCM-PTR
                         END-CALL
           END-EVALUATE.

       SCR-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN DES EINTRAGS
     ‚*--------------------------------------------------------------
       UPDATE-PCMPHIS SECTION.
       UPD-CMP-00.

     ‚* EINTRAG IN DER PCMPHIS AKTUALISIEREN
           INITIALIZE PCMPHISF OF PCMPHIS-P.
           MOVE     X-CMLFDN       TO CMLFDN OF PCMPHIS-P.
           READ     PCMPHIS-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             ENDE USER/DATUM/ZEIT FÜLLEN
                    PERFORM  COPY-GET-TIME
                    MOVE     T-USER         TO CMEUSR OF PCMPHIS-P
                    MOVE     T-TERM         TO CMEBS  OF PCMPHIS-P
                    MOVE     PGM-WRK        TO CMEPGM OF PCMPHIS-P
                    MOVE     DATE-8         TO CMEDTA OF PCMPHIS-P
                    MOVE     TIME-6         TO CMETIA OF PCMPHIS-P
     ‚*             LETZTE MODIFIKATION IMMER FÜLLEN
                    MOVE     T-USER         TO CMMSER OF PCMPHIS-P
                    MOVE     T-TERM         TO CMMBS  OF PCMPHIS-P
                    MOVE     PGM-WRK        TO CMMPGM OF PCMPHIS-P
                    MOVE     DATE-8         TO CMMDTA OF PCMPHIS-P
                    MOVE     TIME-6         TO CMMTIA OF PCMPHIS-P
     ‚*             VERARBEITUNGSSTATUS SETZEN
                    IF   CMFCDE-WRK NOT = SPACES
                         MOVE "F"           TO CMVSTS OF PCMPHIS-P
                         MOVE CMFCDE-WRK    TO CMFCDE OF PCMPHIS-P
                    ELSE
                         MOVE "V"           TO CMVSTS OF PCMPHIS-P
                         MOVE SPACES        TO CMFCDE OF PCMPHIS-P
                    END-IF
                    REWRITE PCMPHIS-P
           END-IF.

       UPD-CMP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSION EINS HOCHZÄHLEN
     ‚*--------------------------------------------------------------
       UPDATE-VERSION SECTION.
       UPD-VER-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "CP"                   TO CFID   OF PCONFIG-P.
           MOVE     CMSLIB  OF PCMPHIS-P   TO CFKEY  OF PCONFIG-P.
           MOVE     CMSFIL  OF PCMPHIS-P   TO CFKEY2 OF PCONFIG-P.
           MOVE     CMSMBR  OF PCMPHIS-P   TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             VERSION UPDATEN
                    ADD    1               TO CFNM04 OF PCONFIG-P
     ‚*             LETZTE MODIFIKATION IMMER FÜLLEN
                    MOVE     T-USER         TO CFMSER OF PCONFIG-P
                    MOVE     T-TERM         TO CFMBS  OF PCONFIG-P
                    MOVE     PGM-WRK        TO CFMPGM OF PCONFIG-P
                    MOVE     DATE-8         TO CFMDTA OF PCONFIG-P
                    MOVE     TIME-6         TO CFMTIA OF PCONFIG-P
                    REWRITE  PCONFIG-P
           END-IF.

       UPD-VER-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER ABBRUCHNACHRICHT
     ‚*--------------------------------------------------------------
       SEND-BREAKMESSAGE SECTION.
       SND-MSG-00.

           INITIALIZE RET-CODE.
     ‚* JETZT WERTE FÜLLEN
           INITIALIZE SNDPGMMSG-REC.
           MOVE     CMFCDE-WRK     TO MSGDTA  OF SNDPGMMSG-REC(1:10).
           MOVE     ":"            TO MSGDTA  OF SNDPGMMSG-REC(11:1).

           MOVE     CMFCDE-WRK     TO CPY-TXID.
           MOVE     SPRACHE-WRK    TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT     TO MSGDTA  OF SNDPGMMSG-REC(13:).

           MOVE     "CPF9898"      TO MSGID   OF SNDPGMMSG-REC.
           MOVE     "*ESCAPE"      TO MSGTYPE OF SNDPGMMSG-REC.
           MOVE     "*PRV"         TO TOPGMQ  OF SNDPGMMSG-REC.
           MOVE     "*TOPGMQ"      TO TOMSGQ  OF SNDPGMMSG-REC.
           CALL     "CLP0016" USING SNDPGMMSG-REC
                                    RET-CODE
           END-CALL.

       SND-MSG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER MEMBER-DATEN
     ‚*--------------------------------------------------------------
       GET-MEMBER-DATEN SECTION.
       GET-MBR-00.

           INITIALIZE RET-CODE.
     ‚* TEILDATEIENART ERMITTELN
           INITIALIZE RTVMBRD-REC.
           MOVE     CMSLIB  OF PCMPHIS-P  TO OBJLIB   OF RTVMBRD-REC.
           MOVE     CMSFIL  OF PCMPHIS-P  TO FILENAME OF RTVMBRD-REC.
           MOVE     CMSMBR  OF PCMPHIS-P  TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

       GET-MBR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES OBJEKTTYPEN
     ‚*--------------------------------------------------------------
       GET-OBJTYPE SECTION.
       GET-OBT-00.

           INITIALIZE RET-CODE.
     ‚* OBJEKTTYP ERMITTELN
           INITIALIZE RTVOBJD-REC.
           MOVE     CMOLIB OF PCMPHIS-P TO OBJLIB OF RTVOBJD-REC.
           MOVE     CMOPGM OF PCMPHIS-P TO OBJ    OF RTVOBJD-REC.
           CALL     "HLP0010" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE OBJTYPE OF RTVOBJD-REC TO OBJTYPE-WRK
           END-IF.

       GET-OBJ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-TEILDATEI SECTION.
       CHK-TED-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB SOURCE VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     CMSLIB  OF PCMPHIS-P   TO LIB     OF CHKOBJ-REC.
           MOVE     CMSFIL  OF PCMPHIS-P   TO OBJ     OF CHKOBJ-REC.
           MOVE     CMSMBR  OF PCMPHIS-P   TO MBR     OF CHKOBJ-REC.
           MOVE     "*FILE"                TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TED-90
           END-IF.

       CHK-TED-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
