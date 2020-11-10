       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* RGZPFM AUF ALLE DATEIEN                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  RGZPFM AUSFÜHREN                             *
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

      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP8004".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  FILE-WRK                      PIC X(10).
       01  RGZPFM-REC                    TYPE RGZPFM-TYPE.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP8004" TO PGM-WRK.

     ‚* AJAX-DATEIEN
           MOVE     "PAJXTHM" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PAJXTXT" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.

     ‚* HERMES-DATEIEN
           MOVE     "PPGMLIB" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PTAGLIB" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PPGMLST" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.

     ‚* DIENST-DATEIEN
           MOVE     "PKDPLAN" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PKDBST1" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PKDBEIN" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PKDKKAS" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.

     ‚* AZUBIÜBUNGS-DATEIEN
           MOVE     "PAZUUEB" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PAZUSRC" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PAZUBER" TO FILE-WRK.
           PERFORM  CALL-RGZPFM.

     ‚* CHKSRC-DATEIEN
           MOVE     "PCHKSRC" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCHKANA" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCHKHGH" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PTYPHGH" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PSEUHST" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.

     ‚* COMPILER-DATEIEN
           MOVE     "PCMPHIS" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCMPSCM" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGFFD" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.

     ‚* PROJEKTVERWALTUNG
           MOVE     "PPRJHED" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PPRJSRC" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PPRJVER" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.

     ‚* OPL-DATEIEN
           MOVE     "POPLLST" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.

     ‚* CONFIG-DATEIEN
           MOVE     "PCONFIG" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGTXT" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGDAT" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGSEQ" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGAUT" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGPGM" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGPGT" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PPGMLOG" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PHINTXT" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PARCHIV" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PVARDB"  TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGCMD" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGMAI" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGSTS" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PCFGIDX" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.
           MOVE     "PTXTFIT" TO  FILE-WRK.
           PERFORM  CALL-RGZPFM.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFRUFEN DES RGZPFM
     ‚*--------------------------------------------------------------
       CALL-RGZPFM SECTION.
       CAL-RGZ-00.

           INITIALIZE RET-CODE.
     ‚* DATEI REORGANISIEREN
           INITIALIZE RGZPFM-REC.
           MOVE     FILE-WRK TO FILENAME OF RGZPFM-REC.
           CALL     "CLP0019" USING RGZPFM-REC
                                    RET-CODE
           END-CALL.

       CAL-RGZ-90.
           EXIT.
      /
