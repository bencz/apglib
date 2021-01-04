       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP0000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* COMPILER-STEUERPROGRAMM                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  STARTEN COMPILERVORGANG                      *
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
     ‚* PHYSISCHE DATEI KONFIGURATION
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI COMPILER-HISTORIE
     ‚*-------------------------------------------------------------
           SELECT PCMPHIS-DP
                  ASSIGN       TO  DATABASE-PCMPHIS
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- KONFIGURATION
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
     ‚*--- COMPILER-HISTORIE
        FD PCMPHIS-DP
           LABEL RECORDS ARE STANDARD.
        01 PCMPHIS-P.
           COPY DDS-ALL-FORMATS OF PCMPHIS.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP0000".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.
       01  SBMJOB-REC                    TYPE SBMJOB-TYPE.
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.

       01  DEBUG-WRK                     LIKE CMDBG  OF PCMPHIS-P.
       01  USER-WRK                      LIKE CMUSPR OF PCMPHIS-P.
       01  DFRWRT-WRK                    LIKE CMDFRW OF PCMPHIS-P.
       01  RSTDSP-WRK                    LIKE CMRSTD OF PCMPHIS-P.
       01  CRTOBJ-WRK                    LIKE CMCRTO OF PCMPHIS-P.
       01  SCRAMBLE-WRK                  LIKE CMSCRM OF PCMPHIS-P.

       01  USER-EINGABE                  PIC 9(1).
      /
     ‚*-------------------------------------------------------------
     ˜* PARAMETER IMMER EINS GRÖßER ALS IM COMMAND CX (WEGEN PASSATR)
       LINKAGE SECTION.
     ‚* OBJEKTNAME/BIBLIOTHEK
       01  X-CPPGM                       PIC X(11).
       01  X-CPLIB                       PIC X(11).
     ‚* SOURCENAME/BIBLIOTHEK
       01  X-CPSFIL                      PIC X(11).
       01  X-CPSLIB                      PIC X(11).
       01  X-CPSMBR                      PIC X(11).
       01  X-CPSTYP                      PIC X(11).
     ‚* CBLLE-EINSTELLUNGEN
       01  X-CPRLS                       PIC X(11).
       01  X-CPACT                       PIC X(11).
       01  X-CPBND                       PIC X(11).
       01  X-CPDBG                       PIC X(2).
       01  X-CPOPT                       PIC X(7).
       01  X-CPOBJ                       PIC X(2).
     ‚* CMD-EINSTELLUNGEN
       01  X-OVRPGM                      PIC X(11).
       01  X-OVRLIB                      PIC X(11).
       01  X-EXCPGM                      PIC X(11).
       01  X-EXCLIB                      PIC X(11).
       01  X-PRDLIB                      PIC X(11).
     ‚* DSP-EINSTELLUNGEN
       01  X-RSTDSP                      PIC X(2).
       01  X-DFRWRT                      PIC X(2).
     ‚* PF-EINSTELLUNGEN
       01  X-PFSIZE                      PIC X(11).
       01  X-PFEXT                       PIC X(6).
       01  X-PFMEX                       PIC X(6).
     ‚* TBL-EINSTELLUNGEN
       01  X-TBLTYPE                     PIC X(11).
     ‚* C-EINSTELLUNGEN
       01  X-CDEFINE                     PIC X(81).
     ‚* SONSTIGE EINSTELLUNGEN
       01  X-CPREC                       PIC X(2).
       01  X-CPINTA                      PIC X(2).
       01  X-CPNOWN                      PIC X(11).
       01  X-CPVCPGM                     PIC X(11).
       01  X-CPVCLIB                     PIC X(11).
       01  X-CPVCPRM                     PIC X(151).
       01  X-CPNCPGM                     PIC X(11).
       01  X-CPNCLIB                     PIC X(11).
       01  X-CPNCPRM                     PIC X(151).
       01  X-CPSCRM                      PIC X(2).
       01  X-PRMOVR                      PIC X(5).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CPPGM
                                X-CPLIB
                                X-CPSFIL
                                X-CPSLIB
                                X-CPSMBR
                                X-CPSTYP
                                X-CPRLS
                                X-CPACT
                                X-CPBND
                                X-CPDBG
                                X-CPOPT
                                X-CPOBJ
                                X-OVRPGM
                                X-OVRLIB
                                X-EXCPGM
                                X-EXCLIB
                                X-PRDLIB
                                X-RSTDSP
                                X-DFRWRT
                                X-PFSIZE
                                X-PFEXT
                                X-PFMEX
                                X-TBLTYPE
                                X-CDEFINE
                                X-CPREC
                                X-CPINTA
                                X-CPNOWN
                                X-CPVCPGM
                                X-CPVCLIB
                                X-CPVCPRM
                                X-CPNCPGM
                                X-CPNCLIB
                                X-CPNCPRM
                                X-CPSCRM
                                X-PRMOVR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CMP0000" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCMPHIS-DP
                          PCONFIG-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-CPPGM(2:)  = SPACES
            OR      X-CPLIB(2:)  = SPACES
            OR      X-CPSFIL(2:) = SPACES
            OR      X-CPSLIB(2:) = SPACES
            OR      X-CPSMBR(2:) = SPACES
                    GO TO ENDE
           END-IF.

     ‚* MIT ANZEIGE ODER OHNE?
           INITIALIZE USER-EINGABE.
           IF       X-PRMOVR(2:) = "*YES"
                    MOVE 1 TO USER-EINGABE
           END-IF.

     ‚* PRÜFEN OB ES DIE TEILDATEI GIBT
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* WELCHE TEILDATEI-ART HABEN WIR?
           PERFORM  GET-MEMBER-DATEN.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* WENN DER USER NICHTS EINGEGEBEN HAT, DANN NUN DIE DATEN
     ‚* AUS DER CONFIG-DATEI LESEN
           IF       USER-EINGABE = ZEROES
                    PERFORM GET-CONFIG
                    IF   RET-CODE NOT = SPACES
                         GO TO ENDE
                    END-IF
           END-IF.

     ‚* AUFBEREITEN DER PARAMETER
           PERFORM  CHECK-PARAMETER.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB EINTRAG SCHON VORHANDEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "CP"                  TO CFID   OF CFG-CPY.
           MOVE     X-CPSLIB(2:)          TO CFKEY  OF CFG-CPY.
           MOVE     X-CPSFIL(2:)          TO CFKEY2 OF CFG-CPY.
           MOVE     X-CPSMBR(2:)          TO CFKEY3 OF CFG-CPY.
           MOVE     SPACES                TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
     ‚*             GGF. JETZT EINTRAG IN PCONFIG EINTRAGEN
                    PERFORM  WRITE-PCONFIG
           ELSE
                    PERFORM  UPDATE-PCONFIG
           END-IF.

     ‚* JETZT DATENSATZ IN DER PCMPHIS SCHREIBEN. DORT WERDEN ALLE
     ‚* EINSTELLUNGEN HINTERLEGT. DIESER DIENT DEM CP-PROGRAMM ALS
     ‚* GRUNDLAGE FÜR DAS WANDELN.
           PERFORM  WRITE-PCMPHIS.

     ‚* ALLE PARAMETER IN ORDNUNG. JETZT KANN GEWANDELT WERDEN.
     ‚* SOLL DIES INTERAKTIV ODER IM BATCH GESCHEHEN?
           IF       X-CPINTA(2:1) = "1"
                    PERFORM CP-INTERACT
           ELSE
                    PERFORM CP-BATCH
           END-IF.

       ENDE.
           CLOSE                 PCONFIG-DP
                                 PCMPHIS-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* INTERAKTIV WANDELN
     ‚*--------------------------------------------------------------
       CP-INTERACT SECTION.
       COP-INT-00.

           INITIALIZE RET-CODE.
     ‚* DAS CMP0001 ÜBERNIMMT DAS WANDELN
           CALL     "CMP0001" USING LFDN-WRK
                                    RET-CODE
           END-CALL.

       COP-INT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* IM BATCH WANDELN
     ‚*--------------------------------------------------------------
       CP-BATCH SECTION.
       COP-BTC-00.

           INITIALIZE RET-CODE.
     ‚* INITIALISIEREN DES SBMJOB-RECORDS UND STARTEN
           INITIALIZE SBMJOB-REC.
           MOVE     "CMP0001"              TO PGMNAM  OF SBMJOB-REC.
           MOVE     LFDN-WRK               TO PARM    OF SBMJOB-REC.
           MOVE     X-CPPGM(2:)            TO JOBNAME OF SBMJOB-REC.
           MOVE     "QBATCH"               TO USRSBS  OF SBMJOB-REC.
           MOVE     T-USER                 TO TUSER   OF SBMJOB-REC.
           MOVE     "*USRPRF"              TO JOBD    OF SBMJOB-REC.
           MOVE     "*DFT"                 TO INQMSGR OF SBMJOB-REC.
           MOVE     SPACES                 TO SBMNAME OF SBMJOB-REC.
           MOVE     SPACES                 TO SBMUSER OF SBMJOB-REC.
           MOVE     SPACES                 TO SBMNUM  OF SBMJOB-REC.
           CALL     "CLP0040" USING SBMJOB-REC
                                    RET-CODE
           END-CALL.

       COP-BTC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINES PCMPHIS-SATZES FÜR DAS COMPILIEREN
     ‚*--------------------------------------------------------------
       WRITE-PCMPHIS SECTION.
       WRI-PCM-00.

           INITIALIZE PCMPHISF OF PCMPHIS-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PCMPHIS" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK                  TO CMLFDN OF PCMPHIS-P.
     ‚* INFORMATIONEN AUS CP
           MOVE     X-CPLIB(2:)               TO CMOLIB OF PCMPHIS-P.
           MOVE     X-CPPGM(2:)               TO CMOPGM OF PCMPHIS-P.
           MOVE     X-CPSLIB(2:)              TO CMSLIB OF PCMPHIS-P.
           MOVE     X-CPSFIL(2:)              TO CMSFIL OF PCMPHIS-P.
           MOVE     X-CPSMBR(2:)              TO CMSMBR OF PCMPHIS-P.
           MOVE     X-CPSTYP(2:)              TO CMSTYP OF PCMPHIS-P.
     ‚* CBL/CBLLE-EINSTELLUNGEN
           MOVE     X-CPRLS(2:)               TO CMTRLS OF PCMPHIS-P.
           MOVE     X-CPACT(2:)               TO CMACTG OF PCMPHIS-P.
           MOVE     X-CPBND(2:)               TO CMBNDD OF PCMPHIS-P.
           MOVE     DEBUG-WRK                 TO CMDBG  OF PCMPHIS-P.
           MOVE     USER-WRK                  TO CMUSPR OF PCMPHIS-P.
           MOVE     X-CPOPT(2:)               TO CMOPTI OF PCMPHIS-P.
           MOVE     CRTOBJ-WRK                TO CMCRTO OF PCMPHIS-P.
           MOVE     SCRAMBLE-WRK              TO CMSCRM OF PCMPHIS-P.
     ‚* CMD-EINSTELLUNGEN
           MOVE     X-OVRPGM(2:)              TO CMOVRP OF PCMPHIS-P.
           MOVE     X-OVRLIB(2:)              TO CMOVRL OF PCMPHIS-P.
           MOVE     X-EXCPGM(2:)              TO CMEXCP OF PCMPHIS-P.
           MOVE     X-EXCLIB(2:)              TO CMEXCL OF PCMPHIS-P.
           MOVE     X-PRDLIB(2:)              TO CMPRDL OF PCMPHIS-P.
     ‚* DSP-EINSTELLUNGEN
           MOVE     RSTDSP-WRK                TO CMRSTD OF PCMPHIS-P.
           MOVE     DFRWRT-WRK                TO CMDFRW OF PCMPHIS-P.
     ‚* PF-EINSTELLUNGEN
           MOVE     X-PFSIZE(2:)              TO CMPFSZ OF PCMPHIS-P.
           MOVE     X-PFEXT(2:)               TO CMPFEX OF PCMPHIS-P.
           MOVE     X-PFMEX(2:)               TO CMPFME OF PCMPHIS-P.
     ‚* TBL-EINSTELLUNGEN
           MOVE     X-TBLTYPE(2:)             TO CMTTYP OF PCMPHIS-P.
     ‚* C-EINSTELLUNGEN
           MOVE     X-CDEFINE(2:)             TO CMCDEF OF PCMPHIS-P.
     ‚* SONSTIGE EINSTELLUNGEN
           MOVE     X-CPREC(2:)               TO CMRECT OF PCMPHIS-P.
           MOVE     X-CPINTA(2:)              TO CMINTA OF PCMPHIS-P.
           MOVE     X-CPNOWN(2:)              TO CMNOWN OF PCMPHIS-P.
           MOVE     X-CPVCPGM(2:)             TO CMVCPG OF PCMPHIS-P.
           MOVE     X-CPVCLIB(2:)             TO CMVCLI OF PCMPHIS-P.
           MOVE     X-CPVCPRM(2:)             TO CMVCPA OF PCMPHIS-P.
           MOVE     X-CPNCPGM(2:)             TO CMNCPG OF PCMPHIS-P.
           MOVE     X-CPNCLIB(2:)             TO CMNCLI OF PCMPHIS-P.
           MOVE     X-CPNCPRM(2:)             TO CMNCPA OF PCMPHIS-P.
     ‚* INFORMATIONEN AUS RTVMBRD
           MOVE     CRTDATE   OF RTVMBRD-REC  TO CMRTDE OF PCMPHIS-P.
           MOVE     CHGDATE   OF RTVMBRD-REC  TO CMRTDC OF PCMPHIS-P.
           MOVE     SAVDATE   OF RTVMBRD-REC  TO CMRTDS OF PCMPHIS-P.
           MOVE     RSTDATE   OF RTVMBRD-REC  TO CMRTDR OF PCMPHIS-P.
           MOVE     USEDATE   OF RTVMBRD-REC  TO CMRTUD OF PCMPHIS-P.
           MOVE     USECOUNT  OF RTVMBRD-REC  TO CMRTUC OF PCMPHIS-P.
           MOVE     NBRCURRCD OF RTVMBRD-REC  TO CMRTZA OF PCMPHIS-P.
           MOVE     NBRDLTRCD OF RTVMBRD-REC  TO CMRTZD OF PCMPHIS-P.
           MOVE     MBRTEXT   OF RTVMBRD-REC  TO CMRTMT OF PCMPHIS-P.
     ‚* START USER/DATUM/ZEIT FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                    TO CMAUSR OF PCMPHIS-P.
           MOVE     T-TERM                    TO CMABS  OF PCMPHIS-P.
           MOVE     PGM-WRK                   TO CMAPGM OF PCMPHIS-P.
           MOVE     DATE-8                    TO CMADTA OF PCMPHIS-P.
           MOVE     TIME-6                    TO CMATIA OF PCMPHIS-P.
           WRITE    PCMPHIS-P.

       WRI-PCM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PCONFIG-SATZ SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-PCONFIG SECTION.
       WRI-CFG-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
     ‚* KEY FÜLLEN
           MOVE     "CP"                      TO CFID   OF PCONFIG-P.
           MOVE     X-CPSLIB(2:)              TO CFKEY  OF PCONFIG-P.
           MOVE     X-CPSFIL(2:)              TO CFKEY2 OF PCONFIG-P.
           MOVE     X-CPSMBR(2:)              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                    TO CFKEY4 OF PCONFIG-P.
     ‚* INFORMATIONEN AUS CP
           MOVE     X-CPSTYP(2:)              TO CFIN01 OF PCONFIG-P.
           MOVE     USER-WRK                  TO CFIN02 OF PCONFIG-P.
           MOVE     X-CPRLS(2:)               TO CFIN03 OF PCONFIG-P.
           MOVE     X-CPACT(2:)               TO CFIN04 OF PCONFIG-P.
           MOVE     X-CPOPT(2:)               TO CFIN05 OF PCONFIG-P.
           MOVE     X-CPBND(2:)               TO CFIN06 OF PCONFIG-P.
           IF       X-CPOBJ(2:1) IS NUMERIC
                    MOVE X-CPOBJ(2:1)         TO CFFK06 OF PCONFIG-P
           ELSE
                    MOVE ZEROES               TO CFFK06 OF PCONFIG-P
           END-IF.
           IF       X-CPSCRM(2:1) IS NUMERIC
                    MOVE X-CPSCRM(2:1)        TO CFFK07 OF PCONFIG-P
           ELSE
                    MOVE ZEROES               TO CFFK07 OF PCONFIG-P
           END-IF.
     ‚* CMD-EINSTELLUNGEN
           MOVE     X-PRDLIB(2:)              TO CFIN07 OF PCONFIG-P.
           MOVE     X-OVRPGM(2:)              TO CFIN08 OF PCONFIG-P.
           MOVE     X-OVRLIB(2:)              TO CFIN09 OF PCONFIG-P.
           MOVE     X-EXCPGM(2:)              TO CFIN10 OF PCONFIG-P.
           MOVE     X-EXCLIB(2:)              TO CFIN11 OF PCONFIG-P.
     ‚* DSP-EINSTELLUNGEN
           IF       X-RSTDSP(2:1) IS NUMERIC
                    MOVE X-RSTDSP(2:1)        TO CFFK04 OF PCONFIG-P
           ELSE
                    MOVE 1                    TO CFFK04 OF PCONFIG-P
           END-IF.
           IF       X-DFRWRT(2:1) IS NUMERIC
                    MOVE X-DFRWRT(2:1)        TO CFFK05 OF PCONFIG-P
           ELSE
                    MOVE 1                    TO CFFK05 OF PCONFIG-P
           END-IF.
     ‚* TBL-EINSTELLUNGEN
           MOVE     X-TBLTYPE(2:)             TO CFIN13 OF PCONFIG-P.
     ‚* C-EINSTELLUNGEN
           MOVE     X-CDEFINE(2:)             TO CFBG01 OF PCONFIG-P.
     ‚* SONSTIGE EINSTELLUNGEN
           IF       X-CPDBG(2:1) IS NUMERIC
                    MOVE X-CPDBG(2:1)         TO CFFK01 OF PCONFIG-P
           ELSE
                    MOVE 1                    TO CFFK01 OF PCONFIG-P
           END-IF.
           IF       X-CPREC(2:1) IS NUMERIC
                    MOVE X-CPREC(2:1)         TO CFFK02 OF PCONFIG-P
           ELSE
                    MOVE ZEROES               TO CFFK02 OF PCONFIG-P
           END-IF.
           IF       X-CPINTA(2:1) IS NUMERIC
                    MOVE X-CPINTA(2:1)        TO CFFK03 OF PCONFIG-P
           ELSE
                    MOVE ZEROES               TO CFFK03 OF PCONFIG-P
           END-IF.
           MOVE     X-CPNOWN(2:)              TO CFIN12 OF PCONFIG-P.
           MOVE     X-CPVCPGM(2:)             TO CFDAT1 OF PCONFIG-P.
           MOVE     X-CPVCLIB(2:)             TO CFDAT2 OF PCONFIG-P.
           MOVE     X-CPVCPRM(2:)             TO CFTXT2 OF PCONFIG-P.
           MOVE     X-CPNCPGM(2:)             TO CFDAT3 OF PCONFIG-P.
           MOVE     X-CPNCLIB(2:)             TO CFDAT4 OF PCONFIG-P.
           MOVE     X-CPNCPRM(2:)             TO CFTXT3 OF PCONFIG-P.
     ‚* INFORMATIONEN AUS RTVMBRD
           MOVE     MBRTEXT   OF RTVMBRD-REC  TO CFTXT1 OF PCONFIG-P.
     ‚* START USER/DATUM/ZEIT FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                    TO CFAUSR OF PCONFIG-P.
           MOVE     T-TERM                    TO CFABS  OF PCONFIG-P.
           MOVE     PGM-WRK                   TO CFAPGM OF PCONFIG-P.
           MOVE     DATE-8                    TO CFADTA OF PCONFIG-P.
           MOVE     TIME-6                    TO CFATIA OF PCONFIG-P.
           WRITE    PCONFIG-P.

     ‚* JETZT NOCHMAL CFG-CPY LESEN DAMIT DIES IM ZUGRIFF IST
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "CP"                  TO CFID   OF CFG-CPY.
           MOVE     X-CPSLIB(2:)          TO CFKEY  OF CFG-CPY.
           MOVE     X-CPSFIL(2:)          TO CFKEY2 OF CFG-CPY.
           MOVE     X-CPSMBR(2:)          TO CFKEY3 OF CFG-CPY.
           MOVE     SPACES                TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

       WRI-CFG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PCONFIG-SATZ AKTUALISIEREN
     ‚*--------------------------------------------------------------
       UPDATE-PCONFIG SECTION.
       UPD-CFG-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "CP"                  TO CFID   OF PCONFIG-P.
           MOVE     X-CPSLIB(2:)          TO CFKEY  OF PCONFIG-P.
           MOVE     X-CPSFIL(2:)          TO CFKEY2 OF PCONFIG-P.
           MOVE     X-CPSMBR(2:)          TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             INFORMATIONEN AUS RTVMBRD
                    MOVE MBRTEXT OF RTVMBRD-REC TO CFTXT1 OF PCONFIG-P
     ‚*             INFORMATIONEN AUS CP
                    MOVE     X-CPRLS(2:)        TO CFIN03 OF PCONFIG-P
                    MOVE     X-CPACT(2:)        TO CFIN04 OF PCONFIG-P
                    MOVE     X-CPOPT(2:)        TO CFIN05 OF PCONFIG-P
                    MOVE     X-CPBND(2:)        TO CFIN06 OF PCONFIG-P
     ‚*             USER/DATUM/ZEIT FÜLLEN
                    PERFORM  COPY-GET-TIME
                    MOVE     T-USER             TO CFMSER OF PCONFIG-P
                    MOVE     T-TERM             TO CFMBS  OF PCONFIG-P
                    MOVE     PGM-WRK            TO CFMPGM OF PCONFIG-P
                    MOVE     DATE-8             TO CFMDTA OF PCONFIG-P
                    MOVE     TIME-6             TO CFMTIA OF PCONFIG-P
                    REWRITE  PCONFIG-P
           END-IF.

     ‚* JETZT NOCHMAL CFG-CPY LESEN DAMIT DIES IM ZUGRIFF IST
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "CP"                  TO CFID   OF CFG-CPY.
           MOVE     X-CPSLIB(2:)          TO CFKEY  OF CFG-CPY.
           MOVE     X-CPSFIL(2:)          TO CFKEY2 OF CFG-CPY.
           MOVE     X-CPSMBR(2:)          TO CFKEY3 OF CFG-CPY.
           MOVE     SPACES                TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

       UPD-CFG-90.
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
           MOVE     X-CPSFIL(2:) TO FILENAME OF RTVMBRD-REC.
           MOVE     X-CPSMBR(2:) TO MBRNAME  OF RTVMBRD-REC.
           MOVE     X-CPSLIB(2:) TO OBJLIB   OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

       GET-MBR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER KONFIGURATION
     ‚*--------------------------------------------------------------
       GET-CONFIG SECTION.
       GET-CFG-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB EINTRAG SCHON VORHANDEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "CP"                  TO CFID   OF CFG-CPY.
           MOVE     X-CPSLIB(2:)          TO CFKEY  OF CFG-CPY.
           MOVE     X-CPSFIL(2:)          TO CFKEY2 OF CFG-CPY.
           MOVE     X-CPSMBR(2:)          TO CFKEY3 OF CFG-CPY.
           MOVE     SPACES                TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
     ‚*             DEFAULT-EINTRAG LESEN
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE    "CP"             TO CFID   OF CFG-CPY
                    MOVE    X-CPSLIB(2:)     TO CFKEY  OF CFG-CPY
                    MOVE    SPACES           TO CFKEY2 OF CFG-CPY
                    MOVE    "*"              TO CFKEY3 OF CFG-CPY(1:1)
                    MOVE    SRCTYPE OF RTVMBRD-REC TO
                            CFKEY3 OF CFG-CPY(2:)
                    MOVE    SPACES           TO CFKEY4 OF CFG-CPY
                    PERFORM COPY-CFG-CPY
                    IF   CFG-DA OF CFG-CPY = ZEROES
                         MOVE "1" TO RET-CODE
                    END-IF
           END-IF.

     ‚* INFORMATIONEN AUS CP
           MOVE     CFIN01 OF CFG-CPY         TO X-CPSTYP(2:).
           MOVE     CFIN02 OF CFG-CPY         TO USER-WRK.
           MOVE     CFIN03 OF CFG-CPY         TO X-CPRLS(2:).
           MOVE     CFIN04 OF CFG-CPY         TO X-CPACT(2:).
           MOVE     CFIN05 OF CFG-CPY         TO X-CPOPT(2:).
           MOVE     CFIN06 OF CFG-CPY         TO X-CPBND(2:).
           MOVE     CFFK06 OF CFG-CPY         TO X-CPOBJ(2:).
           MOVE     CFFK07 OF CFG-CPY         TO X-CPSCRM(2:).
     ‚* CMD-EINSTELLUNGEN
           MOVE     CFIN07 OF CFG-CPY         TO X-PRDLIB(2:).
           MOVE     CFIN08 OF CFG-CPY         TO X-OVRPGM(2:).
           MOVE     CFIN09 OF CFG-CPY         TO X-OVRLIB(2:).
           MOVE     CFIN10 OF CFG-CPY         TO X-EXCPGM(2:).
           MOVE     CFIN11 OF CFG-CPY         TO X-EXCLIB(2:).
     ‚* DSP-EINSTELLUNGEN
           MOVE     CFFK04 OF CFG-CPY         TO X-RSTDSP(2:).
           MOVE     CFFK05 OF CFG-CPY         TO X-DFRWRT(2:).
     ‚* TBL-EINSTELLUNGEN
           MOVE     CFIN13 OF CFG-CPY         TO X-TBLTYPE(2:).
     ‚* C-EINSTELLUNGEN
           MOVE     CFBG01 OF CFG-CPY         TO X-CDEFINE(2:).
     ‚* SONSTIGE EINSTELLUNGEN
           MOVE     CFFK01 OF CFG-CPY         TO X-CPDBG(2:).
           MOVE     CFFK02 OF CFG-CPY         TO X-CPREC(2:).
           MOVE     CFFK03 OF CFG-CPY         TO X-CPINTA(2:).
           MOVE     CFFK05 OF CFG-CPY         TO X-DFRWRT(2:).
           MOVE     CFDAT1 OF CFG-CPY         TO X-CPVCPGM(2:).
           MOVE     CFDAT2 OF CFG-CPY         TO X-CPVCLIB(2:).
           MOVE     CFTXT2 OF CFG-CPY         TO X-CPVCPRM(2:).
           MOVE     CFDAT3 OF CFG-CPY         TO X-CPNCPGM(2:).
           MOVE     CFDAT4 OF CFG-CPY         TO X-CPNCLIB(2:).
           MOVE     CFTXT3 OF CFG-CPY         TO X-CPNCPRM(2:).
           MOVE     CFIN12 OF CFG-CPY         TO X-CPNOWN(2:).

       GET-CFG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-TEILDATEI SECTION.
       CHK-TED-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     X-CPSFIL(2:) TO OBJ     OF CHKOBJ-REC.
           MOVE     X-CPSMBR(2:) TO MBR     OF CHKOBJ-REC.
           MOVE     X-CPSLIB(2:) TO LIB     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TED-90
           END-IF.

       CHK-TED-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER PARAMETER
     ‚*--------------------------------------------------------------
       CHECK-PARAMETER SECTION.
       CHK-PAR-00.

     ‚* USERPROFIL IMMER AUF *USER SETZEN
           MOVE     "*USER"          TO USER-WRK.

     ‚* DEBUG-PARAMETER MAPPEN
           EVALUATE X-CPSTYP(2:) ALSO X-CPDBG(2:1)
     ‚*             COBOL-PROGRAMME
                    WHEN "CBL" ALSO "0"
                         MOVE "*SOURCE"   TO DEBUG-WRK
                    WHEN "CBL" ALSO "1"
                         MOVE "*SRCDBG"   TO DEBUG-WRK
     ‚*             ILE-COBOL-RPGORAMME
                    WHEN "CBLLE" ALSO "0"
                    WHEN "CLLE"  ALSO "0"
                         MOVE "*NONE"     TO DEBUG-WRK
                    WHEN "CBLLE" ALSO "1"
                    WHEN "CLLE"  ALSO "1"
                         MOVE "*ALL"      TO DEBUG-WRK
                    WHEN OTHER
                         MOVE "*ALL"      TO DEBUG-WRK
           END-EVALUATE.

     ‚* WENN DAS BEFEHLSVERARBEITUNGSPROGRAMM LEER IST, DANN IST DER
     ‚* NAME GLEICHZUZIEHEN (CMD)
           IF       X-EXCPGM(2:) = SPACES
                    MOVE X-CPPGM(2:) TO X-EXCPGM(2:)
                    MOVE X-CPLIB(2:) TO X-EXCLIB(2:)
           END-IF.

     ‚* JA/NEIN-EINSTELLUNGEN UMSETZEN
     ‚*
     ‚* ANZEIGE WIEDERHERSTELLEN (DSPF)
           IF       X-RSTDSP(2:1) = "1"
                    MOVE "*YES" TO RSTDSP-WRK
           ELSE
                    MOVE "*NO"  TO RSTDSP-WRK
           END-IF.
     ‚* SCHREIBEN VERZÖGERN (DSPF)
           IF       X-DFRWRT(2:1) = "1"
                    MOVE "*YES" TO DFRWRT-WRK
           ELSE
                    MOVE "*NO"  TO DFRWRT-WRK
           END-IF.
     ‚* MODUL ERSTELLEN?
           IF       X-CPOBJ(2:1) IS NUMERIC
                    MOVE X-CPOBJ(2:1) TO CRTOBJ-WRK
           ELSE
                    MOVE ZEROES       TO CRTOBJ-WRK
           END-IF.
     ‚* SOURCE SCRAMBLEN?
           IF       X-CPSCRM(2:1) IS NUMERIC
                    MOVE X-CPSCRM(2:1) TO SCRAMBLE-WRK
           ELSE
                    MOVE ZEROES        TO SCRAMBLE-WRK
           END-IF.

       CHK-PAR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
