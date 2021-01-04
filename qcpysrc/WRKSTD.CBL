     ‚*---------------------------------------------------------------*
     ‚* STANDARDFELDER DIE IN DEN MEISTEN PROGRAMMEN GENUTZT WERDEN   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  14.12.2007                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BEREITSTELLUNG VON STANDARDVARIABLEN         *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*

     ‚* K O N S T A N T E N
       01  CONST-LIB                     PIC X(10)   VALUE "APG".

     ‚* JOBDATEN
       01  JOBINFO.
        05 T-TERM                        PIC X(10)   VALUE SPACES.
        05 T-USER                        PIC X(10)   VALUE SPACES.
        05 T-CURLIB                      PIC X(10)   VALUE SPACES.
        05 T-LIBL                        PIC X(2750) VALUE SPACES.
        05 T-SPRACHE                     PIC X(3)    VALUE SPACES.

     ‚* KENNZEICHEN/SCHALTER
       01  COPY-AKTION                   PIC X(10)   VALUE SPACES.
       77  FILE-STATUS                   PIC X(2).
       01  ERRFLD-WRK                    PIC X(10).
       01  BETRIEBSSYSTEM                PIC 9(1)    VALUE ZEROES.

       01  RET-CODE                      PIC X(1).
     ‚* DER RETURN-CODE WIRD SCHON VOM SYSTEM DEFINIERT
      *01  RETURN-CODE GLOBAL PICTURE S9999 USAGE BINARY VALUE 0.

     ‚* AKTUELLES PGM/LIB
       01  ACTPGM-WRK                    PIC X(10).
       01  ACTLIB-WRK                    PIC X(10).
       01  ACT-DEPTH                     PIC 9(4).

     ‚* LFDN-NUMMER
       01  LFDN-NAME                     PIC X(10)   VALUE SPACES.
       01  LFDN-ID                       PIC X(20)   VALUE SPACES.
       01  LFDN-WRK                      PIC 9(9)    VALUE ZEROES.

     ‚* UHRZEIT- UND DATUMSFELDER
       01  DATUM-CODE                    PIC X(2).
       01  DATUM-FORMAT                  PIC X(8).
       01  DATUM-REKONV                  PIC X(6).
       01  DATE-8                        PIC 9(8).
       01  TIME-6                        PIC 9(6).
       01  TIME-8                        PIC 9(8).
       01  DIFF-DAYS                     PIC S9(9).
       01  DIFF-SECS                     PIC S9(9).

     ‚* FELDER FÜR DATUMSAUFBEREITUNG
       01  DATUM-ALPHA-10                PIC X(10).
       01  DATUM-ALPHA-8                 PIC X(8).
       01  DATE-DB                       PIC 9(8).
       01  DATE-DB-6                     PIC 9(6).
       01  DATUM-CONVERT                 PIC X(10).
       01  DATUM-RETCODE                 PIC X(1).

     ‚* FELDER FÜR UHRZEITAUFBEREITUNG
       01  TIME-ALPHA-8                  PIC X(8).
       01  TIME-DB                       PIC 9(6).
       01  TIME-CONVERT                  PIC X(10).
       01  TIME-RETCODE                  PIC X(1).

     ‚* FELDER FÜR DIE OUTQ-ERMITTLUNG
       01  CPY-OUTQ                      PIC X(10).

     ‚* VARIABLEN FÜR DIE NEGATIVE ZAHLENERKENNUNG (NEGNUM)
       01  NEGATIVE-FLAG                 PIC 9(1).
       01  NEGATIVE-WRK                  PIC X(1).

       01  UHRZEIT.
        05 STUNDE                        PIC 9(2).
        05 MINUTE                        PIC 9(2).
        05 SEKUNDE                       PIC 9(2).
       01  UHRZEIT-RED REDEFINES UHRZEIT PIC 9(6).

       01  JAHRHUNDERT                   PIC 9(2).
       01  DATUM.
        05 JAHR                          PIC 9(2).
        05 MONAT                         PIC 9(2).
        05 TAG                           PIC 9(2).
       01  DATUM-RED REDEFINES DATUM     PIC 9(6).

       01  T-PRTF                        PIC X(10).
       01  T-JBNR                        PIC S9(6).
       01  FIRST-CALL                    PIC X(1)    VALUE "0".
       01  FIRST-INIT                    PIC X(1)    VALUE "0".
       01  FIRST-EXIT                    PIC X(1)    VALUE "0".
       01  DATEN                         PIC 9(1).
       01  SPRACHE-WRK                   PIC X(3).

     ‚* HILFSVARIABLEN
       01  SYS-COMPIL                    PIC X(21).
       01  ANZREC-WRK                    PIC 9(4).
       01  RECORD-WRK                    PIC X(8192) VALUE SPACES.
       01  RECORD-LEN                    PIC 9(4)    VALUE ZEROES.
       01  SELEKTION-WRK                 PIC X(50)   VALUE SPACES.

     ‚* EXTERNALS !!!
       01  EXTERNAL-PGM                  PIC X(10)   EXTERNAL.
       01  EXTERNAL-DEPTH                PIC 9(9)    EXTERNAL.
     ‚*€
