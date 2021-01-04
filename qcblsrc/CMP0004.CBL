       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CMP0004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ÜBERSCHREIBUNGSPROGRAMM FÜR CX                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  OKTOBER 2009                                 *
     ‚*                                                               *
     ‚* FUNKTION      :  ÜBERSCHREIBUNGSPROGRAMM FÜR CX               *
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
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP0004".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

       01  STRING-WRK                    PIC X(5700).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-COMMAND                     PIC X(20).
       01  X-OBJPGM                      PIC X(11).
       01  X-OBJLIB                      PIC X(11).
       01  X-FILE                        PIC X(11).
       01  X-LIB                         PIC X(11).
       01  X-MBR                         PIC X(11).
       01  X-TYP                         PIC X(11).
       01  X-STRING                      PIC X(5700).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION  USING   X-COMMAND
                                   X-OBJPGM
                                   X-OBJLIB
                                   X-FILE
                                   X-LIB
                                   X-MBR
                                   X-TYP
                                   X-STRING.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CMP0004" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           INITIALIZE X-STRING.
           INITIALIZE STRING-WRK.
     ‚* DIE ERSTEN BEIDEN BYTES DES RÜCKGABESTRINGS ENTHALTEN DIE LÄNGE
     ‚* VORBELEGEN MIT X'0000'
           MOVE     X"0000" TO X-STRING(1:2).

     ‚* PROGRAMM ÜBERGEBEN?
           IF       X-LIB(2:)  = SPACES
            OR      X-FILE(2:) = SPACES
            OR      X-MBR(2:)  = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB EINTRAG SCHON VORHANDEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "CP"                  TO CFID   OF CFG-CPY.
           MOVE     X-LIB(2:)             TO CFKEY  OF CFG-CPY.
           MOVE     X-FILE(2:)            TO CFKEY2 OF CFG-CPY.
           MOVE     X-MBR(2:)             TO CFKEY3 OF CFG-CPY.
           MOVE     SPACES                TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    STRING      "PRMOVR(*YES)"        DELIMITED BY SIZE
                    INTO        STRING-WRK
                    GO TO ENDE
           END-IF.

     ‚* NUN EINSTELLUNGEN ZUM WANDELN ERMITTELN
           EVALUATE X-TYP(2:)
                    WHEN "CMD"
                         STRING
                                "RECHT("              DELIMITED BY SIZE
                                CFFK02 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "INTER("              DELIMITED BY SIZE
                                CFFK03 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCPGM("              DELIMITED BY SIZE
                                CFDAT1 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCLIB("              DELIMITED BY SIZE
                                CFDAT2 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCPRM("              DELIMITED BY SIZE
                                CFTXT2 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCPGM("              DELIMITED BY SIZE
                                CFDAT3 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCLIB("              DELIMITED BY SIZE
                                CFDAT4 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCPRM("              DELIMITED BY SIZE
                                CFTXT3 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "OVRPGM("             DELIMITED BY SIZE
                                CFIN08 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "OVRLIB("             DELIMITED BY SIZE
                                CFIN09 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "EXCPGM("             DELIMITED BY SIZE
                                CFIN10 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "EXCLIB("             DELIMITED BY SIZE
                                CFIN11 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "PRDLIB("             DELIMITED BY SIZE
                                CFIN07 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NEWOWN("             DELIMITED BY SIZE
                                CFIN12 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "PRMOVR(*YES)"        DELIMITED BY SIZE
                         INTO   STRING-WRK
                    WHEN "CBL"
                    WHEN "CBLLE"
                    WHEN "CLP"
                    WHEN "CLLE"
                         STRING
                                "RECHT("              DELIMITED BY SIZE
                                CFFK02 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "INTER("              DELIMITED BY SIZE
                                CFFK03 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCPGM("              DELIMITED BY SIZE
                                CFDAT1 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCLIB("              DELIMITED BY SIZE
                                CFDAT2 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCPRM("              DELIMITED BY SIZE
                                CFTXT2 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCPGM("              DELIMITED BY SIZE
                                CFDAT3 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCLIB("              DELIMITED BY SIZE
                                CFDAT4 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCPRM("              DELIMITED BY SIZE
                                CFTXT3 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "TGTRLS("             DELIMITED BY SIZE
                                CFIN03 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "ACTGRP("             DELIMITED BY SIZE
                                CFIN04 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "BND1("               DELIMITED BY SIZE
                                CFIN06 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "DBG("                DELIMITED BY SIZE
                                CFFK01 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "OPTIM("              DELIMITED BY SIZE
                                CFIN05 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "MODUL("              DELIMITED BY SIZE
                                CFFK06 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NEWOWN("             DELIMITED BY SIZE
                                CFIN12 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "SCRMB("              DELIMITED BY SIZE
                                CFFK07 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "PRMOVR(*YES)"        DELIMITED BY SIZE
                         INTO   STRING-WRK
                    WHEN "DSPF"
                         STRING
                                "RECHT("              DELIMITED BY SIZE
                                CFFK02 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "INTER("              DELIMITED BY SIZE
                                CFFK03 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCPGM("              DELIMITED BY SIZE
                                CFDAT1 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCLIB("              DELIMITED BY SIZE
                                CFDAT2 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCPRM("              DELIMITED BY SIZE
                                CFTXT2 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCPGM("              DELIMITED BY SIZE
                                CFDAT3 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCLIB("              DELIMITED BY SIZE
                                CFDAT4 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCPRM("              DELIMITED BY SIZE
                                CFTXT3 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "RSTDSP("             DELIMITED BY SIZE
                                CFFK04 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "DFRWRT("             DELIMITED BY SIZE
                                CFFK05 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NEWOWN("             DELIMITED BY SIZE
                                CFIN12 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "PRMOVR(*YES)"        DELIMITED BY SIZE
                         INTO   STRING-WRK
                    WHEN "C"
                         STRING
                                "RECHT("              DELIMITED BY SIZE
                                CFFK02 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "INTER("              DELIMITED BY SIZE
                                CFFK03 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCPGM("              DELIMITED BY SIZE
                                CFDAT1 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCLIB("              DELIMITED BY SIZE
                                CFDAT2 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "VCPRM("              DELIMITED BY SIZE
                                CFTXT2 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCPGM("              DELIMITED BY SIZE
                                CFDAT3 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCLIB("              DELIMITED BY SIZE
                                CFDAT4 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NCPRM("              DELIMITED BY SIZE
                                CFTXT3 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "TGTRLS("             DELIMITED BY SIZE
                                CFIN03 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "ACTGRP("             DELIMITED BY SIZE
                                CFIN04 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "BND1("               DELIMITED BY SIZE
                                CFIN06 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "DBG("                DELIMITED BY SIZE
                                CFFK01 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "OPTIM("              DELIMITED BY SIZE
                                CFIN05 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "MODUL("              DELIMITED BY SIZE
                                CFFK06 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "NEWOWN("             DELIMITED BY SIZE
                                CFIN12 OF CFG-CPY     DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "DEFINE("             DELIMITED BY SIZE
                              CFBG01 OF CFG-CPY(1:80) DELIMITED BY SIZE
                                ") "                  DELIMITED BY SIZE
                                "PRMOVR(*YES)"        DELIMITED BY SIZE
                         INTO   STRING-WRK
                    WHEN OTHER
                         GO TO ENDE
           END-EVALUATE.

       ENDE.
           IF       STRING-WRK NOT = SPACES
                    MOVE STRING-WRK TO X-STRING(3:)
     ‚*             FESTE LÄNGE FÜR DATENSTRING
                    MOVE   X"1642"  TO X-STRING(1:2)
           END-IF.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
