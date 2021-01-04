       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0020.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PROJEKTNUMMER AUFBEREITEN                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  01.09.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUFBEREITEN DER PROJEKTNUMMER                *
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

       01  PGM-WRK                       PIC X(10) VALUE "HLP0020".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PRN1                        PIC 9(2).
       01  X-PRN2                        PIC 9(4).
       01  X-PRN3                        PIC 9(2).
       01  X-RETURN                      PIC X(7).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PRN1
                                X-PRN2
                                X-PRN3
                                X-RETURN.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE X-RETURN.
     ‚* JETZT PROJEKTNUMMER AUFBEREITEN
           MOVE     X-PRN2                TO X-RETURN(1:4).
           MOVE     "."                   TO X-RETURN(5:1).
           MOVE     X-PRN3                TO X-RETURN(6:2).
           INSPECT  X-RETURN REPLACING LEADING ZEROES BY SPACES.

       ENDE.
           GOBACK.
      /
