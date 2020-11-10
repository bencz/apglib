     ˆ* BAUSTELLE
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP2007.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFRUF EINER API "RETRIEVE DATA QUEUE DESCRIPTION (QMHQRDQD)  *
     ‚* API                                                           *
     ‚* HTTP://PUBLIB.BOULDER.IBM.COM/ISERIES/V5R2/IC2924/INFO/APIS/  *
     ‚* QMHQRDQD.HTMHTM                                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.09.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSFÜHREN DER API UND ERMITTELN DTAQ-INFOS   *
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

     ‚* QMHQRDQD API STRUKTUR AUS QSYSINC KOPIEREN
           COPY QMHQRDQD    OF QSYSINC-QCBLLESRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP2007".
       01  PGM-TYP                       PIC X(8)  VALUE "API".

       01  ERROR-REC.
           05 BYTESPROV                     PIC 9(8) BINARY.
           05 BYTESAVAIL                    PIC 9(8) BINARY.
           05 EXCEPTID                      PIC X(7).
           05 FILLER1                       PIC X(1).
           05 FILLER2                       PIC X(16).

       01  DTAQ.
        05 DTAQ-NAME                      PIC X(10).
        05 DTAQ-LIB                       PIC X(10) VALUE "*LIBL".

       01  ANZ-API                        PIC 9(5) COMP-3.
       01  ANZ-CUR-ALLE                   PIC 9(5) COMP-3.
       01  ANZ-CUR-IA                     PIC 9(5) COMP-3.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE QMH-QRDQD-RDQD0100,
                      ANZ-API,
                      ANZ-CUR-ALLE,
                      ANZ-CUR-IA.

           CALL     "QMHQRDQD" USING   QMH-QRDQD-RDQD0100
                               CONTENT LENGTH OF QMH-QRDQD-RDQD0100
                                       "RDQD0100  "
                                       DTAQ
           END-CALL.

           MOVE     NUMBER-MESSAGES OF QMH-QRDQD-RDQD0100 TO ANZ-API.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DTAQ DATEN AUS API UND PCURDQE ERMITTELN
     ‚*--------------------------------------------------------------
       ERMITTELN-DTAQ-DATEN SECTION.
       ERM-DTQ-00.

     ‚*€   CALL     "SQLANZDTQ" USING DTAQ-NAME
     ‚*€                              ANZ-CUR-ALLE
     ‚*€                              ANZ-CUR-IA
     ‚*€   END-CALL.

       ERM-DTQ-90.
           EXIT.
      /
