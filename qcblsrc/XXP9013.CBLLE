       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9013.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SCHREIBEN PCMPSCM                                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  17.06.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SCHREIBEN EINES DATENSATZES                  *
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
           SELECT PCMPSCM-DP
                  ASSIGN       TO  DATABASE-PCMPSCM
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEXTE
       FD  PCMPSCM-DP
           LABEL RECORDS ARE STANDARD.
       01  PCMPSCM-P.
           COPY DDS-ALL-FORMATS OF PCMPSCM.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9013".
       01  VAL-WRK                       LIKE SMVAL  OF PCMPSCM-P.
       01  VAL-LEN                       LIKE SMVLEN OF PCMPSCM-P.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SMKEY                       LIKE SMKEY  OF PCMPSCM-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SMKEY.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9013" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCMPSCM-DP.

           INITIALIZE VAL-WRK, VAL-LEN.
     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-SMKEY  = SPACES
                    GO TO ENDE
           END-IF.

     ‚* WERT ERMITTELN
           PERFORM  GET-DATA.

     ‚* DATEN SCHREIBEN
           PERFORM  WRITE-DATA.

       ENDE.
           CLOSE                 PCMPSCM-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER DATEN
     ‚*--------------------------------------------------------------
       GET-DATA SECTION.
       GET-DTA-00.

           INITIALIZE VAL-WRK.
           EVALUATE X-SMKEY
                    WHEN "SCRAMBLE_MAIN"
                         MOVE 1000                   TO VAL-LEN
                         MOVE "AbCDEFghIjKLMNoPqRst" TO VAL-WRK(001:20)
                         MOVE "uVwxyZAbhGaaKmNloIqQ" TO VAL-WRK(021:20)
                         MOVE "wErjHgaKiOplHsAtZioJ" TO VAL-WRK(041:20)
                         MOVE "kJuTREwAvJhhAgFaAAzT" TO VAL-WRK(061:20)
                         MOVE "tResIoAbCdzUiJsaOpWt" TO VAL-WRK(081:20)
                         MOVE "wQuIztOpAdxYyloCxTrw" TO VAL-WRK(101:20)
                         MOVE "YxXHEuAewQkIoaLkmNaO" TO VAL-WRK(121:20)
                         MOVE "kIuAsaQsAlKiuzTAsbCx" TO VAL-WRK(141:20)
                         MOVE "wqeiuhasdasdlksadaaq" TO VAL-WRK(161:20)
                         MOVE "LkjlHJjdtrztAasdljAd" TO VAL-WRK(181:20)
                         MOVE "IuoiudadjhkASDmkllAF" TO VAL-WRK(201:20)
                         MOVE "LJioqdUziuVvsaDiuziF" TO VAL-WRK(221:20)
                         MOVE "LKjoizuZUiZTzuhjFjfA" TO VAL-WRK(241:20)
                         MOVE "POIUiuZAbCDefGhijklM" TO VAL-WRK(261:20)
                         MOVE "nopQRSTuVWXyZsadaASD" TO VAL-WRK(281:20)
                         MOVE "LKJADioUCbvSdjlkVNaQ" TO VAL-WRK(301:20)
                         MOVE "LkjLZUIzuiVFhgDMAGaR" TO VAL-WRK(321:20)
                         MOVE "OIZoidfgefdgddsasGak" TO VAL-WRK(341:20)
                         MOVE "POIpouRsadajklutHlkj" TO VAL-WRK(361:20)
                         MOVE "IUTTADKJkjhAhkjttrmr" TO VAL-WRK(381:20)
                         MOVE "LJlYxcycxQRWRErrtzdd" TO VAL-WRK(401:20)
                         MOVE "IUziTrzEtretgjhfcaSD" TO VAL-WRK(421:20)
                         MOVE "POIUiuZutrzZTUjJHGat" TO VAL-WRK(441:20)
                         MOVE "YxXHEuAewQkIoaLkmNaO" TO VAL-WRK(461:20)
                         MOVE "kIuAsaQsAlKiuzTAsbCx" TO VAL-WRK(481:20)
                         MOVE "wqeiuhasdasdlksadaaq" TO VAL-WRK(501:20)
                         MOVE "LkjlHJjdtrztAasdljAd" TO VAL-WRK(521:20)
                         MOVE "POIUiuZutrzZTUjJHGat" TO VAL-WRK(541:20)
                         MOVE "uVwxyZAbhGaaKmNloIqQ" TO VAL-WRK(561:20)
                         MOVE "wErjHgaKiOplHsAtZioJ" TO VAL-WRK(581:20)
                         MOVE "kJuTREwAvJhhAgFaAAzT" TO VAL-WRK(601:20)
                         MOVE "tResIoAbCdzUiJsaOpWt" TO VAL-WRK(621:20)
                         MOVE "wQuIztOpAdxYyloCxTrw" TO VAL-WRK(641:20)
                         MOVE "YxXHEuAewQkIoaLkmNaO" TO VAL-WRK(661:20)
                         MOVE "kIuAsaQsAlKiuzTAsbCx" TO VAL-WRK(681:20)
                         MOVE "KJHKJASDoizuiZIUZTAx" TO VAL-WRK(701:20)
                         MOVE "LKLKVNBymNMdOIoiOQQr" TO VAL-WRK(721:20)
                         MOVE "wIUZTIQWEdbdKiOplHsA" TO VAL-WRK(741:20)
                         MOVE "wKLJIOUzuiZwAvJhhAgF" TO VAL-WRK(761:20)
                         MOVE "wqQEQhasaKiAbCdzUiJs" TO VAL-WRK(781:20)
                         MOVE "XaKiOplHwAvOpAdxYylo" TO VAL-WRK(801:20)
                         MOVE "XyAvJhhAAbCAewQkIoaL" TO VAL-WRK(821:20)
                         MOVE "wAbCdzUiOpAQsAlKiuzT" TO VAL-WRK(841:20)
                         MOVE "wOpAdxYyAewQkIoaLayy" TO VAL-WRK(861:20)
                         MOVE "wAewQkIoQsAlKiuzTaaq" TO VAL-WRK(881:20)
                         MOVE "wQsAlKiuzTsdlksTTuuq" TO VAL-WRK(901:20)
                         MOVE "wUIQWEZiukjHKSADxasd" TO VAL-WRK(921:20)
                         MOVE "OIuwzeiuASDioUASDASD" TO VAL-WRK(941:20)
                         MOVE "wOIZAOIDSASDdasoiuoq" TO VAL-WRK(961:20)
                         MOVE "XXoipUsaAPOPLDJLKJLA" TO VAL-WRK(981:20)
           END-EVALUATE.

       GET-DTA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINES DATENSATZES
     ‚*--------------------------------------------------------------
       WRITE-DATA SECTION.
       WRI-DTA-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

           INITIALIZE PCMPSCMF OF PCMPSCM-P.
           MOVE     X-SMKEY         TO SMKEY  OF PCMPSCM-P.
           READ     PCMPSCM-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCMPSCMF OF PCMPSCM-P
                    MOVE   X-SMKEY  TO SMKEY  OF PCMPSCM-P
                    MOVE   VAL-WRK  TO SMVAL  OF PCMPSCM-P
                    MOVE   VAL-LEN  TO SMVLEN OF PCMPSCM-P
     ‚*             USER/TERMINAL
                    MOVE   PGM-WRK  TO SMAPGM OF PCMPSCM-P
                    MOVE   T-USER   TO SMAUSR OF PCMPSCM-P
                    MOVE   T-TERM   TO SMABS  OF PCMPSCM-P
     ‚*             DATUM/UHRZEIT
                    MOVE   DATE-8   TO SMADTA OF PCMPSCM-P
                    MOVE   TIME-6   TO SMATIA OF PCMPSCM-P
                    WRITE  PCMPSCM-P
           ELSE
                    MOVE   VAL-WRK  TO SMVAL  OF PCMPSCM-P
                    MOVE   VAL-LEN  TO SMVLEN OF PCMPSCM-P
     ‚*             USER/TERMINAL
                    MOVE   PGM-WRK  TO SMMPGM OF PCMPSCM-P
                    MOVE   T-USER   TO SMMSER OF PCMPSCM-P
                    MOVE   T-TERM   TO SMMBS  OF PCMPSCM-P
     ‚*             DATUM/UHRZEIT
                    MOVE   DATE-8   TO SMMDTA OF PCMPSCM-P
                    MOVE   TIME-6   TO SMMTIA OF PCMPSCM-P
                    REWRITE  PCMPSCM-P
           END-IF.

       WRI-DTA-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
