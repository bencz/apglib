             CMD        PROMPT('Source verschieben')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('MOVMBR') RSTD(*NO) MIN(1)
             PARM       KWD(FRMLIB) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         MIN(1) CHOICE('Name')                       +
                         PROMPT('Quellbibliothek') KEYPARM(*YES)
             PARM       KWD(FRMFILE) TYPE(*CHAR) LEN(10) RSTD(*NO)   +
                         MIN(1) CHOICE('Name') PROMPT('  Datei')     +
                         KEYPARM(*YES)
             PARM       KWD(FRMMBR) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         MIN(1) CHOICE('Name') PROMPT('  Teildatei') +
                         KEYPARM(*YES)
             PARM       KWD(TOLIB) TYPE(*CHAR) LEN(10) RSTD(*NO)     +
                         MIN(1) CHOICE('Name')                       +
                         PROMPT('Zielbibliothek') KEYPARM(*YES)
             PARM       KWD(TOFILE) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         MIN(1) CHOICE('Name') PROMPT('  Datei')     +
                         KEYPARM(*YES)
             PARM       KWD(TOMBR) TYPE(*CHAR) LEN(10) RSTD(*NO)     +
                         MIN(1) CHOICE('Name') PROMPT('  Teildatei') +
                         KEYPARM(*YES)
