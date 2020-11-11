             CMD        PROMPT('PETIK drucken')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('PRINT') RSTD(*NO) MIN(1)

             PARM       KWD(LIB) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(1) CHOICE('Name') PROMPT('Bibliothek') +
                         KEYPARM(*YES)
             PARM       KWD(FILE) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(1) CHOICE('Name') PROMPT('Datei')   +
                         KEYPARM(*YES)
             PARM       KWD(MBR) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name, *NONE') DFT(*NONE) +
                         PROMPT('Teildatei') KEYPARM(*YES)
             PARM       KWD(TYP) TYPE(*CHAR) LEN(1) RSTD(*NO)  +
                         MIN(0) CHOICE('(1=PETIK/2=Layout)') +
                         PROMPT('Typ') DFT(1)
             PARM       KWD(OUTQ) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') DFT(PRT01)        +
                         PROMPT('Ausgabewarteschlange')
