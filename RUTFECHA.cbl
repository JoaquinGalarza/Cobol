 000010       *VALIDACION FECHA LOGICA*
 000100        IDENTIFICATION DIVISION.
 000110         PROGRAM-ID RUTFECHA.
 000200        ENVIRONMENT DIVISION.
 000210        INPUT-OUTPUT SECTION.
 000211        FILE-CONTROL.
 000300        DATA DIVISION.
 000310        FILE SECTION.
 000391
 000400        WORKING-STORAGE SECTION.
 000401       **************************************************************
 000410        77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.
 000414
 000415       *********************************************
 000416       *              VARIABLES                    *
 000417       *********************************************
 000418        01  WS-VARIABLES.
 000423            05  WS-VAR.
 000424                10  WS-RESULTADO       PIC S9(2)     VALUE ZEROS.
 000425                10  WS-RESTO           PIC S9(2)     VALUE ZEROS.
 000426       *********************************************
 000427       *                FECHA                      *
 000428       *********************************************
 000429        01  WS-FECHA.
 000430            05  WS-FECHA-AA      PIC 9(04)         VALUE ZEROS.
 000432            05  WS-FECHA-MM      PIC 9(02)         VALUE ZEROS.
 000433            05  WS-FECHA-DD      PIC 9(02)         VALUE ZEROS.
 000460
 000470        01  WS-FECHA-ACTUAL.
 000480            05  WS-ACTUAL-AA     PIC 9(04)         VALUE ZEROS.
 000490            05  WS-ACTUAL-MM     PIC 9(02)         VALUE ZEROS.
 000500            05  WS-ACTUAL-DD     PIC 9(02)         VALUE ZEROS.
 000501
 000502        01  WS-FECHA-CALCULO.
 000503            05  WS-CALCULO-AA    PIC 9(04)         VALUE ZEROS.
 000504            05  WS-CALCULO-MM    PIC 9(02)         VALUE ZEROS.
 000505            05  WS-CALCULO-DD    PIC 9(02)         VALUE ZEROS.
 000510       *********************************************
 000520       *                SWITCHES                   *
 000530       *********************************************
 000531        01 WS-SWITCHES.
 000535           05 SW-BISIESTO            PIC X.
 000536              88 SW-BISIESTO-Y             VALUE 'Y'.
 000537              88 SW-BISIESTO-N             VALUE 'N'.
 000538           05 SW-FECHA-VALIDACION    PIC X.
 000539              88 SW-FECHA-OK               VALUE 'Y'.
 000540              88 SW-FECHA-ER               VALUE 'N'.
 000541           05 SW-FECHA-LOGICA        PIC X.
 000542              88 SW-LOGICA-OK              VALUE 'Y'.
 000543              88 SW-LOGICA-ER              VALUE 'N'.
 000544       *********************************************
 000545       *                CONSTANTES                 *
 000546       ********************************************
 000547        01  CT-CONSTANTES.
 000548            05  CT-400                 PIC 9(3)      VALUE 400.
 000549            05  CT-100                 PIC 9(3)      VALUE 100.
 000550            05  CT-4                   PIC 9(1)      VALUE 4.
 000551            05  CT-FECHA-LIMITE.
 000552                10 CT-1920             PIC 9(4) VALUE 1920.
 000553                10 CT-31               PIC 9(2) VALUE 31.
 000554                10 CT-30               PIC 9(2) VALUE 30.
 000555                10 CT-29               PIC 9(2) VALUE 29.
 000556                10 CT-28               PIC 9(2) VALUE 02.
 000557                10 CT-01               PIC 9(2) VALUE 01.
 000558            05  CT-MESES.
 000559                10 CT-ENERO            PIC 9(2) VALUE 01.
 000560                10 CT-FEBRERO          PIC 9(2) VALUE 02.
 000561                10 CT-MARZO            PIC 9(2) VALUE 03.
 000562                10 CT-ABRIL            PIC 9(2) VALUE 04.
 000563                10 CT-MAYO             PIC 9(2) VALUE 05.
 000564                10 CT-JUNIO            PIC 9(2) VALUE 06.
 000565                10 CT-JULIO            PIC 9(2) VALUE 07.
 000566                10 CT-AGOSTO           PIC 9(2) VALUE 08.
 000567                10 CT-SEPTIEMBRE       PIC 9(2) VALUE 09.
 000568                10 CT-OCTUBRE          PIC 9(2) VALUE 10.
 000569                10 CT-NOVIEMBRE        PIC 9(2) VALUE 11.
 000570                10 CT-DICIEMBRE        PIC 9(2) VALUE 12.
 000571
 000572        77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.
 000573
 000574       **************************************************************
 000575        LINKAGE SECTION.
 000576
 000577        01  LK-AREA.
 000578            05 LK-ENTRADA      PIC X(08).
 000579            05 LK-SALIDA       PIC X(01).
 000580               88 LK-FECHA-OK  VALUE 'Y'.
 000581               88 LK-FECHA-ER  VALUE 'N'.
 000582            05 LK-CODE-ERROR   PIC 9(04).
 000583            05 FILLER          PIC X(22).
 000584
 000585       ***************************************************************.
 000586        PROCEDURE DIVISION USING LK-AREA.
 000588
 000589       **************************************
 000590       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000591       **************************************
 000600        MAIN-PROGRAM.
 000601            SET SW-LOGICA-OK TO TRUE
 000602
 000603            PERFORM 1000-I-INICIO
 000604               THRU 1000-F-INICIO
 000605
 000606            IF SW-LOGICA-OK
 000607
 000608              PERFORM 2000-I-PROCESO
 000609                 THRU 2000-F-PROCESO
 000610
 000611            ELSE
 000612
 000613              SET SW-FECHA-ER TO TRUE
 000614
 000615              DISPLAY 'LA FECHA INGRESADA NO ES LOGICA'
 000616
 000617            END-IF
 000618
 000619            PERFORM 9999-I-FINAL
 000620               THRU 9999-F-FINAL
 000621            .
 000622        F-MAIN-PROGRAM. GOBACK.
 000623
 000624       **************************************
 000625       *  CUERPO INICIO INDICES             *
 000626       **************************************
 000627        1000-I-INICIO.
 000630            ACCEPT WS-FECHA-ACTUAL FROM DATE YYYYMMDD
 000631
 000632            MOVE   LK-ENTRADA      TO        WS-FECHA-CALCULO
 000633
 000634            IF  (WS-FECHA-CALCULO IS LESS OR EQUAL TO WS-FECHA-ACTUAL)
 000635            AND (WS-FECHA-CALCULO IS GREATER OR EQUAL TO CT-1920)
 000636
 000637               SET SW-LOGICA-OK TO TRUE
 000638
 000639            ELSE
 000640
 000641               SET SW-LOGICA-ER TO TRUE
 000642
 000643            END-IF
 000650            .
 000860        1000-F-INICIO. EXIT.
 000882       **************************************
 000890       *  CUERPO PRINCIPAL DE PROCESO       *
 000897       **************************************
 000900        2000-I-PROCESO.
 001013            SET SW-FECHA-OK TO TRUE
 001014
 001015            MOVE LK-ENTRADA TO WS-FECHA
 001016
 001017            DIVIDE WS-FECHA-AA BY CT-4 GIVING  WS-RESULTADO REMAINDER
 001018                                                            WS-RESTO
 001019
 001020            IF WS-RESTO EQUAL ZEROS
 001021              DIVIDE WS-FECHA-AA BY CT-100 GIVING WS-RESULTADO REMAINDER
 001022                                                               WS-RESTO
 001023
 001024              IF WS-RESTO EQUAL ZEROS
 001025                DIVIDE WS-FECHA-AA BY CT-400 GIVING WS-RESULTADO
 001026                                                               REMAINDER
 001027                                                               WS-RESTO
 001028
 001029                IF WS-RESTO EQUAL ZEROS
 001030                  SET SW-BISIESTO-Y TO TRUE
 001031
 001032                ELSE
 001033                  SET SW-BISIESTO-N TO TRUE
 001034
 001035                END-IF
 001036
 001037              ELSE
 001038                SET SW-BISIESTO-Y   TO TRUE
 001039
 001040              END-IF
 001041
 001042            ELSE
 001043              SET SW-BISIESTO-N     TO TRUE
 001044
 001045            END-IF
 001046
 001047            IF (WS-FECHA-AA IS GREATER OR EQUAL TO CT-1920)     AND
 001048               (WS-FECHA-AA IS LESS    OR EQUAL TO WS-ACTUAL-AA)
 001049
 001050              EVALUATE WS-FECHA-MM
 001051
 001052              WHEN CT-FEBRERO
 001053                IF SW-BISIESTO-Y
 001054                  DISPLAY 'ES BISIESTO'
 001055                  IF (WS-FECHA-DD IS GREATER OR EQUAL TO CT-01) AND
 001056                     (WS-FECHA-DD IS LESS    OR EQUAL TO CT-29)
 001057                    CONTINUE
 001058
 001059                  ELSE
 001060                    SET SW-FECHA-ER TO TRUE
 001061                    DISPLAY 'DIA ERRONEO'
 001062
 001063                  END-IF
 001064
 001065                ELSE
 001066                  DISPLAY 'NO ES BISIESTO'
 001067                    IF (WS-FECHA-DD IS GREATER OR EQUAL TO CT-01) AND
 001068                       (WS-FECHA-DD IS LESS OR EQUAL TO CT-28)
 001069                      CONTINUE
 001070                    ELSE
 001071                      SET SW-FECHA-ER TO TRUE
 001072                      DISPLAY 'DIA ERRONEO'
 001073
 001074                    END-IF
 001075
 001076                END-IF
 001077
 001078              WHEN CT-MARZO
 001079              WHEN CT-MAYO
 001080              WHEN CT-JULIO
 001081              WHEN CT-AGOSTO
 001082              WHEN CT-OCTUBRE
 001083              WHEN CT-DICIEMBRE
 001084                IF (WS-FECHA-DD IS GREATER OR EQUAL TO CT-01) AND
 001085                   (WS-FECHA-DD IS LESS OR EQUAL TO CT-31)
 001086                  CONTINUE
 001087
 001088                ELSE
 001089                  SET SW-FECHA-ER TO TRUE
 001090                  DISPLAY 'DIA ERRONEO'
 001091
 001092                END-IF
 001093
 001094              WHEN CT-ENERO
 001095              WHEN CT-ABRIL
 001096              WHEN CT-JUNIO
 001097              WHEN CT-SEPTIEMBRE
 001098              WHEN CT-NOVIEMBRE
 001099                IF (WS-FECHA-DD IS GREATER OR EQUAL TO CT-01) AND
 001100                   (WS-FECHA-DD IS LESS OR EQUAL TO CT-30)
 001101                  CONTINUE
 001102
 001103                ELSE
 001104                  SET SW-FECHA-ER TO TRUE
 001105                  DISPLAY 'DIA ERRONEO'
 001106
 001107                END-IF
 001108
 001109              WHEN OTHER
 001110                SET SW-FECHA-ER TO TRUE
 001111                DISPLAY 'MES ERRONEO'
 001112
 001113              END-EVALUATE
 001114
 001115
 001116            ELSE
 001117              SET SW-FECHA-ER TO TRUE
 001118              DISPLAY 'ANIO ERRONEO'
 001119
 001120            END-IF
 001125            .
 001130        2000-F-PROCESO. EXIT.
 001200
 001650       **************************************
 001670       *  CUERPO FINAL MUESTRA RESULTADO    *
 001690       **************************************
 001700        9999-I-FINAL.
 001740            MOVE SW-FECHA-VALIDACION TO LK-SALIDA.
 001802            DISPLAY SW-FECHA-VALIDACION
 001810            .
 001900        9999-F-FINAL.  EXIT.
 001910
 002000       *