 000001        IDENTIFICATION DIVISION.
 000002         PROGRAM-ID PGMAPA33
 000003        ENVIRONMENT DIVISION.
 000004        INPUT-OUTPUT SECTION.
 000005        FILE-CONTROL.
 000006              SELECT FILE1   ASSIGN DDFILE1
 000007                     FILE STATUS IS WS-CON-CODE.
 000008
 000009              SELECT FILE2   ASSIGN DDFILE2
 000010                     FILE STATUS IS WS-CLA-CODE.
 000011
 000012              SELECT SALIDA  ASSIGN DDSALI
 000013                     FILE STATUS IS WS-SA-CODE.
 000014
 000015        DATA DIVISION.
 000016        FILE SECTION.
 000017        FD FILE1
 000018             BLOCK CONTAINS 0 RECORDS
 000019             RECORDING MODE IS F.
 000020
 000021        01 REG-CON        PIC X(93).
 000022
 000023        FD FILE2
 000024             BLOCK CONTAINS 0 RECORDS
 000025             RECORDING MODE IS F.
 000026
 000027        01 REG-CLA        PIC X(93).
 000028
 000029        FD SALIDA
 000030             BLOCK CONTAINS 0 RECORDS
 000031             RECORDING MODE IS F.
 000032
 000033        01 REG-SA         PIC X(93).
 000034
 000035       **************************************
 000036        WORKING-STORAGE SECTION.
 000037       **************************************
 000038        77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.
 000039        77  FILLER        PIC X(26) VALUE '* CODIGOS RETORNO FILES  *'.
 000040       **************************************
 000041       *        CONTROL DE FINAL           *
 000042       **************************************
 000043        77  WS-CON-CODE   PIC XX    VALUE ZEROS.
 000044        77  WS-CLA-CODE   PIC XX    VALUE ZEROS.
 000045        77  WS-SA-CODE    PIC XX    VALUE ZEROS.
 000046        01 WS-STATUS-FIN     PIC X.
 000047           88 WS-FIN-LECTURA         VALUE 'Y'.
 000048           88 WS-NO-FIN-LECTURA      VALUE 'N'.
 000049       **************************************
 000050       *         LAYOUT CONSULTA            *
 000051       **************************************
 000052        01  WS-CON-REG.
 000053            03 WS-CON-CLAVE.
 000054              05 WS-DOC-TIPO          PIC XX          VALUE SPACES.
 000055              05 WS-NRO-DOC           PIC X(11)       VALUE ZEROS.
 000056              05 WS-NOMBRE-APELLIDO PIC X(30)       VALUE SPACES.
 000057            03 WS-ESTADO-CIVIL      PIC X(10)       VALUE SPACES.
 000058            03 WS-SEXO              PIC X           VALUE SPACES.
 000059            03 FILLER                 PIC X(39)       VALUE SPACES.
 000060       ************************************
 000061       *       LAYOUT CLASE               *
 000062       ************************************
 000063        01  WS-CLA-REG.
 000064            03 WS-CLA-CLAVE.
 000065              05 WS-DOC-TIPO          PIC XX          VALUE SPACES.
 000066              05 WS-NRO-DOC           PIC X(11)       VALUE ZEROS.
 000067              05 WS-NOMBRE-APELLIDO PIC X(30)       VALUE SPACES.
 000068            03 WS-ESTADO-CIVIL      PIC X(10)       VALUE SPACES.
 000069            03 WS-SEXO              PIC X           VALUE SPACES.
 000070            03 FILLER                 PIC X(39)       VALUE SPACES.
 000071       ************************************
 000072       *          LAYOUT SALIDA           *
 000073       ************************************
 000074        01  WS-SA-REG.
 000075            03 WS-DOC-TIPO          PIC XX          VALUE SPACES.
 000076            03 WS-NRO-DOC           PIC X(11)       VALUE ZEROS.
 000077            03 WS-NOMBRE-APELLIDO PIC X(30)       VALUE SPACES.
 000078            03 WS-ESTADO-CIVIL      PIC X(10)       VALUE SPACES.
 000079            03 WS-SEXO              PIC X           VALUE SPACES.
 000080            03 FILLER                 PIC X(39)       VALUE SPACES.
 000081        77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.
 000082       ************************************
 000083       *          CONTADORES              *
 000084       ************************************
 000085        77  WS-CANT-REG               PIC 9(10)       VALUE ZEROS.
 000086        77  WS-CANT-GRAB              PIC 9(10)       VALUE ZEROS.
 000087       ***************************************************************.
 000088        PROCEDURE DIVISION.
 000089       **************************************
 000090       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000091       *                                    *
 000092       **************************************
 000093        MAIN-PROGRAM.
 000094
 000095            PERFORM 1000-INICIO  THRU   F-1000-INICIO.
 000096
 000097            PERFORM 2000-PROCESO  THRU  F-2000-PROCESO
 000098                    UNTIL WS-FIN-LECTURA.
 000099
 000100            PERFORM 9999-FINAL    THRU  F-9999-FINAL.
 000101
 000102        F-MAIN-PROGRAM. GOBACK.
 000103
 000104       **************************************
 000105       *                                    *
 000106       *  CUERPO INICIO APERTURA ARCHIVOS   *
 000107       *                                    *
 000108       **************************************
 000109        1000-INICIO.
 000110            SET WS-NO-FIN-LECTURA TO TRUE.
 000111            OPEN INPUT  FILE1.
 000112            IF WS-CON-CODE IS NOT EQUAL '00'
 000113               DISPLAY '* ERROR EN OPEN CONSULTA = ' WS-CON-CODE
 000114               MOVE 9999 TO RETURN-CODE
 000115               SET  WS-FIN-LECTURA TO TRUE
 000116            END-IF.
 000117
 000118            OPEN INPUT  FILE2.
 000119            IF WS-CLA-CODE IS NOT EQUAL '00'
 000120               DISPLAY '* ERROR EN OPEN CLASE  = ' WS-CLA-CODE
 000121               MOVE 9999 TO RETURN-CODE
 000122               SET  WS-FIN-LECTURA TO TRUE
 000123            END-IF.
 000124
 000125            OPEN OUTPUT SALIDA.
 000126            IF WS-SA-CODE IS NOT EQUAL '00'
 000127               DISPLAY '* ERROR EN OPEN SALIDA  = ' WS-SA-CODE
 000128               MOVE 9999 TO RETURN-CODE
 000129               SET  WS-FIN-LECTURA TO TRUE
 000130            END-IF.
 000131
 000132             PERFORM 3000-LEER-CON   THRU F-3000-LEER-CON.
 000133             PERFORM 4000-LEER-CLA   THRU F-4000-LEER-CLA.
 000134
 000135
 000136        F-1000-INICIO.   EXIT.
 000137
 000138       **************************************
 000139       *  CUERPO PRINCIPAL DE PROCESOS      *
 000140       **************************************
 000141        2000-PROCESO.
 000142             PERFORM 2500-ORGANIZAR THRU F-2500-ORGANIZAR
 000143             IF WS-CON-CLAVE = HIGH-VALUE AND WS-CLA-CLAVE = HIGH-VALUE
 000144                SET WS-FIN-LECTURA TO TRUE
 000145             END-IF.
 000146        F-2000-PROCESO.
 000147       **************************************
 000148       *    ORGANIZAR ARCHIVOS              *
 000149       **************************************
 000150        2500-ORGANIZAR.
 000151             IF WS-CON-CLAVE = WS-CLA-CLAVE
 000152                MOVE WS-CON-REG TO WS-SA-REG
 000153                PERFORM 5000-SALIDA   THRU F-5000-SALIDA
 000154                PERFORM 3000-LEER-CON THRU F-3000-LEER-CON
 000155                PERFORM 4000-LEER-CLA THRU F-4000-LEER-CLA
 000156             ELSE
 000157               IF WS-CON-CLAVE > WS-CLA-CLAVE
 000158                MOVE WS-CLA-REG TO WS-SA-REG
 000159                PERFORM 5000-SALIDA   THRU F-5000-SALIDA
 000160                PERFORM 4000-LEER-CLA THRU F-4000-LEER-CLA
 000161               ELSE
 000162                MOVE WS-CON-REG TO WS-SA-REG
 000163                 PERFORM 5000-SALIDA  THRU F-5000-SALIDA
 000164
 000165                 PERFORM 3000-LEER-CON THRU F-3000-LEER-CON
 000166             END-IF.
 000167        F-2500-ORGANIZAR.
 000168       **************************************
 000169       * LECTURA CONSULTA                   *
 000170       **************************************
 000171        3000-LEER-CON.
 000172
 000173            READ FILE1     INTO WS-CON-REG.
 000174
 000175            EVALUATE WS-CON-CODE
 000176            WHEN '00'
 000177                ADD 1 TO WS-CANT-REG
 000178            WHEN '10'
 000179                MOVE HIGH-VALUE TO WS-CON-CLAVE
 000180            WHEN OTHER
 000181                DISPLAY '* ERROR EN LECTURA CONSULTA= ' WS-CON-CODE
 000182                MOVE 9999 TO RETURN-CODE
 000183                SET WS-FIN-LECTURA TO TRUE
 000184
 000185            END-EVALUATE.
 000186        F-3000-LEER-CON. EXIT.
 000187
 000188       **************************************
 000189       * LECTURA CLASE                      *
 000190       **************************************
 000191        4000-LEER-CLA.
 000192
 000193            READ FILE2     INTO WS-CLA-REG.
 000194
 000195            EVALUATE WS-CLA-CODE
 000196            WHEN '00'
 000197                ADD 1 TO WS-CANT-REG
 000198            WHEN '10'
 000199                MOVE HIGH-VALUE TO WS-CLA-CLAVE
 000200            WHEN OTHER
 000201               DISPLAY '* ERROR EN LECTURA CLASE= ' WS-CLA-CODE
 000202               MOVE 9999 TO RETURN-CODE
 000203                 SET WS-FIN-LECTURA TO TRUE
 000204
 000205            END-EVALUATE.
 000206        F-4000-LEER-CLA. EXIT.
 000207       **************************************
 000208       *         GRABAR SALIDA              *
 000209       **************************************
 000210        5000-SALIDA.
 000211            WRITE REG-SA FROM WS-SA-REG.
 000212            ADD 1 TO WS-CANT-GRAB.
 000213            IF WS-SA-CODE IS NOT EQUAL '00'
 000214               DISPLAY '* ERROR EN SALIDA = ' WS-SA-CODE
 000215               MOVE 9999 TO RETURN-CODE
 000216               SET  WS-FIN-LECTURA TO TRUE
 000217            END-IF.
 000218            DISPLAY WS-SA-REG.
 000219        F-5000-SALIDA.
 000220       **************************************
 000221       *  CUERPO FINAL CIERRE DE FILES      *
 000222       **************************************
 000223        9999-FINAL.
 000224
 000225            CLOSE FILE1.
 000226               IF WS-CON-CODE IS NOT EQUAL '00'
 000227                 DISPLAY '* ERROR EN CLOSE CONSULTA = ' WS-CON-CODE
 000228                 MOVE 9999 TO RETURN-CODE
 000229                 SET WS-FIN-LECTURA TO TRUE
 000230               END-IF.
 000231
 000232            CLOSE  FILE2.
 000233               IF WS-CLA-CODE IS NOT EQUAL '00'
 000234                 DISPLAY '* ERROR EN CLOSE CLASE= ' WS-CLA-CODE
 000235                 MOVE 9999 TO RETURN-CODE
 000236                 SET WS-FIN-LECTURA TO TRUE
 000237               END-IF.
 000238
 000239            CLOSE SALIDA.
 000240               IF WS-SA-CODE IS NOT EQUAL '00'
 000241                 DISPLAY '* ERROR EN CLOSE SALIDA  = ' WS-SA-CODE
 000242                 MOVE 9999 TO RETURN-CODE
 000243                 SET WS-FIN-LECTURA TO TRUE
 000244               END-IF.
 000245            DISPLAY 'LA CANTIDAD DE REGISTROS LEIDOS ES DE ' WS-CANT-REG.
 000246            DISPLAY 'LA CANTIDAD DE REGISTROS GRABADOS ES ' WS-CANT-GRAB.
 000247        F-9999-FINAL.
 000248            EXIT.