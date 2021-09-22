      ******************************************************************
      * ESQUELETO CORTE DE CONTROL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORTCTRL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO "ENTPAIS.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA.

           SELECT SALIDA ASSIGN TO "salidapais.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA.
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA.
           01 REG-ENTRADA.
               05 REG-PAIS             PIC X(3).
               05 REG-CIUDAD           PIC X(3).
               05 REG-SEXO             PIC X(1).
               05 REG-HABITANTES       PIC 9(9).

       FD  SALIDA.
           01 REG-SALIDA PIC X(100).


       WORKING-STORAGE SECTION.

       01  ESTRUCTURA-SALIDA.
           05 TEXTO                    PIC X(40).
           05 TOTALES                  PIC 9(9).

       01  VARIABLES.
           05 FS-ENTRADA               PIC XX.
               88 FS-ENTRADA-OK        VALUE "00".
               88 FS-ENTRADA-FIN       VALUE "10".

           05 FS-SALIDA                PIC XX.
               88 FS-SALIDA-OK         VALUE "00".
               88 FS-SALIDA-FIN        VALUE "10".

           05 WS-PAIS-ANT                 PIC X(3).
           05 WS-CIUDAD-ANT               PIC X(3).
           05 WS-SEXO-ANT                 PIC X(3).
           05 WS-TOTAL-PAIS               PIC 9(9).
           05 WS-TOTAL-CIUDAD             PIC 9(9).
           05 WS-TOTAL-SEXO               PIC 9(9).
           05 WS-TOTAL-HABITANTES         PIC 9(9).

       77  WS-CONT-LECTURA                PIC 99.
       77  WS-CONT-REGISTROS              PIC 99.

       PROCEDURE DIVISION.
           PERFORM 1000-I-INICIO
           PERFORM 2000-I-PROCESO UNTIL FS-ENTRADA-FIN.
           PERFORM 9000-I-FINAL.
               STOP RUN.
      *----------------------------------------------------------------*
       1000-I-INICIO.
           INITIALIZE VARIABLES
           PERFORM 1100-ABRIRENTRADA.
           PERFORM 1150-ABRIRSALIDA.
           PERFORM 5000-LEERENTRADA.
       1000-F-INICIO.EXIT.
      *----------------------------------------------------------------*
       1100-ABRIRENTRADA.
           OPEN INPUT ENTRADA.

           IF NOT FS-ENTRADA-OK
               DISPLAY "ERROR APERTURA ENTRADA FS: " FS-ENTRADA
               PERFORM 9000-I-FINAL
           END-IF.
       1100-F-ABRIRENTRADA.EXIT.
      *----------------------------------------------------------------*
       1150-ABRIRSALIDA.

           OPEN OUTPUT SALIDA.

           IF NOT FS-SALIDA-OK
               DISPLAY "ERROR APERTURA SALIDA FS: " FS-SALIDA
               PERFORM 9000-I-FINAL
           END-IF.
       1150-F-ABRIRSALIDA.EXIT.
      *----------------------------------------------------------------*
       2000-I-PROCESO.
           MOVE REG-PAIS TO WS-PAIS-ANT
           INITIALIZE WS-TOTAL-PAIS

           PERFORM 2100-I-CORTE-CONTROL-PAIS
           UNTIL REG-PAIS <> WS-PAIS-ANT OR FS-ENTRADA-FIN

           PERFORM 3000-TOTALIZAR-HABXPAIS.
       2000-F-PROCESO.
           EXIT.
      *----------------------------------------------------------------*
       2100-I-CORTE-CONTROL-PAIS.
           MOVE REG-CIUDAD TO WS-CIUDAD-ANT
           INITIALIZE WS-TOTAL-CIUDAD

           PERFORM 2200-I-CORTE-CONTROL-CIUDAD
           UNTIL REG-CIUDAD <> WS-CIUDAD-ANT
           OR REG-PAIS <> WS-PAIS-ANT
           OR FS-ENTRADA-FIN.

           PERFORM 3001-TOTALIZAR-HABXCIUDAD.
       2100-F-CORTE-CONTROL-PAIS.
           EXIT.
      *----------------------------------------------------------------*
       2200-I-CORTE-CONTROL-CIUDAD.
           MOVE REG-SEXO TO WS-SEXO-ANT
           INITIALIZE WS-TOTAL-SEXO

           PERFORM 2300-I-CORTE-CONTROL-SEXO
           UNTIL REG-SEXO <> WS-SEXO-ANT
           OR REG-PAIS <> WS-PAIS-ANT
           OR REG-CIUDAD <> WS-CIUDAD-ANT
           OR FS-ENTRADA-FIN.

           PERFORM 3002-TOTALIZAR-HABXSEXO.
       2200-F-CORTE-CONTROL-CIUDAD.
           EXIT.
      *----------------------------------------------------------------*
       2300-I-CORTE-CONTROL-SEXO.
           ADD REG-HABITANTES TO WS-TOTAL-SEXO WS-TOTAL-PAIS
           WS-TOTAL-CIUDAD

           WRITE REG-SALIDA FROM REG-ENTRADA.
           PERFORM 5000-LEERENTRADA.
       2300-F-CORTE-CONTROL-SEXO.
           EXIT.
      *----------------------------------------------------------------*
       3000-TOTALIZAR-HABXPAIS.
           MOVE "CANT TOTAL DE HABITANTES POR PAIS: " TO TEXTO
           MOVE WS-TOTAL-PAIS TO TOTALES

           PERFORM 4000-ESCRIBIR-SALIDA.
       3000-F-TOTALIZAR-HABXPAIS.
      *----------------------------------------------------------------*
       3001-TOTALIZAR-HABXCIUDAD.
           MOVE "CANT TOTAL DE HABITANTES POR CIUDAD: " TO TEXTO
           MOVE WS-TOTAL-CIUDAD TO TOTALES

           PERFORM 4000-ESCRIBIR-SALIDA.
       3001-F-TOTALIZAR-HABXCIUDAD.
      *----------------------------------------------------------------*
       3002-TOTALIZAR-HABXSEXO.
           MOVE "CANT TOTAL DE HABITANTES POR SEXO: " TO TEXTO
           MOVE WS-TOTAL-SEXO TO TOTALES

           PERFORM 4000-ESCRIBIR-SALIDA.
       3001-F-TOTALIZAR-HABXSEXO.
      *----------------------------------------------------------------*
       4000-ESCRIBIR-SALIDA.
           IF NOT FS-SALIDA-OK
               DISPLAY "ERROR ESCRITURA SALIDA FS: " FS-SALIDA
           ELSE
               WRITE REG-SALIDA FROM ESTRUCTURA-SALIDA
           END-IF.
       4000-F-ESCRIBIR-SALIDA.
      *----------------------------------------------------------------*
       5000-LEERENTRADA.
           READ ENTRADA
           EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                   ADD 1 TO WS-CONT-LECTURA
               WHEN FS-ENTRADA-FIN
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR LECTURA ENTRADA"
           END-EVALUATE.
       5000-F-LEERENTRADA.EXIT.
      *----------------------------------------------------------------*
       9000-I-FINAL.
           CLOSE ENTRADA.
           CLOSE SALIDA.
       9000-F-FINAL.
           EXIT.
      *----------------------------------------------------------------*
