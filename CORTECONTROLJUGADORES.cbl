       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORTECONTROLJUGADORES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO "JUGCOPAM.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA.

           SELECT SALIDA ASSIGN TO "salidajugadores.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA.
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA.
           COPY JUGCOPAM.

       FD  SALIDA.
           01 REG-SALIDA PIC X(170).

       WORKING-STORAGE SECTION.
       01  ESTRUCTURA-SALIDA.
           05 TEXTO1                   PIC X(5) VALUE "PAIS".
           05 PAIS                     PIC X(4).
           05 TEXTO2                   PIC X(22)
           VALUE "CANTIDAD DE JUGADORES ".
           05 CANTJUGADORES            PIC X(5).
           05 TEXTO3                   PIC X(18)
           VALUE "CANTIDAD DE GOLES ".
           05 CANTGOLES                PIC X(4).

       01  VARIABLES.
           05 FS-ENTRADA               PIC XX.
               88 FS-ENTRADA-OK        VALUE "00".
               88 FS-ENTRADA-FIN       VALUE "10".

           05 FS-SALIDA                PIC XX.
               88 FS-SALIDA-OK         VALUE "00".
               88 FS-SALIDA-FIN        VALUE "10".

           05 WS-PAIS-ANT                  PIC X(03).
           05 WS-TOTAL-JUGPORPAIS          PIC 9(4).
           05 WS-TOTAL-GOLESPORPAIS        PIC 9(4).

       77  WS-CONT-LECTURA                 PIC 99.

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
           MOVE JUGCOPAM-PAIS TO WS-PAIS-ANT
           INITIALIZE WS-TOTAL-JUGPORPAIS
           INITIALIZE WS-TOTAL-GOLESPORPAIS

           PERFORM 2100-I-CORTE-CONTROL-PAIS
           UNTIL JUGCOPAM-PAIS <> WS-PAIS-ANT OR FS-ENTRADA-FIN

           PERFORM 3000-TOTALIZAR-JUGYGOLESPORPAIS.
       2000-F-PROCESO.EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-I-CORTE-CONTROL-PAIS.
           ADD 1 TO WS-TOTAL-JUGPORPAIS
           ADD JUGCOPAM-GOLES TO WS-TOTAL-GOLESPORPAIS

           PERFORM 5000-LEERENTRADA.
       2100-F-CORTE-CONTROL-PAIS.EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-TOTALIZAR-JUGYGOLESPORPAIS.
           MOVE WS-PAIS-ANT TO PAIS
           MOVE WS-TOTAL-JUGPORPAIS TO CANTJUGADORES
           MOVE WS-TOTAL-GOLESPORPAIS TO CANTGOLES

           PERFORM 6000-ESCRIBIRSALIDA.
       3000-F-TOTALIZAR-JUGPORPAIS.EXIT.
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
       6000-ESCRIBIRSALIDA.
           IF NOT FS-SALIDA-OK
               DISPLAY "ERROR ESCRITURA SALIDA FS: " FS-SALIDA
           ELSE
               WRITE REG-SALIDA FROM ESTRUCTURA-SALIDA
           END-IF.

       6000-F-ESCRIBIRSALIDA.
      *----------------------------------------------------------------*
       9000-I-FINAL.
           CLOSE ENTRADA.
           CLOSE SALIDA.
       9000-F-FINAL.
           EXIT.
      *----------------------------------------------------------------*
