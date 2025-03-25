       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX08.
       AUTHOR.        LEVI LIMA BELTRAO.
       DATE-WRITTEN.  25/03/2025.
       DATE-COMPILED. 25/03/2025.
       SECURITY.      SOMENTE O AUTOR PODE MODIFICAR.
      *REMARKS.       LE OS REGISTROS DO ARQUIVO CADFUN (CODIGO, NOME,
      *               SALARIO), CALCULA PARA TODOS OS REGISTROS DE 
      *               ENTRADA O REAJUSTE DE SALARIO E GRAVA OS REGISTROS
      *               DE SAIDA NO ARQUIVO CADSAI (CODIGO, NOME, SALARIO
      *               REAJUSTADO).

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LEVI-PC.
       OBJECT-COMPUTER. LEVI-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
           
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFUN ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADFUN
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADFUN.DAT".
       
       01 REG-ENT.
           02 COD-ENT             PIC 9(05).
           02 NOME-ENT            PIC X(20).
           02 SAL-ENT             PIC 9(05)V99.

       FD CADSAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSAI.DAT".

       01 REG-SAI.
           02 COD-SAI             PIC 9(05).
           02 NOME-SAI            PIC X(20).
           02 SAL-SAI             PIC 9(05)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ                 PIC X(03)       VALUE IS "NAO".
       77 REAJUSTE                PIC 9(01)V99.
       77 SAL-REAJUSTADO          PIC 9(05)V99.

       PROCEDURE DIVISION.
       EXFIX.
           
       PERFORM INICIO.
       PERFORM PRINCIPAL
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
           OPEN INPUT CADFUN
               OUTPUT CADSAI.
           PERFORM LEITURA.

       LEITURA.
           READ CADFUN
               AT END
               MOVE "SIM" TO FIM-ARQ.
       
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
        
       GRAVACAO.
           MOVE ZEROS        TO   REAJUSTE.
           MOVE ZEROS        TO   SAL-REAJUSTADO.
           IF SAL-ENT NOT > 1000 THEN
               MOVE 1,12       TO   REAJUSTE
           ELSE
               IF SAL-ENT NOT > 2000 THEN
                   MOVE 1,11  TO   REAJUSTE
               ELSE
                   MOVE 1,10  TO   REAJUSTE.
           
           MULTIPLY SAL-ENT BY REAJUSTE GIVING SAL-REAJUSTADO.
           MOVE COD-ENT      TO   COD-SAI.
           MOVE NOME-ENT     TO   NOME-SAI.
           MOVE SAL-REAJUSTADO TO SAL-SAI.
           WRITE REG-SAI.
       
       TERMINO.
           CLOSE CADFUN
                 CADSAI.
