       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX07.
       AUTHOR.        LEVI LIMA BELTRAO.
       DATE-WRITTEN.  24/03/2025.
       DATE-COMPILED. 24/03/2025.
       SECURITY.      APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.       LE TODOS OS REGISTROS NO ARQUIVO CADALU 
      *               (NUM, NOME, NOTA1, 2, 3, 4 E SEXO) E SELECIONA
      *               PARA GRAVACAO NO ARQUIVO CADATU (NUM, NOME, 
      *               MEDIA, SEXO) SOMENTE OS REGISTROS DO SEXO FEMININO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LEVI-PC.
       OBJECT-COMPUTER. LEVI-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADALU.DAT".

       01 REG-ENT.
           02 NUM-ENT             PIC 9(05).
           02 NOME-ENT            PIC X(20).
           02 NOTA1               PIC 9(2)V99.
           02 NOTA2               PIC 9(2)V99.
           02 NOTA3               PIC 9(2)V99.
           02 NOTA4               PIC 9(2)V99.
           02 SEXO-ENT                PIC X(01).

       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
       
       01 REG-SAI.
           02 NUM-SAI             PIC 9(05).
           02 NOME-SAI            PIC X(20).
           02 MEDIA               PIC 9(2)V99.
           02 SEXO-SAI            PIC X(01).
       
       WORKING-STORAGE SECTION.
       77 FIM-ARQ                 PIC X(03)       VALUE IS "NAO".
       77 SOMA-NOTA               PIC 9(2)V99.
       77 WMEDIA                  PIC 9(2)V99.

       PROCEDURE DIVISION.
       EXFIX.
       
       PERFORM INICIO.
       PERFORM PRINCIPAL
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
           OPEN INPUT CADALU
               OUTPUT CADATU.
           PERFORM LEITURA.
       
       LEITURA.
           READ CADALU
               AT END
               MOVE "SIM" TO FIM-ARQ.
       
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
       
       GRAVACAO.
           IF SEXO-ENT EQUAL "F" OR SEXO-ENT EQUAL "f" THEN
              MOVE ZEROS     TO SOMA-NOTA
              MOVE ZEROS     TO WMEDIA
              ADD NOTA1, NOTA2, NOTA3, NOTA4 TO SOMA-NOTA
              DIVIDE SOMA-NOTA BY 4 GIVING WMEDIA
              MOVE NUM-ENT   TO   NUM-SAI
              MOVE NOME-ENT  TO   NOME-SAI
              MOVE WMEDIA    TO   MEDIA
              MOVE SEXO-ENT  TO   SEXO-SAI
              WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADATU.