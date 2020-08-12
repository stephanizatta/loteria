      $set sourceformat"free"

      *>Divisão de identificação do programa
       identification division.
       program-id. "desafio2".
       author. "Stephani S. Zatta".
       installation. "PC".
       date-written. 21/07/2020.
       date-compiled. 11/08/2020.

      *>Divisão para configuração do ambiente
       environment division.
       configuration section.

           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.

      *>----Variaveis de trabalho
       working-storage section.

       01 ws-sorteio-numeros.
           05 ws-semente                           pic 9(10).
           05 ws-semente1                          pic 9(10).
           05 ws-num-random                        pic 9(02)V9999999.

       01  ws-numeros-sorteados occurs 10.
           05 ws-num-sorteado                      pic 9(02).

       01  ws-indice-sorteio.
           05 ws-ind-sorteio                       pic 9(02).

       01 ws-numeros-apostados occurs 10.
           05 ws-num-apostado                      pic 9(02).

       01  ws-indice-aposta.
           05 ws-ind-aposta                        pic 9(02).

       77  ws-quantia-numeros                      pic 9(02).
       77  ws-num-aux                              pic 9(02).
       77  ws-pontos                               pic 9(02).

      *>----Variaveis para comunicação entre programas
       linkage section.

      *>----Declaração de tela
       screen section.

      *>Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  Procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.

           move zero to ws-quantia-numeros
           move zero to ws-pontos
           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       processamento section.

           perform quant-numero-para-aposta
           perform escolha-de-numeros
           perform sorteio
           perform verifica-pontos
           perform ganhou-perdeu
           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Coleta a quantia de numeros para aposta --- OK
      *>------------------------------------------------------------------------
       quant-numero-para-aposta section.

           display "Mega-Sena - escolha numeros diferentes de 1 a 60"
           display " "

           display "Voce deseja apostar quantos numeros? (6-10) "
           accept ws-quantia-numeros
           display " "

        *>--- aqui garante que o jogador escolhera de 6 a 10 numeros, nao menos nem mais ---
           if ws-quantia-numeros <= 6
           or ws-quantia-numeros >= 10
               perform until ws-quantia-numeros = 6
                          or ws-quantia-numeros = 7
                          or ws-quantia-numeros = 8
                          or ws-quantia-numeros = 9
                          or ws-quantia-numeros = 10
                   display "Voce tem que apostar no minimo 6 numeros e no maximo 10."
                   display "Voce deseja apostar quantos numeros? (6-10) "
                   accept ws-quantia-numeros
                   display " "
               end-perform
           end-if

           .
       quant-numero-para-aposta-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Coleta de numeros para aposta
      *>------------------------------------------------------------------------
       escolha-de-numeros section.
       *>--- aqui coleta os numeros que o jogador quer apostar ---

           perform varying ws-ind-aposta from 1 by 1 until ws-ind-aposta > ws-quantia-numeros

               display "Insira um numero para a aposta: "
               accept ws-num-aux
               move ws-num-aux to ws-num-apostado(ws-ind-aposta)

       *>      caso o jogador escolha um numero menor que 1 ou maior que 60
               if ws-num-apostado(ws-ind-aposta) < 1
               or ws-num-apostado(ws-ind-aposta) > 60 then
                   perform until ws-num-apostado(ws-ind-aposta) > 00
                             and ws-num-apostado(ws-ind-aposta) <= 60
                       display "Os numeros devem ser entre 1 e 60."
                       display " "
                       display "Insira um numero para a aposta: "
                       accept ws-num-apostado(ws-ind-aposta)
                       display " "
                   end-perform
               end-if

       *>      caso o jogador escolha numeros iguais
            *> caso o segundo numero escolhido seja igual ao primeiro
               if ws-ind-aposta = 2 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

            *> caso o terceiro numero escolhido seja igual a outro
               if ws-ind-aposta = 3 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 2) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 2)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

            *> caso o quarto numero escolhido seja igual a outro
               if ws-ind-aposta = 4 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 2)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 3) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 2)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 3)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

            *> caso o quinto numero escolhido seja igual a outro
               if ws-ind-aposta = 5 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 2)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 3)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 4) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 2)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 3)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 4)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

            *> caso o sexto numero escolhido seja igual a outro
               if ws-ind-aposta = 6 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 2)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 3)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 4)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 5) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 2)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 3)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 4)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 5)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

            *> caso o setimo numero escolhido (caso houver mais que 6) seja igual a outro
               if ws-ind-aposta = 7 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 2)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 3)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 4)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 5)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 6) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 2)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 3)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 4)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 5)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 6)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

            *> caso o oitavo numero escolhido (caso houver mais que 6) seja igual a outro
               if ws-ind-aposta = 8 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 2)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 3)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 4)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 5)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 6)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 7) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 2)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 3)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 4)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 5)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 6)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 7)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

            *> caso o nono numero escolhido (caso houver mais que 6) seja igual a outro
               if ws-ind-aposta = 9 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 2)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 3)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 4)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 5)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 6)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 8) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 2)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 3)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 4)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 5)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 6)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 7)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 8)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

            *> caso o decimo numero escolhido (caso houver mais que 6) seja igual a outro
               if ws-ind-aposta = 10 then
                   if ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 1)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 2)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 3)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 4)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 5)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 6)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-apostado(ws-ind-aposta) = ws-num-apostado(ws-ind-aposta - 9) then
                       perform until ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 1)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 2)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 3)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 4)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 5)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 6)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 7)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 8)
                                 and ws-num-apostado(ws-ind-aposta) <> ws-num-apostado(ws-ind-aposta - 9)
                           display "Os numeros nao podem se repetir..."
                           display " "
                           display "Insira um numero para a aposta: "
                           accept ws-num-apostado(ws-ind-aposta)
                       end-perform
                   end-if
               end-if

           end-perform

           .
       escolha-de-numeros-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Sorteio dos numeros --- OK
      *>------------------------------------------------------------------------
       sorteio section.
       *>--- aqui sorteia de forma randomica os 6 numeros ---

           perform varying ws-ind-sorteio from 1 by 1 until ws-ind-sorteio > 6

               perform delay-semente-sorteio

               compute ws-num-random = function random(ws-semente)
               multiply ws-num-random by 60 giving ws-num-sorteado(ws-ind-sorteio)

       *>  caso o sorteio dê numero 0
               if ws-num-sorteado(ws-ind-sorteio) = 00 then
                   perform until ws-num-sorteado(ws-ind-sorteio) >= 1
                       perform delay-semente-sorteio
                       compute ws-num-random = function random(ws-semente)
                       multiply ws-num-random by 60 giving ws-num-sorteado(ws-ind-sorteio)
                   end-perform
               end-if

       *>  caso o sorteio dê numeros iguais
            *> se for o segundo numero sorteado
               if ws-ind-sorteio = 2 then
                   if ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 1) then
                       perform until ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 1)
                           perform delay-semente-sorteio
                           compute ws-num-random = function random(ws-semente)
                           multiply ws-num-random by 60 giving ws-num-sorteado(ws-ind-sorteio)
                       end-perform
                   end-if
               end-if

            *> se for o terceiro numero sorteado
               if ws-ind-sorteio = 3 then
                   if ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 1)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 2) then
                       perform until ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 1)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 2)
                           perform delay-semente-sorteio
                           compute ws-num-random = function random(ws-semente)
                           multiply ws-num-random by 60 giving ws-num-sorteado(ws-ind-sorteio)
                       end-perform
                   end-if
               end-if

            *> se for o quarto numero sorteado
               if ws-ind-sorteio = 4 then
                   if ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 1)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 2)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 3) then
                       perform until ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 1)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 2)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 3)
                           perform delay-semente-sorteio
                           compute ws-num-random = function random(ws-semente)
                           multiply ws-num-random by 60 giving ws-num-sorteado(ws-ind-sorteio)
                       end-perform
                   end-if
               end-if

            *> se for o quinto numero sorteado
               if ws-ind-sorteio = 5 then
                   if ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 1)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 2)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 3)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 4) then
                       perform until ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 1)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 2)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 3)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 4)
                           perform delay-semente-sorteio
                           compute ws-num-random = function random(ws-semente)
                           multiply ws-num-random by 60 giving ws-num-sorteado(ws-ind-sorteio)
                       end-perform
                   end-if
               end-if

            *> se for o sexto numero sorteado
               if ws-ind-sorteio = 6 then
                   if ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 1)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 2)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 3)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 4)
                   or ws-num-sorteado(ws-ind-sorteio) = ws-num-sorteado(ws-ind-sorteio - 5) then
                       perform until ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 1)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 2)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 3)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 4)
                                 and ws-num-sorteado(ws-ind-sorteio) <> ws-num-sorteado(ws-ind-sorteio - 5)
                           perform delay-semente-sorteio
                           compute ws-num-random = function random(ws-semente)
                           multiply ws-num-random by 60 giving ws-num-sorteado(ws-ind-sorteio)
                       end-perform
                   end-if
               end-if

           *>  display dos numeros sorteados
               display "Numero sorteado: " ws-num-sorteado(ws-ind-sorteio)
           end-perform

           .
       sorteio-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Delay do Sorteio --- OK
      *>------------------------------------------------------------------------
       delay-semente-sorteio section.
       *>--- aqui faz com que o programa tenha um delay de 50 centesimos de segundo ao dar o numero sorteados ---
       *>--- assim impossibilita com que os numeros apareçam em sequencia ---

           perform 50 times
               accept ws-semente1 from time
               move ws-semente1 to ws-semente

               perform until ws-semente > ws-semente1
                   accept ws-semente from time
               end-perform

           end-perform
           .
       delay-semente-sorteio-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Pontos - se igual a 6, ganhou --- OK
      *>------------------------------------------------------------------------
       verifica-pontos section.
       *> verifica se os numeros apostados são iguais aos sorteados

        *>    ultimo-primeiro numero sorteado       ultimo-primeiro numero apostado

        *> verifica primeiro numero sorteado
           if ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 1)
           or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 2)
           or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 3)
           or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 4)
           or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 5)
           or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 6) then
               add 1 to ws-pontos
               if ws-quantia-numeros = 7 then
                   if ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 7) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 8 then
                   if ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 8) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 9) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 9)
                   or ws-num-sorteado(ws-ind-sorteio - 1) = ws-num-apostado(ws-ind-aposta - 10) then
                       add 1 to ws-pontos
                   end-if
               end-if
           end-if

        *> verifica segundo numero sorteado
           if ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 1)
           or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 2)
           or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 3)
           or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 4)
           or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 5)
           or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 6) then
               add 1 to ws-pontos
               if ws-quantia-numeros = 7 then
                   if ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 7) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 8 then
                   if ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 8) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 9) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 9)
                   or ws-num-sorteado(ws-ind-sorteio - 2) = ws-num-apostado(ws-ind-aposta - 10) then
                       add 1 to ws-pontos
                   end-if
               end-if
           end-if

        *> verifica terceiro numero sorteado
           if ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 1)
           or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 2)
           or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 3)
           or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 4)
           or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 5)
           or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 6) then
               add 1 to ws-pontos
               if ws-quantia-numeros = 7 then
                   if ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 7) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 8 then
                   if ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 8) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 9) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 9)
                   or ws-num-sorteado(ws-ind-sorteio - 3) = ws-num-apostado(ws-ind-aposta - 10) then
                       add 1 to ws-pontos
                   end-if
               end-if
           end-if

        *> verifica quarto numero sorteado
           if ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 1)
           or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 2)
           or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 3)
           or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 4)
           or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 5)
           or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 6) then
               add 1 to ws-pontos
               if ws-quantia-numeros = 7 then
                   if ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 7) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 8 then
                   if ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 8) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 9) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 9)
                   or ws-num-sorteado(ws-ind-sorteio - 4) = ws-num-apostado(ws-ind-aposta - 10) then
                       add 1 to ws-pontos
                   end-if
               end-if
           end-if

        *> verifica quinto numero sorteado
           if ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 1)
           or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 2)
           or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 3)
           or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 4)
           or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 5)
           or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 6) then
               add 1 to ws-pontos
               if ws-quantia-numeros = 7 then
                   if ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 7) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 8 then
                   if ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 8) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 9) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 9)
                   or ws-num-sorteado(ws-ind-sorteio - 5) = ws-num-apostado(ws-ind-aposta - 10) then
                       add 1 to ws-pontos
                   end-if
               end-if
           end-if

        *> verifica sexto numero sorteado
           if ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 1)
           or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 2)
           or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 3)
           or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 4)
           or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 5)
           or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 6) then
               add 1 to ws-pontos
               if ws-quantia-numeros = 7 then
                   if ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 7) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 8 then
                   if ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 8) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 9) then
                       add 1 to ws-pontos
                   end-if
               end-if
               if ws-quantia-numeros = 9 then
                   if ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 7)
                   or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 8)
                   or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 9)
                   or ws-num-sorteado(ws-ind-sorteio - 6) = ws-num-apostado(ws-ind-aposta - 10) then
                       add 1 to ws-pontos
                   end-if
               end-if
           end-if

           display " "
           display "Total de acertos: " ws-pontos

           .
       verifica-pontos-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Ganhou ou Perdeu --- OK
      *>------------------------------------------------------------------------
       ganhou-perdeu section.
       *> avisa se o jogador ganhou ou perdeu

           if ws-pontos = 6 then
               display "*** Voce ganhou!!! ***"
           else
               display "*** Voce perdeu. ***"
           end-if

           .
       ganhou-perdeu-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.
           stop run
           .
       finaliza-exit.
           exit.







