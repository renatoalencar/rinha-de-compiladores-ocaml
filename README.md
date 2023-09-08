# Rinha(ML?) em OCaml

## Ideias

- Reproduzir a sintaxe de definida em https://github.com/aripiprazole/rinha-de-compiler/blob/main/SPECS.md
- Fazer type inference onde for possível, com objetivo de performance.
- Tem poucos tipos, o que deve tornar a implementacao mais facil.
  - Hindley-Milner sem ADTs =).
- Ter um frontend JSON e um da propria linguagem com Menhir.
- Fazer inlining quando possível, provavelmente vou ter que ter algum passo de otimizacoes especificas. Ideias de otimizacao abaixo.
- Backend WebAssembly, flambda, LLVM e x86.

## Otimizacoes

- print(...) pra printf
  Exemplo: print("fib " + fib(1000)) -> printf("fib %d" + fib(1000))
  TODO: comparar com outras ideias.
- Conversao em tempo de compilacao -> print(true) -> printf("true") -> puts("true")
- Alguns formatos de tupla serem gerados como vetores direto na memoria:
  Exemplo: (1, (2, (3, 4))) -> [1, 2, 3, 4]
- Acesso a tuplas aninhadas serem traduzidas para acesso a um array. Pode depender do tipo retornado.
  Exemplo: second(second((1, (2, (3, 4))))) -> &[1, 2, 3, 4][2]
  Exemplo: first(second(second((1, (2, (3, 4)))))) -> [1, 2, 3, 4][2]
- Existem outras paradigmas de execucao que podem aproveitar melhor paralelizacao?

## Garbage collection

- Ainda nao tenho certeza de como fazer isso, mas provavelmente só um ref counting deve resolver, no comeco devo fazer so o couting mesmo.
- Dependendo do caso, devo jogar tudo na stack mesmo e torcer pra dar certo.
- Não há tempo suficiente pra escrever algo grande o suficiente que valha a pena implementar generational garbage collection. Quando mais simples (e menor) melhor assim consigo aproveitar mais o cache em memoria (posso mesmo???).
- Alocação pode ser feita como em OCaml, ter um registrador e só incrementar quando necessário.

## Runtime

- Talvez consiga performance com mais previsibidade se implementar tudo na mão ao invés de usar libc, funcoes como concat e print fazendo as chamadas de sistema diretamente.
