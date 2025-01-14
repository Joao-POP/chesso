# Funções

Entre entrada de dados pelo usuário e processamento, chesso precisa:

a. Validar cada movimento feito em tempo real; 
b. Ler e editar arquivos FEN;
c. Carregar e salvar PGNs das partidas dos jogadores;
d. Enviar e receber comandos pela rede.

# Blocos fundamentais

Podemos categorizar essas funções em módulos:

a. san\_validator
b. pgn\_parser
c. fen\_validator

## Lógica

Dessa forma, a lógica de programa pode ser descrita em passos:

1. Ler entrada (mouse, teclado);
2. Validar se a entrada é legal:
    a. Se sim, atualize a interface e envie comandos;
    b. Se não, retorne ao estado antes de executar o movimento.

### Em pseudocódigo

```
standard_position = open("standard.fen")
board = fen_parser::from_string(standard_position)
move = read_input()
is_legal = san_validator::validate(board, move)

if is_legal:
    board.apply(move)
```

# API

O núcleo da API é a struct `Move`, um movimento de xadrez que pode ser
compartilhado internamente para validação, transmissão ou atualização do
tabuleiro.
