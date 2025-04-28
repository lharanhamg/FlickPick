# 🎬 FlickPick
### Indicador de filmes em PROLOG

**FlickPick** é um sistema de recomendação de filmes desenvolvido em **Prolog**, para a cadeira de Lógica Aplicada à Programação da professora Tatiana Simões, referente ao período 2024.2 da UFPB.
O objetivo do projeto é auxiliar usuários a encontrar filmes de seu interesse com base em filtros como ano de lançamento, duração, gênero, avaliação IMDb e número de Oscars.

---

## ⚙️ Funcionamento do Código

O sistema carrega uma base de dados de filmes a partir de um arquivo CSV e permite que o usuário interaja de forma personalizada. Durante a execução, o usuário pode definir filtros como:

- Ano mínimo de lançamento
- Duração máxima (em minutos)
- Gênero
- Avaliação mínima no IMDb
- Número mínimo de Oscars ganhos

A partir desses filtros, o Prolog processa as informações e exibe uma lista de filmes que atendem aos critérios selecionados.

---

## ▶️ Como Usar

1. Certifique-se de ter o **SWI-Prolog** instalado em sua máquina.

2. No terminal, navegue até o diretório do projeto e execute:

```bash
swipl -q -f recommender.pl
```

3. Para rodar o programa, execute:

```bash
?- main.
```

4. Siga as instruções interativas para aplicar seus filtros e receber sugestões de filmes.

---

## 🛠️ Tecnologias

- **Prolog (SWI-Prolog)**: implementação da lógica de recomendação  
- **CSV Library**: leitura e conversão de dados em fatos Prolog  
- **Google Colab (Python)**: pré-processamento e EDA do dataset IMDB  

---

## 👥 Integrantes

- Miguel de Queiroz F. Soares  
- Luis Henrique A. Magalhães  
- Luigi Emanuel M. Schmitt  
