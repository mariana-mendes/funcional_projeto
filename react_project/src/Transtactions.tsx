import React from 'react'

const Transactions = () => {
  return (
    <div>
      <button> Transações por ano</button>
      <button> Filtrar transações por ano e mês. </button>
      <button>
        Calcular o valor das receitas (créditos) em um determinado mês e ano.
      </button>
      <button>
        Calcular o valor das despesas (débitos) em um determinado mês e ano.
      </button>
      <button>
        Calcular a sobra (receitas - despesas) de determinado mês e ano
      </button>
      <button> Calcular o saldo final em um determinado ano e mês</button>
      <button>Calcular o saldo máximo atingido em determinado ano e mês</button>
      <button> Calcular a média das receitas em determinado ano</button>
      <button> Calcular a média das sobras em determinado ano</button>
      <button>Retornar o fluxo de caixa de determinado mês/ano.</button>
    </div>
  )
}

export default Transactions
