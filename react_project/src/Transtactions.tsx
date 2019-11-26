import React, { useState } from 'react'
import transactionsJSON from './transactions.json'

export interface Transaction {
  data: {
    year: number
    month: number
    dayOfMonth: number
    hourOfDay: number
    second: number
  }
  textoIdentificador: string
  valor: number
  descricao: string
  numeroDOC: string
  classificada: boolean
  tipos: string[]
  arquivos: string[]
}

const Transactions = () => {
  const [year, setYear] = useState('')
  const [month, setMonth] = useState('')

  const [transactions] = useState<Transaction[]>(transactionsJSON)

  const transactionsByYear = () =>
    transactions.filter(
      (value: Transaction) => value.data.year === Number(year)
    )

  const transactionsByYearAndMonth = () =>
    transactionsByYear().filter(
      (value: Transaction) => value.data.month === Number(month)
    )

  const creditByYearAndMonth = () =>
    transactionsByYearAndMonth().reduce(
      (total: number, current: Transaction) =>
        total + (current.valor < 0 ? 0 : current.valor),
      0
    )

  const debitByYearAndMonth = () =>
    transactionsByYearAndMonth().reduce(
      (total: number, current: Transaction) =>
        total + (current.valor > 0 ? 0 : current.valor),
      0
    )

  const restYearAndMonth = () => creditByYearAndMonth() - debitByYearAndMonth()

  const maxYearAndMonth = () =>
    transactionsByYearAndMonth().reduce(
      (total: number, current: Transaction) => Math.max(total, current.valor),
      0
    )

  const minYearAndMonth = () =>
    transactionsByYearAndMonth().reduce(
      (total: number, current: Transaction) => Math.min(total, current.valor),
      0
    )

  const meanReceptsYear = () => {}

  const meanCostsYear = () => {}

  const meanRestYear = () => {}

  const flowYearAndMonth = () => {}

  return (
    <div>
      <div>
        ano
        <input
          title='ANO'
          type='text'
          value={year}
          onChange={event => {
            setYear(event.target.value)
          }}
        />
      </div>
      <div>
        mes
        <input
          title='MES'
          type='text'
          value={month}
          onChange={event => {
            setMonth(event.target.value)
          }}
        />
      </div>
      <div>
        <button
          onClick={() => {
            console.log(transactionsByYear())
          }}
        >
          Transações por ano
        </button>
      </div>
      <div>
        <button
          onClick={() => {
            console.log(transactionsByYearAndMonth())
          }}
        >
          Filtrar transações por ano e mês.{' '}
        </button>
      </div>
      <div>
        <button>
          Calcular o valor das receitas (créditos) em um determinado mês e ano.
        </button>
      </div>
      <div>
        <button>
          Calcular o valor das despesas (débitos) em um determinado mês e ano.
        </button>
      </div>
      <div>
        <button>
          Calcular a sobra (receitas - despesas) de determinado mês e ano
        </button>
      </div>

      <div>
        <button> Calcular o saldo final em um determinado ano e mês</button>
      </div>
      <div>
        <button>
          Calcular o saldo máximo atingido em determinado ano e mês
        </button>
      </div>
      <div>
        <button> Calcular a média das receitas em determinado ano</button>
      </div>
      <div>
        <button> Calcular a média das sobras em determinado ano</button>
      </div>
      <div>
        <button>Retornar o fluxo de caixa de determinado mês/ano.</button>
      </div>
    </div>
  )
}

export default Transactions
