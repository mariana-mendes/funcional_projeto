import React, { useState } from 'react'
import transactionsJSON from './transactions.json'
import './form.css'

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

const valid = (array: string[]) =>
  array.includes('SALDO_CORRENTE') ||
  array.includes('VALOR_APLICACAO') ||
  array.includes('APLICACAO')

const Transactions = () => {
  const [year, setYear] = useState('')
  const [month, setMonth] = useState('')

  const [transactions] = useState<Transaction[]>(transactionsJSON)

  const validTransactions = (array: Transaction[]) =>
    array.filter((value: Transaction) => valid(value.tipos))

  const transactionsByYear = () =>
    transactions.filter(
      (value: Transaction) => value.data.year === Number(year)
    )

  const transactionsByYearAndMonth = () =>
    transactionsByYear().filter(
      (value: Transaction) => value.data.month === Number(month)
    )

  const creditByYearAndMonth = () =>
    validTransactions(transactionsByYearAndMonth()).reduce(
      (total: number, current: Transaction) =>
        total + (current.valor < 0 ? 0 : current.valor),
      0
    )

  const debitByYearAndMonth = () =>
    validTransactions(transactionsByYearAndMonth()).reduce(
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

  const meanReceptsYear = () => {
    const qtdCredit = validTransactions(
      transactions.filter(
        (value: Transaction) =>
          value.data.year === Number(year) && value.valor >= 0
      )
    ).length

    const totalValue = validTransactions(transactionsByYear()).reduce(
      (total: number, current: Transaction) =>
        total + (current.valor >= 0 ? 0 : current.valor),
      0
    )

    return totalValue / qtdCredit
  }

  const meanCostsYear = () => {
    const qtdCredit = validTransactions(
      transactions.filter(
        (value: Transaction) =>
          value.data.year === Number(year) && value.valor < 0
      )
    ).length

    const totalValue = validTransactions(transactionsByYear()).reduce(
      (total: number, current: Transaction) =>
        total + (current.valor < 0 ? 0 : current.valor),
      0
    )

    return totalValue / qtdCredit
  }

  const meanRestYear = () => {}

  const flowYearAndMonth = () => {}

  return (
    <div>
      <div className='row'>
        <div className='column'>
          <form id='divForm'>
            <label>Ano </label>
            <input
              title='ANO'
              type='text'
              value={year}
              onChange={event => {
                setYear(event.target.value)
              }}
            />

            <label>Mes</label>
            <input
              title='MES'
              type='text'
              value={month}
              onChange={event => {
                setMonth(event.target.value)
              }}
            />
          </form>
        </div>
        <div className='column'>
          <div>
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
                Filtrar transações por ano e mês.
              </button>
            </div>
            <div>
              <button
                onClick={() => {
                  console.log(creditByYearAndMonth())
                }}
              >
                Calcular o valor das receitas (créditos) em um determinado mês e
                ano.
              </button>
            </div>
            <div>
              <button
                onClick={() => {
                  console.log(debitByYearAndMonth())
                }}
              >
                Calcular o valor das despesas (débitos) em um determinado mês e
                ano.
              </button>
            </div>
            <div>
              <button>
                Calcular a sobra (receitas - despesas) de determinado mês e ano
              </button>
            </div>

            <div>
              <button>
                {' '}
                Calcular o saldo final em um determinado ano e mês
              </button>
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
        </div>
        <div className='column'>
          <h2> RESULT </h2>
        </div>
      </div>
    </div>
  )
}

export default Transactions
