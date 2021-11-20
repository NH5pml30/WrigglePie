package info.kgeorgiy.ja.holyavin.bank;

import java.rmi.RemoteException;

public interface MoneyGenerator {
    /**
     * Gives money to a person.
     * @param name name
     * @param surname surname
     * @param passport passport number
     * @param subId personal account identifier
     * @param amount amount to add
     * @return new account's amount.
     * @throws MoneyGeneratorException if error occurred.
     */
    int giveMoney(String name, String surname, int passport, String subId, int amount) throws MoneyGeneratorException;
}
