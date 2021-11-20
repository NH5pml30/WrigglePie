package info.kgeorgiy.ja.holyavin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {
    /**
     * Creates a new account with specified identifier if it does not already exist and does not contain ':'.
     * @param id account id
     * @return created or existing account, or {@code null} if {@code id} contains ':'.
     */
    Account createAccount(String id) throws RemoteException;

    /**
     * Returns account by identifier.
     * @param id account id
     * @return account with specified identifier or {@code null} if such account does not exist.
     */
    Account getAccount(String id) throws RemoteException;

    /**
     * Returns a local person by passport or null, if no such entry exists.
     * @param passport passport
     * @return local person entry.
     */
    LocalPerson getLocalPerson(int passport) throws RemoteException;

    /**
     * Returns a remote person by passport or null, if no such entry exists.
     * @param passport passport
     * @return remote person entry.
     */
    Person getPerson(int passport) throws RemoteException;

    /**
     * Creates a person entry by personal information. If a matching entry exists, it is returned instead.
     * If an entry with the same passport but different personal info exists, null is returned.
     * @param passport passport
     * @param name name
     * @param surname surname
     * @return remote person entry, or null if the passport is associated with another person.
     */
    Person createPerson(String name, String surname, int passport) throws RemoteException;
}
