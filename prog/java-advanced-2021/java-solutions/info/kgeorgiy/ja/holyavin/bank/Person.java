package info.kgeorgiy.ja.holyavin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Person extends Remote {
    /** Gets the person's name. */
    String getName() throws RemoteException;

    /** Gets the person's surname. */
    String getSurname() throws RemoteException;

    /** Gets the person's passport number. */
    int getPassport() throws RemoteException;

    /** Gets the person's account by its identifier. */
    Account getAccount(String subId) throws RemoteException;

    /** Creates and returns the person's account by its identifier.
     *  If such account already exists, it is returned instead. */
    Account createAccount(String subId) throws RemoteException;
}
