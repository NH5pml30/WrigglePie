package info.kgeorgiy.ja.holyavin.bank;

import java.rmi.*;

public interface Account extends Remote {
    /** Returns account identifier. */
    String getId() throws RemoteException;

    /** Returns amount of money at the account. */
    int getAmount() throws RemoteException;

    /** Sets amount of money at the account. */
    void setAmount(int amount) throws RemoteException;

    /** Adds some amount of money to the account */
    int addAmount(int amount) throws RemoteException;
}