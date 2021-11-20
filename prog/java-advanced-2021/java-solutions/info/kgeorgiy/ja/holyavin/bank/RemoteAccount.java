package info.kgeorgiy.ja.holyavin.bank;

import java.io.Serializable;
import java.rmi.RemoteException;

public class RemoteAccount implements Account, Serializable, Cloneable {
    private final String id;
    private int amount;

    public RemoteAccount(final String id) {
        this.id = id;
        amount = 0;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public synchronized int getAmount() {
        System.out.println("Getting amount of money for account " + id);
        return amount;
    }

    @Override
    public synchronized void setAmount(final int amount) {
        System.out.println("Setting amount of money for account " + id);
        this.amount = amount;
    }

    @Override
    public synchronized int addAmount(final int amount) {
        System.out.println("Adding some money to account " + id);
        int old = this.amount;
        this.amount += amount;
        return old;
    }

    @Override
    public RemoteAccount clone() {
        try {
            return (RemoteAccount)super.clone();
        } catch (CloneNotSupportedException e) {
            // impossible.
            return null;
        }
    }
}
