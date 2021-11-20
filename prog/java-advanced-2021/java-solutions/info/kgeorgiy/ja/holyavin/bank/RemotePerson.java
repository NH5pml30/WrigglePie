package info.kgeorgiy.ja.holyavin.bank;

import java.io.UncheckedIOException;
import java.rmi.RemoteException;
import java.util.concurrent.ConcurrentMap;

public class RemotePerson extends PersonInfo implements Person {
    private final RemoteBank bank;

    RemotePerson(final String name, final String surname, final int passport, RemoteBank bank) {
        super(name, surname, passport);
        this.bank = bank;
    }

    @Override
    public Account createAccount(String subId) throws RemoteException {
        try {
            return accounts.computeIfAbsent(subId, id -> {
                try {
                    return bank.createColonAccount(computeBankAccountId(id));
                } catch (RemoteException e) {
                    throw new UncheckedIOException(e);
                }
            });
        } catch (UncheckedIOException e) {
            throw (RemoteException)e.getCause();
        }
    }

    LocalPerson makeLocal() {
        return clone(LocalPerson::new);
    }
}
