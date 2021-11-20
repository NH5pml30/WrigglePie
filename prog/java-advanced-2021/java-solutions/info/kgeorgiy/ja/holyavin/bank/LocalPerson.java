package info.kgeorgiy.ja.holyavin.bank;

import java.io.*;
import java.util.concurrent.ConcurrentMap;

public class LocalPerson extends PersonInfo implements Person, Serializable {
    LocalPerson(final String name, final String surname, final int passport,
                final ConcurrentMap<String, RemoteAccount> accounts) {
        super(name, surname, passport, accounts);
    }

    @Override
    public Account createAccount(final String subId) {
        return accounts.computeIfAbsent(subId, id -> new RemoteAccount(computeBankAccountId(id)));
    }
}
