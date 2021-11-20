package info.kgeorgiy.ja.holyavin.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

abstract class PersonInfo implements Person, Serializable {
    final String name, surname;
    final int passport;
    final ConcurrentMap<String, RemoteAccount> accounts;

    String computeBankAccountId(final String subId) {
        return passport + ":" + subId;
    }

    PersonInfo(final String name, final String surname, final int passport,
               final ConcurrentMap<String, RemoteAccount> accounts) {
        this.name = name;
        this.surname = surname;
        this.passport = passport;
        this.accounts = accounts;
    }

    PersonInfo(final String name, final String surname, final int passport) {
        this(name, surname, passport, new ConcurrentHashMap<>());
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getSurname() {
        return surname;
    }

    @Override
    public int getPassport() {
        return passport;
    }

    @Override
    public Account getAccount(final String subId) {
        return accounts.get(subId);
    }

    protected interface ChildFactory<T extends PersonInfo> {
        T create(final String name, final String surname, final int passport,
                 final ConcurrentMap<String, RemoteAccount> accounts);
    }

    public <T extends PersonInfo> T clone(ChildFactory<T> factory) {
        return factory.create(
                name, surname, passport, accounts.entrySet().stream()
                        .collect(Collectors.toMap(
                                Map.Entry::getKey, e -> e.getValue().clone(), (x, y) -> y, ConcurrentHashMap::new
                        ))
        );
    }
}
