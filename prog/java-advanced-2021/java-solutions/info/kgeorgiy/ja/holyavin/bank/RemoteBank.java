package info.kgeorgiy.ja.holyavin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;

public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, RemoteAccount> accounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<Integer, RemotePerson> persons = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    private static class WithFlag<T> {
        T val;
        boolean flag;
    }

    private <T, RT extends Remote> WithFlag<RT> exportIfAbsent(ConcurrentMap<T, RT> map, T key,
                                                               Function<? super T, ? extends RT> mapping)
            throws RemoteException {
        WithFlag<RT> result = new WithFlag<>();
        RemoteException[] exc = new RemoteException[1];
        result.val = map.computeIfAbsent(key, lKey -> {
            RT object;
            try {
                object = mapping.apply(lKey);
                UnicastRemoteObject.exportObject(object, port);
                result.flag = true;
            } catch (RemoteException e) {
                exc[0] = e;
                return null;
            }
            return object;
        });
        if (exc[0] != null) {
            throw exc[0];
        }
        return result;
    }

    RemoteAccount createColonAccount(final String id) throws RemoteException {
        return exportIfAbsent(accounts, id, RemoteAccount::new).val;
    }

    @Override
    public Account createAccount(final String id) throws RemoteException {
        System.out.println("Creating account " + id);
        if (id.contains(":")) {
            return null;
        }
        return createColonAccount(id);
    }

    @Override
    public Account getAccount(final String id) throws RemoteException {
        System.out.println("Retrieving account " + id);
        return accounts.get(id);
    }

    @Override
    public LocalPerson getLocalPerson(final int passport) {
        System.out.println("Retrieving local person " + passport + " entry");
        RemotePerson person = persons.get(passport);
        return person == null ? null : person.makeLocal();
    }

    @Override
    public Person getPerson(final int passport) {
        System.out.println("Retrieving remote person " + passport + " entry");
        return persons.get(passport);
    }

    @Override
    public Person createPerson(final String name, final String surname, final int passport) throws RemoteException {
        System.out.println("Creating person " + passport + " entry");
        WithFlag<RemotePerson> res =
                exportIfAbsent(persons, passport, pass -> new RemotePerson(name, surname, pass, this));
        if (!res.flag && (!res.val.getName().equals(name) || !res.val.getSurname().equals(surname))) {
            System.out.println("Name/surname mismatch in person " + passport + " entry");
            return null;
        }
        return res.val;
    }
}
