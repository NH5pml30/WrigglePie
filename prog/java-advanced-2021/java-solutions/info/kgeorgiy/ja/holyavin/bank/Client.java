package info.kgeorgiy.ja.holyavin.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

public class Client implements MoneyGenerator {
    @Override
    public int giveMoney(String name, String surname, int passport, String subId, int amount)
            throws MoneyGeneratorException {
        try {
            final Bank bank;
            try {
                bank = (Bank) Naming.lookup("//localhost/bank");
            } catch (final NotBoundException e) {
                throw new MoneyGeneratorException("Bank is not bound", e);
            } catch (final MalformedURLException e) {
                throw new MoneyGeneratorException("Bank URL is invalid", e);
            }

            Person person = bank.createPerson(name, surname, passport);
            if (person == null) {
                person = bank.getPerson(passport);
                throw new MoneyGeneratorException(
                        "Name mismatch: expected '" + person.getName() + " " + person.getSurname() + "', " +
                                "got '" + name + " " + surname + "'"
                );
            }

            Account account = person.createAccount(subId);
            int oldAmount = account.addAmount(amount);
            return oldAmount + amount;
        } catch (RemoteException e) {
            throw new MoneyGeneratorException("Remote error", e);
        }
    }

    public static void main(final String... args) {
        if (args.length < 5) {
            System.out.println("Usage: java <client class> <name> <surname> <passport> <account> <sum delta>");
            return;
        }

        final String name, surname, subId;
        final int passport, sumDelta;
        try {
            name = args[0];
            surname = args[1];
            passport = Integer.parseInt(args[2]);
            subId = args[3];
            sumDelta = Integer.parseInt(args[4]);
        } catch (NumberFormatException exc) {
            System.err.println("Cannot read number: " + exc.toString());
            return;
        }
        try {
            System.out.println("New amount: " + new Client().giveMoney(name, surname, passport, subId, sumDelta));
        } catch (MoneyGeneratorException e) {
            System.out.println("Cannot give money: " + e.toString());
            e.printStackTrace();
        }
    }
}
