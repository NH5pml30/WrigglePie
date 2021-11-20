package info.kgeorgiy.ja.holyavin.walk;

// Does not extend `WalkerProgram` because of non-default constructor
public class RecursiveWalk {
    public static void main(String[] args) {
        WalkerProgram.executeProgram(args, Integer.MAX_VALUE);
    }
}
