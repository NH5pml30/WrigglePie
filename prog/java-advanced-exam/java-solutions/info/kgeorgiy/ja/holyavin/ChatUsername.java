package info.kgeorgiy.ja.holyavin;

import java.io.Serializable;

public class ChatUsername implements Serializable {
    public final String userName;

    public ChatUsername(final String userName) {
        this.userName = userName;
    }
}
