package info.kgeorgiy.ja.holyavin;

import java.io.Serializable;

public class ChatReceivedMessage implements Serializable {
    public final String userName;
    public final String message;

    public ChatReceivedMessage(final String userName, final String message) {
        this.userName = userName;
        this.message = message;
    }
}
