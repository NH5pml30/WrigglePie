package info.kgeorgiy.ja.holyavin;

import java.io.Serializable;

public class ChatSentMessage implements Serializable {
    public final String message;

    public ChatSentMessage(final String message) {this.message = message;}
}
