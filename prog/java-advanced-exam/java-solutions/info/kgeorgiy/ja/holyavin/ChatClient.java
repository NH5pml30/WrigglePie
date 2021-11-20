package info.kgeorgiy.ja.holyavin;

import java.util.function.Consumer;

// protocol:
//  - send username (ChatUsername)
//  - send messages (ChatSentMessage) / receive messages (ChatReceivedMessage)
public interface ChatClient extends AutoCloseable {
    void connect(String host, int port, String userName, Consumer<ChatReceivedMessage> action);
    void send(String message);
}
