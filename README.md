# chat-server
Simple chat server for tcp clients

## Descrioption:
Write erlang-server listening tcp port 1234 which could connect clients like telnet/net cat, for the tests will be enough to 2-10. The server listens to his customers what they write and send written messages to all connected clients right now. Messages are separated by \n.

Further useful improvements:
	* mark messages the timestamp and the source, such as [hh:mm] (ip:port): <message> \n
	* allow the user to write a message setnick <name> to set a nickname which will appear in place of (ip:port)
	* the ability to specify in the config file the maximum length of the message. If the message is more than n characters, then everything that is not climbed replaced by [cut. message too long]
	* after reconnection client get n messages, that was written while he wasn't in chat.
	* if the message has word shit, then a 10% chance the user will be kicked from the chat, and its message will not be sent to other participants.