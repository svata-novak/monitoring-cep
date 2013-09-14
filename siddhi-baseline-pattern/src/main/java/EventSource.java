import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

public class EventSource {

	private enum EventSourceType {
		FILE, SOCKET
	}

	private EventSourceType type;
	private String fileName;

	private FileInputStream fstream;
	private DataInputStream in;
	private InputStreamReader isr;
	private BufferedReader br;
	private Socket clientSocket;

	private String host;
	private int port;

	private void openFile() throws FileNotFoundException {
		this.fstream = new FileInputStream(fileName);
		this.in = new DataInputStream(fstream);
		this.br = new BufferedReader(new InputStreamReader(in));
	}

	public EventSource(String fileName) {
		this.type = EventSourceType.FILE;

		this.fileName = fileName;
	}

	private void connect() throws IOException {
		this.clientSocket = new Socket(host, port);
		this.isr = new InputStreamReader(clientSocket.getInputStream());
		this.br = new BufferedReader(isr);
	}

	public EventSource(String host, int port) {
		this.type = EventSourceType.SOCKET;

		this.host = host;
		this.port = port;
	}

	public void open() throws IOException {
		switch (this.type) {
		case FILE:
			openFile();
			break;
		case SOCKET:
			connect();
			break;
		}
	}

	public String readLine() throws IOException {
		return br.readLine();
	}

	public void close() throws IOException {
		switch (type) {
		case FILE:
			this.br.close();
			this.in.close();
			this.fstream.close();
		case SOCKET:
			this.br.close();
			this.isr.close();
			this.clientSocket.close();
		}
	}

	public String getInfo() {
		StringBuilder sb = new StringBuilder("EventSource: type=" + type + ", ");
		switch (type) {
		case FILE:
			sb.append("file=" + this.fileName);
			break;
		case SOCKET:
			sb.append("host=" + this.host + ", " + "port=" + this.port);
			break;
		}
		
		return sb.toString();
	}
	
	

}
