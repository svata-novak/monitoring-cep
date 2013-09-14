import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public class Main {

	private static final String LOG_PATH = "C:\\Users\\Svata\\AppData\\Local\\Temp\\jel.log";
	private static final int PORT = 4747;

	/**
	 * Sends lines of text from file to socket (after a client connects)
	 */
	public static void main(String[] args) {
		ServerSocket serverSocket = null;
		Socket eventSocket = null;
		PrintWriter out = null;

		try {
			FileInputStream fstream = new FileInputStream(LOG_PATH);
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			String strLine;

			serverSocket = new ServerSocket(PORT);
			System.out.println("Waiting for client connection on port " + PORT + "...");
			eventSocket = serverSocket.accept();
			System.out.println("Client connected");
			out = new PrintWriter(eventSocket.getOutputStream(), true);

			System.out.println("Sending events...");
			while ((strLine = br.readLine()) != null) {
				strLine = strLine.trim();
				if (strLine.isEmpty())
					continue;

				out.write(strLine);
				out.println();
				
				// System.out.println(strLine);

			}
			in.close();
		} catch (Exception e) {// Catch exception if any
			System.err.println("Error: " + e.getMessage());
			System.exit(-1);
		} finally {
			if (out != null)
				out.close();
			if (serverSocket != null) {
				try {
					serverSocket.close();
				} catch (IOException e) {
				}
			}
		}

		System.out.println("Events sent.");
	}

}
