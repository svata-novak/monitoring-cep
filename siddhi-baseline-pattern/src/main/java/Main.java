import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.wso2.siddhi.core.SiddhiManager;
import org.wso2.siddhi.core.event.Event;
import org.wso2.siddhi.core.stream.input.InputHandler;
import org.wso2.siddhi.core.stream.output.StreamCallback;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

public class Main {

	/**
	 * Log file used if not supplied as a command-line parameter
	 */
	private static final String LOG_PATH = "C:\\Users\\Svata\\AppData\\Local\\Temp\\jel.log";
	private static int distributedAttackCount = 0;

	private static EventToSend convertEvent(MyEvent event) {
		String type = event.getType();
		Payload payload = event.getPayload();
		// "value1":"5484077c-bbc1-4b", "value2":85
		if (type.startsWith("cz.muni.fi.ngmon.Namespace"))
			/*-return new Object[] { payload.getValue("value1"),
					payload.getValue("value2") };*/
			return null;
		// "success":true, "sourceHost":"151.60.43.89", "sourcePort":10005,
		// "user":"xnovak36"
		else if (type.startsWith("org.ssh.Daemon#Login"))
			return new EventToSend(event.getOccurrenceTime(), new Object[] {
					event.getHost(), payload.getValue("success"),
					payload.getValue("sourceHost"),
					payload.getValue("sourcePort"), payload.getValue("user") });
		else
			throw new IllegalArgumentException("Unknown event type " + type);
	}

	public static void main(String[] args) throws InterruptedException {
		// log file can be supplied as a command-line parameter
		String logFileUsed = args.length > 0 ? args[0] : LOG_PATH;

		System.out.println("Loading log...");
		long millisOnStart = System.currentTimeMillis();

		ObjectMapper mapper = new ObjectMapper();

		mapper.configure(SerializationFeature.WRAP_ROOT_VALUE, true);
		mapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
		mapper.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);

		ArrayList<EventToSend> events = new ArrayList<EventToSend>();

		try {
			FileInputStream fstream = new FileInputStream(logFileUsed);
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			String strLine;

			while ((strLine = br.readLine()) != null) {
				strLine = strLine.trim();
				if (strLine.isEmpty())
					continue;

				// System.out.println(strLine);
				MyEvent event = mapper.readValue(strLine, MyEvent.class);
				EventToSend convertedEvent = convertEvent(event);
				// add only login events
				if (convertedEvent != null)
					events.add(convertedEvent);
			}
			in.close();
		} catch (Exception e) {// Catch exception if any
			System.err.println("Error: " + e.getMessage());
			System.exit(-1);
		}

		System.out.println("Log loaded");
		System.out.println("Total number of login events: " + events.size());
		long millisAfterLoad = System.currentTimeMillis();

		// CREATE STREAMS AND PROCESS THE BASELINE QUERY

		SiddhiManager siddhiManager = new SiddhiManager();
		/*-InputHandler testEventStreamHandler = siddhiManager
				.defineStream("define stream testEventStream (value1 string, value2 int)");*/
		InputHandler loginStreamHandler = siddhiManager
				.defineStream("define stream loginEventStream (host string, success bool, sourceHost string, sourcePort int, user string)");

		siddhiManager
				.addQuery("from loginEventStream[success == false]#window.time(60 sec) insert into RepeatedLoginStream host, user, success, count(*) as attempts group by host, user having attempts > 1000");
		siddhiManager
				.addQuery("from RepeatedLoginStream#window.time(2 min) insert into DistributedDictAttack count(*) as hostsNumber group by host having hostsNumber > 10");

		/*-siddhiManager.addCallback("RepeatedLoginStream", new StreamCallback() {
			@Override
			public void receive(Event[] events) {
				System.out.println("Repeated login detected");
				EventPrinter.print(events);
			}
		});*/

		siddhiManager.addCallback("DistributedDictAttack",
				new StreamCallback() {
					@Override
					public void receive(Event[] events) {
						/*-System.out
								.println("Distributed dictionary attack detected");
						EventPrinter.print(events);*/

						distributedAttackCount++;
					}
				});

		// SENDING EVENTS

		System.out.println("Sending events...");
		long millisBeforeSend = System.currentTimeMillis();
		// send log entries
		for (EventToSend event : events) {
			// if (event.length == 4)
			loginStreamHandler.send(event.getTimestamp(), event.getEvent());
		}

		long millisAfterSend = System.currentTimeMillis();

		System.out
				.println("Number of distributed attack pattern callback calls: "
						+ distributedAttackCount);
		System.out.println("Ending...");

		/*-System.out.println(millisOnStart);
		System.out.println(millisAfterLoad);
		System.out.println(millisBeforeSend);
		System.out.println(millisAfterSend);*/

		System.out.println("Parsing and loading took "
				+ (millisAfterLoad - millisOnStart) + "ms.");
		System.out
				.println("Creating event streams and processing queries took "
						+ (millisBeforeSend - millisAfterLoad) + "ms.");
		System.out.println("Sending events and detecting patterns took "
				+ (millisAfterSend - millisBeforeSend) + "ms.");
		System.out
				.println("Total time (including stream creating and queries parsing and processing):\n"
						+ (millisAfterSend - millisAfterLoad)
						+ "ms, including log parsing: "
						+ (millisAfterSend - millisOnStart) + "ms.");
		
		siddhiManager.shutdown();
	}
}
