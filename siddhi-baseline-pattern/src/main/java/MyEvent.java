import java.util.Date;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("Event")
@JsonPropertyOrder({ "occurrenceTime", "type", "_" })
public class MyEvent {

	private long id;

	// private Date occurrenceTime;
	private Long occurrenceTime;

	private Date detectionTime;

	// private String hostname;
	private String host;

	private String type;

	private String application;

	private String process;

	private int processId;

	private int level;

	private int priority;

	@JsonProperty("_")
	private Payload payload;

	public MyEvent() {
		this.payload = new Payload();
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	/*-public Date getOccurrenceTime() {
		return occurrenceTime;
	}

	public void setOccurrenceTime(Date occurrenceTime) {
		this.occurrenceTime = occurrenceTime;
	}*/

	public Long getOccurrenceTime() {
		return occurrenceTime;
	}

	public void setOccurrenceTime(Long occurrenceTime) {
		this.occurrenceTime = occurrenceTime;
	}

	public Date getDetectionTime() {
		return detectionTime;
	}

	public void setDetectionTime(Date detectionTime) {
		this.detectionTime = detectionTime;
	}

	/*-public String getHostname() {
		return hostname;
	}

	public void setHostname(String hostname) {
		this.hostname = hostname;
	}*/

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getApplication() {
		return application;
	}

	public void setApplication(String application) {
		this.application = application;
	}

	public String getProcess() {
		return process;
	}

	public void setProcess(String process) {
		this.process = process;
	}

	public int getProcessId() {
		return processId;
	}

	public void setProcessId(int processId) {
		this.processId = processId;
	}

	public int getLevel() {
		return level;
	}

	public void setLevel(int level) {
		this.level = level;
	}

	public int getPriority() {
		return priority;
	}

	public void setPriority(int priority) {
		this.priority = priority;
	}

	public Payload getPayload() {
		return payload;
	}

	public void setPayload(Payload payload) {
		this.payload = payload;
	}

	/*-@Override
	public String toString() {
		return "[Event@" + this.hashCode() + "] {" + "id=" + id
				+ ", occurrenceTime=" + occurrenceTime + ", detectionTime="
				+ detectionTime + ", hostname='" + hostname + '\'' + ", type='"
				+ type + '\'' + ", application='" + application + '\''
				+ ", process='" + process + '\'' + ", processId=" + processId
				+ ", level=" + level + ", priority=" + priority + ", payload="
				+ payload + '}';
	}*/

	@Override
	public String toString() {
		return "Event [id=" + id + ", occurrenceTime=" + occurrenceTime
				+ ", detectionTime=" + detectionTime + ", host=" + host
				+ ", type=" + type + ", application=" + application
				+ ", process=" + process + ", processId=" + processId
				+ ", level=" + level + ", priority=" + priority + ", payload="
				+ payload + "]";
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;

		MyEvent event = (MyEvent) o;

		if (id != event.id)
			return false;

		return true;
	}

	@Override
	public int hashCode() {
		return (int) (id ^ (id >>> 32));
	}
}
