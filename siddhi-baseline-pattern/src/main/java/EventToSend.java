public class EventToSend {

	private long timestamp;
	private Object[] event;

	public EventToSend(long timestamp, Object[] event) {
		this.timestamp = timestamp;
		this.event = event;
	}

	public long getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(long timestamp) {
		this.timestamp = timestamp;
	}

	public Object[] getEvent() {
		return event;
	}

	public void setEvent(Object[] event) {
		this.event = event;
	}

}
