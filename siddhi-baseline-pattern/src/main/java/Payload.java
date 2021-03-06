import java.util.LinkedHashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;

public class Payload {

	private String schema;
	private String schemaVersion;

	private Map<String, Object> properties = new LinkedHashMap<String, Object>(1);

	@JsonAnySetter
	public void add(String key, Object value) {
		properties.put(key, value);
	}

	@JsonAnyGetter
	public Map<String, Object> properties() {
		return properties;
	}

	public String getSchemaVersion() {
		return schemaVersion;
	}

	public void setSchemaVersion(String schemaVersion) {
		this.schemaVersion = schemaVersion;
	}

	public String getSchema() {
		return schema;
	}

	public void setSchema(String schema) {
		this.schema = schema;
	}

	public Object getValue(String name) {
		return properties.get(name);
	}

	@Override
	public String toString() {
		return "Payload{" +
			  "schema='" + schema + '\'' +
			  ", schemaVersion='" + schemaVersion + '\'' +
			  ", properties=" + properties +
			  '}';
	}
}
