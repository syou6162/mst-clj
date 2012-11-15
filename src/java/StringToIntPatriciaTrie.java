import org.ardverk.collection.PatriciaTrie;
import org.ardverk.collection.StringKeyAnalyzer;

public class StringToIntPatriciaTrie {
  public static PatriciaTrie<String, Integer> factory() {
    return new PatriciaTrie<String, Integer>(StringKeyAnalyzer.CHAR);
  }
}
