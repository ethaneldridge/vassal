package Dev;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StreamTokenizer;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.SecureRandomSpi;

/**
 * This class is a natural extension of java.security.SecureRandom
 * which can be applied in two ways:
 * After constructing an instance, it works like java.security.SecureRandom,
 * but after invoking reset(true, <cacheSize>) it receives its
 * raw random bytes from www.random.org.
 * 
 * The crucial nextBytes() method goes back to nextBytes()
 * of java.security.SecureRandom, if any IOException occurs
 * while receiving data from www.random.org.
 * Hence this class can be used without any risk.
 * 
 * Of course, the class could have been derived from 
 * java.util.Random as well. Then, the method next() must be
 * implemented instead of nextBytes().
 * 
 * Written: 05.11.2003
 * 
 * @author Olaf Blömer, Osnabrück, Germany (olaf.bloemer@web.de)
 * 
 */
public class Random extends SecureRandom {
	
	private int		cacheSize	= 0;
	private byte[]	cache		= null;
	private int		iterator	= 0;
	private boolean	trueRandom	= false;
		
	/**
	 * Constructor inherited from super class.
	 */
	public Random() {
		super();
	}

	/**
	 * Constructor inherited from super class.
	 */
	public Random(byte[] seed) {
		super(seed);
	}

	/**
	 * Constructor inherited from super class.
	 */
	protected Random(SecureRandomSpi secureRandomSpi, Provider provider) {
		super(secureRandomSpi, provider);
	}

	/**
	 * The method to switch from pseudo random numbers
	 * to true random numbers and vice versa.
	 */
	public synchronized void reset(boolean trueRandom, int cacheSize) {
		if(trueRandom && (cacheSize < 1 || cacheSize > 16384))
			throw new IllegalArgumentException(
				"cache size must be between 1 and 16384");
			
		this.cacheSize		= cacheSize;
		this.cache			= trueRandom ? new byte[cacheSize] : null;
		this.iterator		= trueRandom ? cacheSize : 0;
		this.trueRandom		= trueRandom;
	}

	/**
	 * This method overrides nextBytes() from java.security.SecureRandom
	 * and provides random bytes from www.random.org if desired
	 * and a connect is possible.
	 * Otherwise the method from super class is invoked.
	 * @see java.security.SecureRandom#nextBytes(byte[])
	 */	
	public synchronized void nextBytes(byte[] bytes) {
		if(trueRandom)
			try {
				for(int i=0; i<bytes.length; ++i) {
					if(iterator == cacheSize) {
						connectRandomOrgAndFillCache();
						iterator = 0;
					}
					bytes[i] = cache[iterator++];
				}
			} catch (IOException e) {
				System.err.println(
					"Exception while receiving true random bytes: "+e
					+". Use super class java.security.SecureRandom.");
				reset(false, 0);
				super.nextBytes(bytes);
			}
		else
			super.nextBytes(bytes);
	}
			
	/**
	 * Connects to www.random.org and fills the cache.
	 */
	private void connectRandomOrgAndFillCache() throws IOException {		

		System.out.println("Connecting www.random.org ...");
		URL randomOrg = new URL("http://www.random.org/cgi-bin/randbyte?" +
								"nbytes="+cacheSize+"&format=dec");
		HttpURLConnection con = (HttpURLConnection) randomOrg.openConnection();

		// should be java-default		
		con.setDoInput(true);
		con.setDoOutput(false);
		con.setRequestMethod("GET");
		
		// put your email address here!
		con.setRequestProperty("User-Agent", "your-email@your-domain.com");

		StreamTokenizer st = new StreamTokenizer(
								new InputStreamReader(con.getInputStream()));
		
		int	i=0;
		while(st.nextToken() != StreamTokenizer.TT_EOF) {
			if(st.ttype != StreamTokenizer.TT_NUMBER)
				throw new IOException("received data contains non-numbers");
			cache[i++] = (byte) st.nval;
		}
		
		con.disconnect();
	}
	
	/**
	 * Returns the flag indicating the usage of true random numbers.
	 */
	public boolean isTrueRandom() {
		return trueRandom;
	}
		
	/**
	 * Returns the cache size.
	 */
	public int getCacheSize() {
		return cacheSize;
	}
	
	/**
	 * A simple demo.
	 */
	public static void main(String[] args) {
		Random rand = new Random();

		System.out.println("100 pseudo random numbers beween 0 and 9:");		
		System.out.println();
		for(int i=0; i<100; ++i) {
			System.out.print(rand.nextInt(10)+" ");
			if(i%20 == 19)
				System.out.println();
		}

		System.out.println();
		System.out.println("100 true random numbers beween 0 and 9:");		
		System.out.println();
		
		rand.reset(true, 256);
		
		for(int i=0; i<100; ++i) {
			System.out.print(rand.nextInt(10)+" ");
			if(i%20 == 19)
				System.out.println();
		}
	}
}
